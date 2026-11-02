# Native process spawning (`use-native` flag)

The `use-native` flag uses a process-spawning implementation that does not
depend on the `process` package. It crashes intermittently. This note
records an investigation (2026-10-18) of the cause and of whether the
approach can work.

## Can it work?

Processes can be spawned without the `process` package. They cannot be
spawned the way the branch does it, by running Haskell code in a
`forkProcess` child under the threaded RTS. The tests are built with
`-threaded -with-rtsopts=-N`. After `fork()`, the child keeps only the
thread that called fork. All other Haskell threads and RTS workers are
gone, but any lock they held stays locked. The `unix` docs warn about this
for `forkProcess`.

The `process` package avoids this by doing fork/exec in C
(`cbits/runProcess.c`, using `vfork` or `posix_spawn`). It never runs
Haskell code between fork and exec, and it creates its pipes close-on-exec
while holding a lock.

## Likely causes

Two small programs using only the `unix` package, `Repro.hs` and
`Leak.hs`, modelled parts of `Posix.hs`. The programs were not kept.

1. **The debug logging deadlocks the child.** `hPutStrLn stderr` in
   `childAction` and `exec` takes the `stderr` Handle's MVar. If another
   thread held that MVar at fork time, the child blocks forever.
   - With 4 threads × 200 spawns and other threads writing to `stderr`, a
     child that writes to `stderr` hung in 20 of 20 runs.
   - Without the writes, 20 of 20 runs passed.
   - The debug code therefore causes hangs of its own, which can hide the
     original crash. The `/proc/<pid>/fd` listing in the child
     (`Dir.readFiles`) has the same problem: it allocates, can trigger a
     GC, and uses Handles.
2. **Pipe fds leak into other children.** `createPipe` does not set
   `O_CLOEXEC`, and nothing serialises spawns. When two spawns overlap,
   child A inherits the pipe fds created for child B and keeps them after
   `exec`. `Leak.hs` showed a pipe created by another thread (fds 17 and
   18) still open in the exec'd `/bin/sh`. While A is alive:
   - B never sees EOF on its stdin.
   - The parent never sees EOF on B's stdout.

   This only happens when spawns overlap, so the failures are
   intermittent.
3. **Any other Haskell code in the child can deadlock the same way as
   cause 1.** Examples:
   - an exception in `action`;
   - `executeFile` failing (the `XXX` comment in `newProcess`);
   - a GC in the child.

   If the child's Haskell code returns instead of exec'ing, the child also
   runs RTS shutdown, which may flush stdio buffers copied from the parent
   and duplicate output. This was not tested.

The actual crash was not reproduced. Whether it is a segfault, a hang,
`BlockedIndefinitelyOnMVar` or a wrong exit code would indicate which of
these causes it is.

## Recommended approach

Spawn in C, or call `posix_spawn(3)` through the FFI:

- `posix_spawn_file_actions_adddup2` / `addclose` replace `childAction`.
- `POSIX_SPAWN_SETSIGMASK` and `POSIX_SPAWN_SETSIGDEF` reset the signal
  mask and the signal dispositions.
- An exec failure is returned to the parent as an errno. This replaces the
  planned exception pipe.
- Create pipes with `pipe2(O_CLOEXEC)`. `dup2` onto fds 0, 1 and 2 clears
  close-on-exec on the target fd, so the child still gets its stdio.

glibc implements `posix_spawn` with `clone(CLONE_VM|CLONE_VFORK)`, which is
safe in a multithreaded process. Wrapping the spawn in `runInBoundThread`
(`createProc''` in the debug commit) does not help, because the other
threads still exist at fork time.

## Build status at the time of the investigation

- `use-native` was rebased onto master as one commit, `Add debug logging`.
- `flake.nix` points `streamly-packages` at revision `69728978…`, the
  revision used by streamly's flake. This provides prebuilt
  streamly-0.11.0 and streamly-core-0.3.0.
- The `-f use-native` build fails:
  - The debug code does not compile against streamly-core 0.3, because the
    `Dir.readFiles` API changed.
  - Without the debug code, the native path on master does not compile
    either. `standalone` uses `withCreateProcess` and the `std_in` field,
    but the native `Config` is only a `Bool`.

Because of these build failures, the test suite was not run with
`use-native`. The findings above come from the two small programs, not
from streamly-process itself.
