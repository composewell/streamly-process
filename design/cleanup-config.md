# Configurable Cleanup of Abandoned Processes

Status: requirements only, not implemented. Based on PR #91
(`cleanup-config`, commit `035104b`), which is not merged.

## Current Behaviour

The pipe-based APIs (`pipeChunks`, `toChunks`, `toBytes` and their variants)
create the process with `pipeChunksWithAction` in
`Streamly.Internal.System.Process`, which uses `Stream.bracketIO3`. When the
consuming stream is terminated by an exception, or is garbage collected
before it finishes, `cleanupException` runs. It:

1. sends `SIGTERM` to the child process only,
2. closes the stdin, stdout and stderr pipe handles, ignoring `EPIPE` on
   stdin,
3. forks a thread that waits for the child to exit.

The same `cleanupException` is used for both the exception case and the
garbage collection case.

## Problems

- **Descendants survive.** `SIGTERM` goes only to the direct child. If the
  child forks, for example `sh -c "a | b"`, the processes `a` and `b` keep
  running after `sh` is terminated.
- **The signal cannot be chosen.** Some programs do an orderly shutdown on
  `SIGINT` (the Ctrl-C signal) and handle `SIGTERM` differently or not at
  all.
- **No escalation.** A child that ignores or blocks `SIGTERM` keeps running
  indefinitely. Nothing sends `SIGKILL` after a timeout.
- **The caller cannot wait.** The wait happens in a forked thread, so when
  the exception reaches the caller the child may still be running. A test
  that restarts a server on the same port fails if the old server still
  holds the port.

## Requirements

1. **Termination signal.** A config setting selects the signal sent on
   cleanup. Default: `SIGTERM`.
2. **Process group.** A config setting sends the signal to the child's
   process group instead of the child alone. Raise an exception if this is
   set without `newProcessGroup True`: without a new group, the child's
   process group is the parent's, and the signal would also be delivered to
   the parent program.
3. **Escalation to SIGKILL.** An optional timeout after which `SIGKILL` is
   sent to the same target (child or process group) if the child has not
   exited.
4. **Synchronous wait.** A config setting makes the exception-path cleanup
   wait for the child to exit before rethrowing. The garbage collection
   path never blocks; it keeps the forked wait. `bracketIO3` takes separate
   actions for the two paths, so they can differ.
5. **Defaults unchanged.** With no new settings, behaviour is the current
   one: `SIGTERM` to the child, forked wait. `newProcessGroup` does not
   change the cleanup behaviour by itself.
6. **Both backends.** Implemented for the `process` library backend and the
   `use-native` backend. Do not depend on `System.Process.Internals`.
7. **Windows.** The Windows build must not break. Signals do not exist on
   Windows; document what each setting does there (ignored, or mapped to
   `TerminateProcess`).
8. **Tests and docs.** Tests for each setting, including termination of a
   grandchild process and `SIGKILL` escalation for a child that ignores
   `SIGTERM`. Haddock for each setter and a CHANGELOG entry.

## Defects in PR #91

These are to be avoided in the new implementation.

- `terminateProcessGroup` does not require `newProcessGroup True`, so it can
  signal the parent's own process group.
- It does not build with `use-native`: the native `Config` is still
  `newtype Config = Config Bool`, and the new `interruptProcess` works on
  `System.Process`'s `ProcessHandle__`, not on the native `Process` type.
- It does not build on Windows: the `System.Posix.*` and
  `System.Process.Internals` imports are guarded only by `USE_NATIVE`, not
  by OS.
- `waitOnTermination True` waits with no timeout on both the exception and
  the garbage collection path, so a child that ignores the signal blocks
  the cleanup forever.
- `newProcessGroup True` implicitly turns on group signalling, changing the
  behaviour of existing programs.
- The setter signatures are inconsistent: `terminateWithSigInt` and
  `waitOnTermination` take a `Bool`, `terminateProcessGroup` takes none.

## Open Questions

- Setter names, and whether to group them in a single cleanup record or
  keep them as separate `Config -> Config` modifiers.
- Default value of the `SIGKILL` timeout: disabled, or a fixed number of
  seconds.
- Whether the same settings also apply to `standalone` and `daemon`, which
  do not use `pipeChunksWithAction`.
