# Proposed design changes

Changes that would make the package more robust but have a wide design
impact. They are not implemented.

## Prompt termination of a process whose output is abandoned

The process APIs allocate the process with `Stream.bracketIO3`. If the
output stream is not consumed to the end, the process is terminated only
when the stream is garbage collected. This happens when:

* the consumer stops early, e.g. `Stream.take 3 (toBytes "yes" [])`;
* an exception is raised in the consumer of the stream, e.g. in a fold;
* a pipe stage stops reading its input because its process exited, e.g. in
  `toBytes "yes" [] & pipeBytes "head" ["-c", "5"]` the `yes` process stays
  blocked writing to its full stdout pipe until garbage collection.

Until garbage collection, the process holds its pipes, and a process that
does not write output, e.g. `sleep`, keeps running.

Proposal: add variants of the APIs that take an `AcquireIO` from
`Streamly.Control.Exception.withAcquireIO` (streamly-core 0.3 and later) and
register the process cleanup with it. The cleanup then runs, at the latest,
when the monad level bracket exits, independent of garbage collection. This
needs CPP, or a new lower bound on streamly-core.

## Escalate SIGTERM to SIGKILL

`cleanupException` sends SIGTERM and closes the pipes. A process that ignores
SIGTERM keeps running, and the thread forked to reap it waits until the
process exits. Proposal: send SIGKILL if the process has not exited after a
timeout, configurable in `Config`.

## `daemon` should connect stdio to /dev/null

`daemon` uses `NoStream` for stdin, stdout and stderr, which closes
descriptors 0, 1 and 2 in the child. The next files opened by the daemon get
these descriptors, and a library in the daemon that writes to stdout or
stderr then writes into those files. The usual daemon setup is to open
`/dev/null` on these descriptors. Changing this changes the behavior of an
existing API, so it needs a decision. The current behavior is documented.

## Stdio modifiers in the internal module

`inheritStdin`, `inheritStdout`, `pipeStdin`, `pipeStdout` and `pipeStdErr`
are exported from `Streamly.Internal.System.Process`. Some APIs apply the
user's modifier after their own stdio settings, so these modifiers can
create configurations that the API does not handle. For example,
`toChunksWith pipeStdin "cat" []` creates a stdin pipe that is never written
or closed, and `cat` waits for input forever.

Proposal: each API should apply the stdio settings it depends on after the
user's modifier, and accept only the stdio choices it supports, e.g. by a
separate argument instead of a `Config` modifier.

## The use-native build flag

With `-f use-native`, the library does not compile, the first error is:

    src/Streamly/Internal/System/Process.hs:1244:15: error: [GHC-22385]
        Not in scope: record field ‘std_in’

The build failed in the same way before the changes in this branch.

`standalone`, `foreground` and `daemon` use `withCreateProcess`,
`createProcess`, `waitForProcess`, `NoStream` and `Inherit` from the process
package, and `Streamly.Internal.System.Command` imports `ProcessHandle` from
the process package, which is not a dependency with this flag. A native
implementation of the standalone processes, and a package-independent
process handle type, are needed.
