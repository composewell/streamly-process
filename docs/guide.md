# Processes

## High level API

* Use Haskell threads instead of processes
* This package is to use external executables, programs seamlessly in Haskell
  streamly programs in the same way as you would use Haskell threads.
* When needed use processes just like threads
* Compose using concurrent stream composition to run asynchronously or to run
  background tasks.

## Low level API

We should not get the handles to processes (pid) or threads (tid). We should
not be killing threads/processes or sendiing signals or exceptions to them.

Rather we should design our programs in such a way that it is not required and
reaping of threads is done automatically.

### Process or thread management

You do not need process or thread management because it is handled
automatically via the stream composition. Except for some introspection cases
e.g. changing the priorities dynamically.

Process groups or thread groups are represented by a tree of threads in one
stream composition.  We can also represent terminal sessions using a tree of
threads.

For introspection type of cases, we could also have dynamic thread groups using
combinators like iterateMap that allow feedback based stream generation.  As a
thread is created we can also register it (tid/pid) into a global state via the
iterateMap like feedback mechanism. The tid/pid stream can be fed back to the
input.

### Changing process properties dynamically

You may want to change the priorities of the tasks dynamically, for that you
may need the handle. We could possibly assign labels to processes and when
starting a process, specify the unique label for that process. That label can
be used to control the process. The label could be an IORef/Handle which would
be updated when we start the process.

Usually we can specify the priority before we start the process.

## Preferred Concurrency Model

However, you cannot just run a task in the background and move ahead and start
doing something else like in imperative programs. You have to compose the
forever running programs with other parallel threads in your program. It is
possible to run a background thread in the IO monad and move ahead, but we do
not encourage that model. In that case we are using the system to keep track of
our tasks rather than we doing it explicitly. The RTS or the OS creates an
entry in the global table of concurrent tasks. In our concurrent monad we do it
ourselves which is better because we have an explicit picture of what's going
on, and also we do not depend on the RTS/OS specific stuff, everything is
expressed in our programming model.

## Idioms

* Run a process synchronously redirecting output to /dev/null:

```
Stream.fold Fold.drain $ toChunks path args
```

## Examples

Use grep on a stream of files concurrently and merge the lines of results.

## FAQ

Q: I want to start a background thread and then move on to doing the next task.
A: This is equivalent to the parallel stream monad. You can either compose like

```
a `parallel` b
```

our

```
do :: ParallelT
    a
    b
```

Where a could be running forever.

Q: I have just a single task.
A: a `parallel` Stream.nil or Stream.fromEffect eff.

