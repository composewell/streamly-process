----------------------
title: Streaming System Processes
----------------------

Use streamly to execute system commands and connect the input, output and
error streams to create processing pipelines.

## Scope of the Project

A system process created by executing a command or binary executable can be
modeled as a stream transformation whose standard input is a stream and
standard output is another stream.  The overall idea is to provide
functionality close to how shell executes processes and connects them via
pipes. The interface has to be as simple as possible.

_Exception Handling:_ The standard error is yet another stream that needs to be
handled by an exception handling mechanism. We can also implements equivalents
of `pipefail` and `!` to apply a default exception handling policy on a
pipeline. Something like `set -e` would be default in a streamly streams
composition, however, we can have an option to achieve the opposite to ignore
exceptions on a portion of a pipeline.

_Signal handling:_ Implement the equivalent of the `trap` shell builtin.

_Redirections:_ Implement input and output redirections like the shell.  We can
use operators for example, `|>`, `<|` and `|>>` etc. to represent redirections.
Standard error can also be merged with standard output, something like the
`2>&1`, `|&` idioms of shell. Redirections using here documents (`|<<`) and
here string (`|<<<`) can also be supported. Duplicating and moving file
descriptors can also be supported.

_Background Jobs:_ Functionality equivalent to the `&` operator and `wait`
command in shell. This should be trivially equivalent to a `Sink` in streamly.
Investigate if there is anything extra required to implement the bash
coprocesses like functionality.

_Network Pipes:_ In addition to stdin and stdout, processes can also
communicate via sockets. We can implement ways to connect input/output streams
to sockets owned by the processes as well. `socat` and `netcat` utilities are
examples of such functionality.

## References

* See the [`bash` man page](https://linux.die.net/man/1/bash)
* https://hackage.haskell.org/package/process
* https://hackage.haskell.org/package/typed-process
* http://hackage.haskell.org/package/turtle

## Prerequisites

Working knowledge of the Haskell programming language is needed, advanced
knowledge is not required. If you have a good knowledge of basics and can work
with Haskell lists that should be enough. Familiarity with process management
related POSIX systems calls, signal handling and Haskell C FFI may be useful.

## Difficulty Level

Intermediate.

## Mentors

* Harendra Kumar
* Pranay Sashank
