# Upstream defects

Defects in dependencies found while testing this package, with the
workaround used here, if any.

## streamly-core: `wordWithQuotes` on an all-whitespace input

Version: streamly-core 0.3.0, also present in the streamly master branch
checked on 2026-11-03.

`Parser.wordWithQuotes` returns one empty word for an input consisting only
of separators. In the `WordQuotedSkipPre` state, `extract` returns the result
of the fold with no input:

    extract (WordQuotedSkipPre s) = fmap (FDone 0) $ ffinal s

Therefore `Stream.parseMany shellWord (Stream.fromList "   ")` produces
`[Right ""]`, while `"a   "` produces `[Right "a"]`, with no trailing empty
word.

Workaround: `parseCommand` in `Streamly.Internal.System.Command` checks for
an all-whitespace command string before parsing and raises the "empty
command" exception. `shellWord` itself is unchanged, its documentation states
the behavior.

Suggested fix: return an error, or a "no word" result, from `extract` in the
`WordQuotedSkipPre` state when the parser has consumed only separators.

## streamly-core: `wordWithQuotes` cannot remove an escape sequence

In a POSIX shell, a backslash followed by a newline is removed (line
continuation), both outside quotes and inside double quotes. The escape
translation function of `wordWithQuotes` has the type
`a -> a -> Maybe a`: it can translate an escaped character to one character
or keep both characters, but it cannot translate it to nothing.

No workaround. The `shellWord` documentation lists this as a difference from
a POSIX shell.

Suggested fix: allow the translation function to drop the escaped character,
e.g. by returning a list or a three-way result.

## GHC runtime or process: a pipe leaks into every child process

Version: GHC 9.10.3, process 1.6.26.1, on Linux.

A program using only `System.Process.callProcess` has a pipe open on file
descriptors 4 and 5 without `O_CLOEXEC`. Every child process inherits both
ends of this pipe:

    /proc/<parent>/fd/4 pipe:[2318949] flags: 00
    /proc/<parent>/fd/5 pipe:[2318949] flags: 01

The other runtime descriptors (timerfd, eventpoll, eventfd and the IO manager
pipes) have `O_CLOEXEC` set. The source of this pipe was not identified.

No workaround. `closeFiles True` closes these descriptors in the child.
