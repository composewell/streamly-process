# Upstream Fixes

Changes needed in other packages, mainly `streamly-core`, to support
features in this package.

## Multiline Command Strings

Status: requirements only, not implemented. Based on issue #73 and PR #75
(`cmd_quasiquoter`), which is not merged.

### Need

A convenience quasiquoter, `cmd`, for writing long commands over multiple
lines. It is the `str` quasiquoter with post-processing of the literal text:

- each newline is replaced by a space,
- each run of unquoted whitespace is collapsed into a single space.

Interpolated `#{var}` values are not modified.

The `Command` APIs (`toBytes`, `runWith`, `pipeWith`, ...) do not need
`cmd`. They split the command string with `shellWord`, which uses `isSpace`
as the separator, so newlines and repeated spaces outside quotes already
act as a single separator, and `str` works for multiline commands. `cmd` is
needed when the string is not split by `shellWord`, for example when it is
passed to `sh -c`, logged or compared.

### Quoting

Whitespace inside single or double quotes must be preserved. PR #75 applies
the normalization to all literal text, so `[cmd|echo "a    b"|]` becomes
`echo "a b"`, and a newline inside quotes becomes a space. Both change the
arguments passed to the command. The normalization has to track quotes and
escapes the way `shellWord` does.

### Upstream: `strWith` in streamly-core

`Streamly.Internal.Unicode.String` exports only `str`. The parser
(`strParser`, `StrSegment`) is not exported, so PR #75 copies about 60 lines
of it, including the bug where a lone `#` at the end of input is dropped.

Add a variant of `str` to `streamly-core` that applies a function to the
literal text segments at compile time:

```haskell
strWith :: (String -> String) -> QuasiQuoter
str = strWith id
```

Then `cmd` in `Streamly.Internal.System.Command` is:

```haskell
cmd :: QuasiQuoter
cmd = strWith normalizeCmd
```

Because the literal text is split at each `#{var}`, a quoted region can span
segments, for example `"a  #{x}  b"`. The function must then take the quote
state from one segment to the next, or `strWith` must pass the whole
sequence of segments. One option:

```haskell
strWith :: ([StrSegment] -> [StrSegment]) -> QuasiQuoter
```

with `StrSegment` exported from an internal module.
