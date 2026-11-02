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

## Backslash-newline in `shellWord`

Status: verified, not fixed.

In a POSIX shell, a backslash followed by a newline is a line continuation:
the backslash and the newline are both removed, outside quotes and inside
double quotes. Inside single quotes both characters are kept.

`shellWord` (in `Streamly.Internal.System.Command`) keeps the newline in
unquoted text and keeps both characters in double-quoted text. Results with
`streamly-core-0.3.0`, comparing `sh -c 'printf "[%s]" ...'` with
`shellWord` (`\` and newline written as in the command text):

| Input             | `sh`             | `shellWord`              |
|-------------------|------------------|--------------------------|
| `foo \<nl>bar`    | `foo`, `bar`     | `foo`, `"\nbar"`         |
| `foo ab\<nl>cd`   | `foo`, `abcd`    | `foo`, `"ab\ncd"`        |
| `foo "ab\<nl>cd"` | `foo`, `abcd`    | `foo`, `"ab\\\ncd"`      |
| `foo 'ab\<nl>cd'` | `foo`, `ab\<nl>cd` | `foo`, `"ab\\\ncd"`    |

In the input column `<nl>` is a line break in the command text. In the
`shellWord` column strings are shown in Haskell notation, so `"\n"` is that
same line break character and `\\` is one backslash. A backslash followed by
the letter `n`, as in `echo "x\ny"`, is a different case: `sh` and
`shellWord` both keep it as the two characters `\n`.

Only the single-quoted case matches.

### Upstream: `wordWithQuotes` in streamly-core

`shellWord` is `Parser.wordWithQuotes` with `\` as the escape character.
`wordWithQuotes` cannot express the shell behaviour:

- Outside quotes, an escape always removes the escape character and keeps
  the next character literally. There is no way to remove both characters.
- Inside quotes, the translation function has type
  `Char -> Char -> Maybe Char`, so an escape sequence can be translated to
  one character or left as it is, but not deleted.

No other parser in `streamly-core` (master, checked 2026-11-03) can delete
an escape sequence either: `wordFramedBy`, `blockWithQuotes`, `takeEndByEsc`
and `takeFramedByEsc_` all keep the escaped element.

Preprocessing the command string to remove backslash-newline is not
correct, because it must not be removed inside single quotes, so the
preprocessor would have to parse quotes as well.

Possible change in `wordWithQuotes`: let the escape translation return
`Maybe String` (or a type that can express deletion), and apply a
translation to unquoted escapes too. `shellWord` would then map
backslash-newline to the empty string outside quotes and inside double
quotes.
