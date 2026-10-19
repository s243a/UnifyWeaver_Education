<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Appendix A: awk cross-compatibility cheat sheet

This appendix answers two questions about a program you are about to write: *will it run under both plawk and gawk*, and *will it print the same thing?* It is a reference, not a lesson. It condenses [Chapter 2](02_the_awk_like_surface.md) and [Chapter 3](03_typed_arithmetic.md) and does not re-teach them.

The "cross-compatible" entries below were checked by running each program under plawk's CLI (UnifyWeaver revision `0d2abe3e2`) and under gawk 5.1.0 and comparing the output; newer feature-stack work not yet on `main` does not change this baseline. The same convention as [Chapter 11](11_demonstrated_vs_design.md) applies: where a claim was run, it is labelled as run; where it rests on a chapter, the chapter is named. Examples use the five-line log from [Chapter 1](01_introduction.md): `INFO boot ok`, `ERROR disk full`, `WARN cpu hot`, `ERROR net down`, `ERROR disk again`.

## 1. The cross-compatible subset

| Construct | awk | plawk |
|---|---|---|
| `pattern { action }` rules, omitted pattern | yes | yes |
| `BEGIN { FS = ":" }`, `BEGIN { OFS = "," }` | yes | yes (single-byte values) |
| `$N == "s"`, `$N ~ /re/`, `$N > 3`, `&&` `\|\|` `!` | yes | yes |
| `scalar++`, `scalar += k`, `scalar = expr` | yes | yes |
| `arr[k]++`, and `arr[k]` as a read | yes | yes |
| `for (k in arr)` as the single action of `END` | yes | yes |
| `if` / `else if` / `else`, `next` | yes | yes |
| `length`, `substr`, `index`, `tolower`, `toupper`, `int` | yes | yes |
| `printf` with `%%`, `%s`, `%d`/`%i`/`%ld`, `%f`/`%g`/`%e` | yes | yes |

Building only from this table, and steering clear of the differences in section 2, gives a program that runs under both — but section 2 lists the *common* divergences, not an exhaustive guarantee of identical output. The deeper difference is numeric conversion: plawk moves between text and numbers under the strict `i64`/double rules of [Chapter 3](03_typed_arithmetic.md), where awk coerces strings to numbers on demand, so two programs built only from the shared constructs can still diverge wherever a value crosses the text/number boundary. Four programs that were run under both, with identical output:

| Program | Output (both) |
|---|---|
| `{ total++; counts[$1]++ }` `$1 == "ERROR" { errors++ }` `END { print "total", total, "errors", errors, "ERROR-lines", counts["ERROR"] }` | `total 5 errors 3 ERROR-lines 3` |
| `BEGIN { OFS = "," }` `$1 == "ERROR" { print NR, $2, $3 }` | `2,disk,full` / `4,net,down` / `5,disk,again` |
| `{ counts[$1]++ }` `END { for (k in counts) print k, counts[k] }` | after `sort`: `ERROR 3` / `INFO 1` / `WARN 1` |
| `{ printf "%s=%d;", $1, NR }` | `INFO=1;ERROR=2;WARN=3;ERROR=4;ERROR=5;` |

The third row carries a caveat: the order of a `for (k in arr)` walk is unspecified in *both* implementations, so any comparison must go through `sort`.

The `printf` conversions for doubles agree too, as long as the value is made a double the awk way — with a float literal. `{ printf "%.2f;", $2 / 2.0 }` over `a 7` prints `3.50;` under both, and `%g` and `%e` likewise (`3.5;` and `3.500000e+00;`). Both were run under plawk and gawk here.

## 2. Same syntax, different output

Types are the cause. plawk gives integer expressions the `i64` type, and scalars begin life as `i64` zero. gawk computes in doubles, and an unset variable is both `""` and `0`. Each row below was run under both.

| Program | Input | plawk | gawk | Reason |
|---|---|---|---|---|
| `{ s += $2 } END { print "avg", s / NR }` | `1 7` / `2 8` | `avg 7` | `avg 7.5` | `s` and `NR` are `i64`; integer `/` truncates |
| `$1 == "NOPE" { c++ } END { print c }` | `a` | `0` | an empty line | the scalar is `i64` zero, not an unset string |
| `{ print $2 / 2, $2 / 2.0 }` | `a 7` | `3 3.5` | `3.5 3.5` | the first division is `i64`; the literal `2.0` makes the second a double |

Two further plawk behaviours, both run under plawk here: integer division by zero yields `0` (`{ print $2 / 0 }` over `a 7` prints `0`), whereas float division has no zero guard and follows IEEE — a nonzero numerator over zero is an infinity (`{ print float($2) / 0.0 }` over `7` prints `inf`), and `0.0 / 0.0` is NaN (the same program over `0` prints `-nan`). And doubles print with `%g`, six significant digits — `2.0 * 10` prints `20`, not `20.0`, and `0.1 + 0.2` prints `0.3` ([Chapter 3](03_typed_arithmetic.md)).

## 3. Not accepted: parse error, exit 2

plawk's grammar is a fixed subset, not awk with a few features switched off. Each construct below was run, and each produced a parse error with exit status `2`.

| Construct | Example |
|---|---|
| `while` loop | `END { while (x < 3) x++ }` |
| C-style `for` | `{ for (i=0;i<3;i++) x++ }` |
| array assignment | `a[$1] = $2` |
| `delete` | `delete a[k]` |
| two-field comparison pattern | `$1 > $2 { ... }` |
| multi-action `END` | `END { print "h"; print x }` |
| `gsub` | `gsub(...)`; `sub` and `split` fall to the same grammar |

The underlying rules are in [Chapter 2](02_the_awk_like_surface.md). In short: the only loop that parses is `for (k in arr)`, and only as the single action of `END` (inside a rule, the one bounded loop is `foreach` over a binary `rep` field, [Chapter 4](04_binary_records.md)); arrays are counters, written only by `arr[k]++`; `BEGIN` assigns quoted strings to `BINFMT`, `OUTFMT`, `DYNLOAD`, `DYNCACHE`, `FS` and `OFS`, plus `print`, and cannot compute; and a function body is one `return <arith-expr>`. The `sub`/`split` entry rests on the grammar and on Chapter 2, which lists all three as not parsed.

## 4. Parses, but will not compile: exit 3

A program can satisfy the grammar and still be refused by the code generator. The CLI then prints that the file "parses but is outside the compilable surface" and exits `3`. Most of this territory is binary-mode text operations; [Chapter 2](02_the_awk_like_surface.md) and [Chapter 4](04_binary_records.md) draw the boundary, and it is not repeated here. One nuance is worth keeping in view. In binary mode, string *equality* against a literal on a string field (`$1 == "ERR"`) and an associative array keyed by an `i64` field are supported. String-*keyed* arrays and text operations on fields (`substr`, `index`, `length`, `tolower`, `toupper`, regex) are not. Parse-clean does not mean runnable.

## 5. Rules of thumb

- **For doubles that still match gawk, use a float literal.** Where awk's default floating arithmetic is what you mean, put a float literal such as `2.0` in the expression: `$2 / 2.0` computes as a double under both, and i64 operands in the same tree are promoted. plawk's `float($N)` and `float(pred(...))` coercions also produce doubles, but they are *plawk-only* — gawk has no `float` builtin, so a program that uses them will not run under gawk ([Chapter 3](03_typed_arithmetic.md)).
- **Do not rely on an unset variable.** A scalar that may never be touched prints `0` under plawk and a blank under gawk. If the output matters, make the empty case impossible to reach, or avoid printing it.
- **Keep `END` to one action.** Put a header in `BEGIN`, or accept a single `print` or a single `for (k in arr)`.
- **Sort before comparing.** Pipe `for (k in arr)` output through `sort` on both sides.
- **Stay with the shared builtins.** `length`, `substr`, `index`, `tolower`, `toupper`, `int`. Do not reach for `sub`, `gsub`, `split`, or `delete`.
- **Count with arrays; do not store.** Use `arr[k]++` and read `arr[k]`. A general map is outside the surface.
- **Treat `printf` conservatively.** Stay with the listed conversions. `%c`, `%x`, and width or flag combinations are not claimed.
- **When in doubt, run both.** A parse error (2) or compile rejection (3) is cheap to see; a silent type difference is not, so compare outputs.

## See also

The full surface is in [Chapter 2](02_the_awk_like_surface.md); typed arithmetic in [Chapter 3](03_typed_arithmetic.md); the Run / Test-backed / Design ledger in [Chapter 11](11_demonstrated_vs_design.md). For the awk side of the comparison, see the sibling [AWK Target book](../../book-awk-target/README.md).
