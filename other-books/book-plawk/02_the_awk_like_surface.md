<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 2: The awk-like surface, and where it diverges

An awk programmer opening a `.plawk` file should recognise nearly everything in it. This chapter is the customs inspection: what crosses the border unchanged, what does not cross at all, and how to tell which is which. It does not re-teach awk; for that, see the [AWK Target book](../../book-awk-target/README.md).

## What is shared

The skeleton of a program is awk's. The parser produces one term, `program(Begin, Rules, End)` (`parser/plawk_parser.pl:124`), and the rules inside it are `rule(Pattern, Actions)`, with `rule(always, Actions)` when the pattern is omitted (`:290-308`). The variables an awk programmer reaches for are present: `$0` and `$N`, `NR`, `NF`, `FS`, `OFS`, and the statements `print` and `printf`. Comments run from `#` to end of line, and statements are separated by `;` or a newline (`:1446-1462`).

**Patterns.** A rule may be guarded by a bare regex `/re/`, a field match `$N ~ /re/` or `$N !~ /re/`, a string comparison `$N == "text"`, a numeric comparison `$N <op> K`, and any of these combined with `&&`, `||`, `!`, and parentheses (`:336-585`). A bare regex made only of literal text is recognised and lowered to a prefix or substring test; anything with metacharacters goes to POSIX extended regular expressions through `regcomp`. That choice is an optimisation, not a visible difference.

**Actions.** `if`, `else if`, `else`; `next` and `break`; `x++`, `x += K`, `x = expr`; `arr[k]++`; and, in `END` only, `for (k in arr)` (`:841-906`, `:722-742`).

**Builtins.** `length`, `substr`, `index`, `tolower`, `toupper`, and `int` (`:937-1122`).

The names are awk's. The forms behind them, as the rest of this chapter shows, are narrower.

## The cross-compatible subset

The test for "cross-compatible" is operational: the program is accepted by plawk and by gawk, and the two produce the same output. Chapter 1's counter-and-report program is the model. Two more, over the same log:

```awk
BEGIN { OFS = "," }
$1 == "ERROR" { print NR, $2, $3 }
```

```awk
{ counts[$2]++ }
END { for (k in counts) print k, counts[k] }
```

The first sets the output separator and prints the line number and two fields of each `ERROR` line; over Chapter 1's five-line log it prints `2,disk,full`, `4,net,down`, and `5,disk,again`, identical under plawk and gawk. The second is awk's canonical frequency report, and plawk accepts it because it is exactly the one loop shape the parser allows (`:728-742`). Its output order is a hash-table walk in both implementations and is unspecified in both, so a comparison must go through `sort`. Both programs here were run under plawk and gawk and their output compared.

The shape of the safe region is this:

| Construct | Valid in awk | Valid in plawk |
|---|---|---|
| `pattern { action }` rules, omitted pattern | yes | yes |
| `BEGIN { FS = ":" }`, `BEGIN { OFS = "," }` | yes | yes (single-byte values) |
| `$N == "s"`, `$N ~ /re/`, `$N > 3`, `&&` `||` `!` | yes | yes |
| `scalar++`, `scalar += k`, `scalar = expr` | yes | yes |
| `arr[k]++` and `arr[k]` as a read | yes | yes |
| `for (k in arr)` inside `END` | yes | yes |
| `if` / `else if` / `else`, `next` | yes | yes |
| `length`, `substr`, `index`, `tolower`, `toupper`, `int` | yes | yes |

A program built only from the right-hand column, and that avoids the semantic differences below, will run under both. The examples marked *cross-compatible* in this book were checked that way; where a claim in this chapter has not been, the text says so.

## Where plawk diverges from POSIX awk

The surface is a subset, and the parser is a fixed grammar rather than an awk grammar with a few features switched off. Each item here is a program the parser does not accept (recon Q2).

**No general loops.** There is no `while`, no `do ... while`, and no C-style `for (i = 0; i < n; i++)`. The only `for` is `for (k in arr)`, and it is parsed only as the action of `END` (`:722-742`). Inside a rule you iterate by being called once per record, which is what a rule is, but you cannot loop within a record.

**No string rewriting.** `sub`, `gsub`, and `split` are not parsed. Of the builtins in the shared list, none modifies a string.

**Arrays are counters.** The only array write is `arr[k]++`. There is no `arr[k] = v`, no `delete arr[k]`, and no multi-dimensional `arr[i, j]`. Reading `arr[k]` is allowed, so an array is a table of counts: enough for frequency reports, not a general map.

**One `BEGIN`, one `END`, in order.** The grammar is a fixed sequence: an optional `BEGIN`, then any function definitions, then the rules, then an optional `END` (`:124-133`). Multiple `BEGIN` or `END` blocks are not parsed. Worse for an awk habit, the `END` block holds exactly *one* action: either one `print`, or one `for (k in arr)` loop (`:709-742`). An `END` that prints a header, loops, and prints a total is three actions and will not parse; the header belongs in `BEGIN`.

**`BEGIN` configures; it does not compute.** It accepts only assignments of a quoted string to `BINFMT`, `OUTFMT`, `DYNLOAD`, `DYNCACHE`, `FS`, or `OFS`, and `print` (`:654-707`). You cannot initialise an ordinary variable or run a loop there. The project README describes `FS` and `OFS` as explicit single-byte values, so a multi-character separator is not the awk behaviour you remember.

**Patterns are a closed list.** The numeric comparison is a field against a signed integer *literal* (`:561-585`), and string equality is a field against a quoted string (`:547-559`). A comparison of two fields, such as `$1 > $2`, is not among the parsed pattern forms; neither is an arbitrary expression used as a pattern. Treat any pattern outside the list above as unsupported until a parse says otherwise.

**Functions are one expression.** `function name(a, b) { return <arith-expr> }` is accepted, and nothing else: no locals, no loops, no conditionals, no multi-statement body, and an identifier that is not a parameter fails the parse (`:136-177`). They are sugar. At parse time each becomes a Prolog clause, `Head :- Result is Expr`, which is why Chapter 8 treats them with the foreign-Prolog bridge.

**Semantics differ even when the syntax is shared.** Chapter 1 gave two: integer `/` truncates, and an uninitialised scalar is `0`, not the empty string. A third concerns `printf`. The project README lists the natively supported conversions as `%%`, `%s`, and `%d`/`%i`/`%ld` for integers, plus `%f`/`%g`/`%e` with optional precision for double-typed expressions, and notes that `printf` adds neither `OFS` nor a newline, as in awk. Anything beyond that list (`%c`, `%x`, width and flag combinations) is not claimed here; the README does not list it.

## Reading a rejection

There are two ways a program can fail to run, and the CLI distinguishes them by exit status (`bin/plawk:87-141`).

A **parse error** means the text does not match the grammar above: a `while`, a second `END` action, a `sub`. The CLI reports `parse error` and exits `2`. A parse failure is blunt by nature of the grammar being a DCG; expect to locate the offending construct yourself, by bisecting the program against the list in this chapter.

A **compile rejection** is subtler. The program parses, so the AST is well formed, but the code generator has no lowering for that shape and the whole driver fails. The CLI prints `plawk: <file> parses but is outside the compilable surface` and exits `3`. These rejections are deliberate `fail` clauses in the code generator. Examples from the recon (Q5): a plain `writebin` against a tagged-union layout, or `writebin case K` against a flat one (`plawk_native_codegen.pl:3097-3104`); an `if` whose two branches both end the record (`:5736-5740`); and, in binary mode, text-shaped forms such as regexes, string equality, `substr`, `index`, `length`, `$0`, and associative arrays, which fail the driver clause outright (`:2492-2493`). (An older README paragraph also lists assigning a double-typed expression to a scalar as a codegen rejection; that text is stale — typed double scalar slots are implemented, as Chapter 3 shows and its test suite confirms.)

The practical reading is that parse-clean does not mean runnable. The surface this chapter lists is what the *parser* accepts; the set the compiler lowers is smaller and depends on the input mode, which later chapters (4 to 6) spell out.

How much the CLI tells you about a compile rejection depends on how the code generator refuses. The deliberate `fail` clauses above — the writebin mismatches, the dual-terminal `if`, the binary-mode text forms — make the driver predicate simply fail, so the CLI falls to its catch-all and prints only the generic `outside the compilable surface` line before exiting `3` (`bin/plawk:128-135`). A few compile-time problems are reported more precisely, because they `throw` rather than `fail`: a `dyncall` with no `DYNLOAD`, and a predicate the program calls through the bridge that no `@prolog` block or `function` defines, each produce a message naming the cause (`check_foreign_calls`, `bin/plawk:185-206`), as does an uncompilable or unknown Prolog predicate (`report_error`, `:208-219`). So a bare *surface* rejection tells you only that something is unsupported — bisect against this chapter's list — while a *foreign-call* problem tells you which predicate is at fault.

## Next

Chapter 3: Typed arithmetic, which takes up the first of the semantic differences in detail: `i64`, `float`, guarded division, and how a scalar becomes a double.
