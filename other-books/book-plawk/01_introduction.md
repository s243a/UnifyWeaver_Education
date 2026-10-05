<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 1: Introduction

**The cast.** Four names recur. **awk** is the classic pattern-action text tool: a program is a list of `pattern { action }` rules run against each input line. **Prolog** is a logic language whose programs are facts and rules; **SWI-Prolog** (`swipl`) is the implementation used here. **WAM**, the Warren Abstract Machine, is the instruction set Prolog is usually compiled to, and **LLVM** is a compiler toolkit that turns a portable intermediate form into native machine code. **plawk**, the subject of this book, is an awk-shaped language that is compiled through Prolog, WAM, and LLVM into a native program. If awk and Prolog are both familiar, skip ahead.

## Two awks in one repository

UnifyWeaver already has an awk story, told in the [AWK Target book](../../book-awk-target/README.md): you write Prolog predicates and UnifyWeaver *emits an awk script*. Prolog is the source, awk is the output. plawk runs the other way. You write something that looks like awk, and it is lowered to Prolog and compiled to a native binary. The name is "Prolog awk", and it also keeps the two projects from being confused. They are complementary: one targets awk, the other imitates it.

This book is about the second. It assumes you can already read an awk program, and it spends its pages on what awk *cannot* do.

## Why plawk differs from awk

The thesis in one paragraph. awk keeps its familiar surface — patterns, actions, `$1`, `NR`, `BEGIN`/`END` — but replaces its interior. Where awk is an interpreter whose every value is, at heart, a string that gets re-split and re-parsed on each use, plawk is a *compiler*, and it can treat a record as **typed binary data**: a declared layout (`BINFMT = "i64 f64"`) makes `$1` a single load of a known type at a known byte offset, with no field splitting and no number parsing. Records can be **tagged unions** — one stream carrying several record shapes — routed to rules with a `case K { ... }` block that awk has no equivalent for. Output can be written back out as binary (`writebin`), so stages chain with no text between them. And because the compiler is a Prolog compiler, a plawk program can **carry Prolog with it** — `@prolog ... @end` blocks compiled into the same binary and called from rules — so a stage can use a real parser or a logic predicate where awk would reach for a regular expression.

Each clause of that paragraph is a chapter: binary records (4), tagged unions (5), `writebin` (6), the Prolog core (7), embedded Prolog (8), native compilation (10).

## Who this is for

You write awk, or read it, and have hit its ceiling: the numbers are really binary and you are paying to render and re-parse them; the stream mixes record kinds and you are dispatching on a prefix by hand; or the interesting logic is a small grammar that a regex cannot express. You are also comfortable enough with a compiler toolchain to run `swipl` and `clang`. You do *not* need to know Prolog to get through Chapters 1-6, and you do not need to know LLVM at all.

If you want a drop-in replacement for gawk, plawk is not that. It parses a subset of awk, deliberately and visibly. Chapter 2 lists what is missing.

## The running examples

Two small inputs carry the book.

**A log.** Text lines, the kind awk was made for:

```text
INFO boot ok
ERROR disk full
WARN cpu hot
ERROR net down
ERROR disk again
```

**A telemetry feed.** The same idea, but binary and heterogeneous: *metric* records (an id and a reading) interleaved with *event* records (a name and a code). On the wire, each record starts with an 8-byte tag saying which shape follows. This is the input that awk handles badly and plawk was built for; it appears from Chapter 4 onward.

We start with the log, because it lets us show where plawk and awk *agree*. This program is valid awk and valid plawk:

```awk
{ total++; counts[$1]++ }
$1 == "ERROR" { errors++ }
END { print "total", total, "errors", errors, "ERROR-lines", counts["ERROR"] }
```

Built and run with the plawk CLI (`examples/plawk/bin/plawk run prog.plawk log.txt`), and run under gawk, both print:

```text
total 5 errors 3 ERROR-lines 3
```

(When a program carries embedded Prolog, plawk's build step can also write `WAM fallback` notes to standard error — one per predicate it lowers that way; they are build diagnostics, not part of a program's output.)

That is the cross-compatible core: a few rules, scalar counters, one associative array, a final report. Chapter 2 maps how far it extends. The book's examples marked *cross-compatible* were checked by running them under both plawk and gawk and comparing output.

## Where they part ways, in miniature

Two differences show up before any binary record appears, and they are worth seeing now because they are the first things that will surprise an awk programmer.

First, arithmetic is typed. In awk, `{ s += $2 } END { print "avg", s / NR }` over the lines `1 7` and `2 8` prints `avg 7.5`. In plawk the same program prints `avg 7`: the operands are native `i64`, so `/` is integer division. You ask for floating point explicitly — `float($2)`, or a float literal such as `2.0` — and then the expression is computed as a double (Chapter 3).

Second, an uninitialised scalar is `0`, not the empty string. `$1 == "NOPE" { c++ } END { print c }` prints an empty line under gawk and `0` under plawk, because plawk's scalar slots are integers from the start.

Neither is a bug. They are consequences of the same decision that makes the rest of the language possible: values have types, and the compiler knows them.

Now the part with no awk analogue. Given the telemetry feed, this program declares a tagged union and routes by record kind:

```awk
BEGIN { BINFMT = "case(i64 f64 | lps16 i64)" }
case 0 { $1 > 100 { msum += float($2) } }
case 1 { $1 == "boom" { events++ } }
END { print msum, events }
```

Tag 0 is a metric (`i64 f64`), tag 1 is an event (`lps16 i64`, a length-prefixed string of up to 16 bytes, then an integer). Inside `case 0`, `$1` is an integer; inside `case 1`, it is a string. On a six-record feed — three metrics, then three events of which two are `boom` — it prints `2.75 2` (the two metrics with an id over 100 contribute 2.5 and 0.25; `boom` occurs twice). Chapter 5 explains every token.

## What runs today versus what is design

This book will not blur the two, so here is the ledger once, up front. Chapter 11 expands it.

**Runs today (exercised).**
- The pure-Prolog interpreter core, `examples/plawk/core/plawk_core.pl`: `process_all/4`, field accessors, `NR`/`NF`/`FS`/`OFS`, a text-file reader. It has a 16-test suite.
- The surface parser (`plawk_parser.pl`) and the native code generator (`plawk_native_codegen.pl`), driven by the `plawk` CLI. Beyond the core's 16 tests, the repository carries a separate set of 44 `tests/test_plawk_*.pl` files that parse programs, generate LLVM IR, and (where `clang` is available) build and run native binaries. The programs in this book marked *run* were built and executed that way and their output compared against the expected result.

**Design, or partly so.**
- The three subsystems are *decoupled*. The interpreter core does not execute what the parser produces; the parser feeds the native code generator, and the interpreter is the reference model for the Reader/Handler/Writer contract. Chapter 7 draws this carefully, because it is easy to assume the interpreter is the thing that runs your `.plawk` file. It is not.
- The surface is a subset of awk. No `while` or C-style `for`, no `sub`/`gsub`/`split`, no general `arr[k] = v` or `delete`, user functions limited to a single `return` expression (Chapter 2).
- Doubles are typed only in expressions and scalar accumulators in the cases Chapter 3 lists; the project's own docs describe broader typed slots as follow-up work.
- Benchmark figures in the project README are environment-relative, measured once on one machine, and not independently reproduced here. Chapter 11 reports them as the project's claim.

plawk is a prototype. The honest summary: the *front door* (a `.plawk` file in, a native binary out) works for the supported subset and is tested; the *breadth* of awk is not there; and the reference interpreter is a model, not the engine.

## How to read this book

- **Chapter 2 — The awk-like surface, and where it diverges.** The shared shape, the cross-compatible subset, the unsupported list.
- **Chapter 3 — Typed arithmetic.** `i64`, `float`, guarded division, how a scalar becomes a double.
- **Chapter 4 — Binary records.** `BINFMT`, fixed and length-prefixed strings, repetition and `foreach`.
- **Chapter 5 — Tagged unions and `case` routing.** The telemetry feed, arms, the `TAG == K` shorthand.
- **Chapter 6 — Writing binary.** `OUTFMT`, `writebin`, `writebin case K`, plawk-to-plawk pipelines.
- **Chapter 7 — The Prolog core.** Reader, Handler, Writer, and what the interpreter is for.
- **Chapter 8 — Foreign Prolog.** `@prolog` blocks, `function`, calling Prolog from a rule.
- **Chapter 9 — Runtime-loaded grammars.** `DYNLOAD` and `dyncall`.
- **Chapter 10 — Compiling to native.** The CLI, WAM to LLVM, reading the generated IR.
- **Chapter 11 — Demonstrated vs design.** The full ledger and the roadmap.

A reader who wants the ideas can read 1, 2, 5, and 11. A reader who wants to run things can read 1, 4, 5, and 10 and keep the CLI usage from Chapter 10 open. Where the book describes behaviour it is describing the code in `examples/plawk/` of the UnifyWeaver repository, chiefly `core/plawk_core.pl`, `parser/plawk_parser.pl`, `codegen/plawk_native_codegen.pl`, and `bin/plawk`, with the project's `README.md` and `TUTORIAL.md` as the secondary reference.

## Next

Chapter 2: The awk-like surface, and where it diverges — what a plawk program shares with awk, and the exact list of what it does not parse.
