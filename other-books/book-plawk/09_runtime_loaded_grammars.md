<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 9: Runtime-loaded grammars: `DYNLOAD` and `dyncall`

Chapter 8 ended with a limit: the Prolog you bridge to is compiled into the
binary. Change a clause and you rebuild. This chapter covers the one place
where plawk lets behaviour change *without* a rebuild, and it is also the
place where this book has to be most careful about what is shown and what is
only intended. The mechanism is real and has passing tests; the larger story
around it is a plan.

## A word on terminology

The plawk README describes this feature as "JIT". This book does not, because
the project's own execution-architecture document
(`docs/design/PLAWK_EXECUTION_ARCHITECTURE.md`) says the opposite in its first
paragraph: LLVM is used purely as an ahead-of-time backend, and "there is no
JIT anywhere in the system today". The accurate description is two tiers in
one AOT-compiled binary: native record loops, plus Prolog running as WAM
bytecode on a natively compiled interpreter. A true JIT tier is listed there
as a Phase 5 plan (runtime-loaded DCG rules compiled to native code).

What `dyncall` does is smaller and easier to state. A `.wamo` file holds WAM
bytecode that was already compiled ahead of time. The plawk binary loads that
bytecode at run time and feeds it to the interpreter it already contains.
Nothing is compiled to native code while the program runs. So this book says
**runtime-loaded**: the *loading* is late, the *compilation* is not. (The test
files still carry "Phase 5 (JIT)" in their headers, which is the plan's name
for the direction, not a description of what they exercise.)

## The `.wamo` object

A `.wamo` is a WAM object written by `write_wam_object/3` from a list of
predicates, with a declared entry:

```prolog
square(X, R) :- R is X * X.
write_wam_object([user:square/2], [wamo_entry(square/2)], 'square.wamo').
```

This is the form the tests use. The calling convention is fixed by arity: a
grammar predicate has arity N+1, the N inputs arrive as `A0..A_{N-1}`, and the
output is the last argument. The result is read back as an i64, and a failed
load or failed call yields `0`, the same silent-zero rule as the compiled
bridge in Chapter 8.

## `DYNLOAD` and `dyncall(...)`

`DYNLOAD` is a `BEGIN` assignment, one of the six names the `BEGIN` parser
accepts (`BINFMT`, `OUTFMT`, `DYNLOAD`, `DYNCACHE`, `FS`, `OFS`). The call is
an expression:

```awk
BEGIN { BINFMT = "i64" ; DYNLOAD = "square.wamo" }
{ total += dyncall($1) }
END { print total }
```

`dyncall(args...)` parses to its own node, `dyncall(Args)`
(`plawk_parser.pl`, around line 1289), and deliberately never touches the
compiled-foreign-call machinery. It is a reserved word: the parser commits
only after the full `dyncall(` token, so an identifier such as `dyncalls(...)`
still falls through to an ordinary `prolog_call`. For each distinct argument
count the code generator emits one shim, `@plawk_dyncall_N`, that boxes the
arguments and calls the object-call primitive. The object loads lazily on the
first `dyncall` and the handle is reused for the rest of the run; the
primitive rewinds the arena per call, so memory stays constant, as in
Chapter 8.

Swapping behaviour means overwriting the file and re-running the *same*
binary. The test does exactly this: it builds the program above against a
`square` object, runs it over four i64 records (3, 4, 5, 10) and expects
`150`; then it overwrites the object with a `twice` grammar and re-runs the
unchanged binary, expecting `44`.

## `dyncall_at(Source, args...)`

`dyncall_at` moves the file name from `BEGIN` into the data. `Source`, a field
or a string literal, names the `.wamo` at run time, chosen per call. It is
parsed before `dyncall` so the longer keyword wins. In the tests the input is a
text file with one grammar path per line:

```awk
BEGIN { DYNCACHE = "on" }
{ total += dyncall_at($1) }
END { print total }
```

Here `dyncall_at($1)` passes no inputs beyond the source, so each grammar is a
one-argument predicate (output only). With lines naming `seven.wamo`,
`nine.wamo`, `seven.wamo`, `seven.wamo`, the program prints `30`
(7+9+7+7).

## Cache policy: `DYNCACHE`

`DYNCACHE` governs how `dyncall_at` manages loaded objects (`dyncall` always
keeps its single object). Three values, from `plawk_native_codegen.pl`:

- `"on"` (default): each distinct grammar loads once, keyed by an interned
  path id, and is reused. The cache holds 64 grammars; past that, grammars
  still load and run correctly but are not cached.
- `"mtime"`: as `on`, but the entry also keys on the file's modification time,
  so recompiling a `.wamo` evicts the stale object and reloads it. The test
  runs a program once (`7`), sleeps past the timestamp resolution, rewrites the
  object, runs the same binary again and gets `11`.
- `"off"`: load fresh and free after every call. Always current, no cache, full
  load cost each time. The test checks only that it gives the same answer
  (`30`) as `on`.

The trade is the usual one: `on` is fast but stale if the file changes under a
long-lived process; `mtime` pays a stat per call to stay current; `off` pays a
load per call.

## The guard rail

`dyncall` with no `DYNLOAD` in `BEGIN` is rejected when the driver is built.
The code generator throws `plawk_dyncall_without_dynload` with the message
`dyncall(...) requires BEGIN { DYNLOAD = "file.wamo" }`
(`plawk_native_codegen.pl:644-646`), and the CLI reports it as a build failure
(exit 3) rather than emitting a program that would return `0` on every call.
`dyncall_at` has no such requirement, since its source is supplied per call.

## What is demonstrated, and what is not

Demonstrated. The two test files, `tests/test_plawk_dyncall.pl` and
`tests/test_plawk_dyncall_at.pl`, hold ten tests: parse shapes, arity
collection, the missing-`DYNLOAD` failure, the `150` then `44` swap,
`dyncall_at` selection by field (`30`), `off` mode (`30`), and `mtime`
redefinition (`7` then `11`). I ran both files against the source tree used for
this book (`swipl` and `clang` present) and all ten passed. Every number quoted
above comes from those assertions, not from a program of my own.

Not demonstrated by me. I did not write and run a `dyncall` program of my own
against a `.wamo` I built separately; the programs shown in this chapter are
the tests' programs, restated. <!-- TODO(run-verify): build a .wamo with write_wam_object/3 outside the test harness and run a standalone dyncall program end to end before presenting it as an independent example -->

Design only, or unmeasured:

- Any speed claim. The README gives no figures for `dyncall` that this book
  re-measured, and bytecode interpretation is slower than the native loop
  around it by construction.
- Native compilation of the loaded grammar. That is the Phase 5 JIT plan, and
  it does not exist today.
- `dyncall_at` with multiple input arguments beyond the arity-collection test;
  the round-trip tests only use the zero-input form. <!-- TODO(run-verify): run dyncall_at($1, $2) with a two-input grammar -->
- Safety of loading untrusted `.wamo` files. Nothing in the source or tests
  addresses it; treat a `.wamo` as code you chose to run.
