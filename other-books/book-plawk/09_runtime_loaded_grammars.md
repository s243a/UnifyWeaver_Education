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

The plawk README describes this feature as "JIT" (just-in-time compilation — producing native machine code while the program runs). This book does not, because
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

Two different things appear in that snippet, and it is worth keeping them
apart. The first line is the **source clause** that *defines* the grammar
(`square/2`, ordinary Prolog). The second is the **object-generation query**,
`write_wam_object/3`, which *compiles* that clause ahead of time into the
`square.wamo` file. The plawk binary never sees the source clause; it loads the
compiled `.wamo` at run time. The clause is the definition, the
`write_wam_object/3` call is the build step, and the `.wamo` is the artifact the
binary consumes.

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
one-argument predicate — that single argument is the *output*, and the source field `$1` only names the file and is never passed into the grammar. With lines naming `seven.wamo`,
`nine.wamo`, `seven.wamo`, `seven.wamo`, the program prints `30`
(7+9+7+7).

## Cache policy: `DYNCACHE`

`DYNCACHE` governs how `dyncall_at` manages loaded objects (`dyncall` always
keeps its single object). Three values, from `plawk_native_codegen.pl`:

- `"on"` (default): each distinct grammar loads once, keyed by an interned
  path id, and is reused. The cache holds 64 grammars; past that, grammars
  still load and run correctly but are not cached.
- `"mtime"`: as `on`, but the cache entry also keys on the file's modification
  time, so that *within one process* a grammar whose `.wamo` has been rewritten
  (new mtime) is treated as stale and reloaded on the next `dyncall_at` instead
  of being served from the cached entry. The accompanying test does **not**
  actually exercise that in-process invalidation. It runs the binary once (`7`),
  sleeps past the timestamp resolution, rewrites the object on disk, then runs
  the *unchanged binary a second time* and gets `11` — but a second process
  starts with an empty cache and would read the current file under any
  `DYNCACHE` mode (`on`, `mtime`, or `off`), so the `7`→`11` result only shows
  two independent runs each reading the file as it then stood. It does not
  establish that the mtime key invalidates a live cached entry; that remains the
  documented source behaviour, not something this test demonstrates.
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

Demonstrated, end to end. The two test files, `tests/test_plawk_dyncall.pl`
and `tests/test_plawk_dyncall_at.pl`, hold ten tests: parse shapes, arity
collection, the missing-`DYNLOAD` failure, the `150` then `44` swap,
`dyncall_at` selection by field (`30`), `off` mode (`30`), and `mtime`
redefinition (`7` then `11`). I ran both files against the source tree used for
this book (`swipl` and `clang` present) and all ten passed. This is the whole
loop, not just a parse: each run test calls `write_wam_object/3` to build a real
`.wamo`, compiles the plawk program to a native binary with `clang`, runs it
over real input, and asserts the printed total. So `dyncall` is **test-backed
end to end** — a built object, loaded at run time, driving real output — and not
merely a described design. Every number quoted above comes from those
assertions, not from a program of my own.

One thing I did not do. I did not write and run a `dyncall` program of my own,
against a `.wamo` I built by hand *outside* the test harness, as a standalone
demonstration. The end-to-end path is exercised by the tests; what remains is an
independent reproduction of it from a fresh program, which Chapter 11 keeps on
its roadmap.

Design only, or unmeasured:

- Any speed claim. The README gives no figures for `dyncall` that this book
  re-measured, and bytecode interpretation is slower than the native loop
  around it by construction.
- Native compilation of the loaded grammar. That is the Phase 5 JIT plan, and
  it does not exist today.
- `dyncall_at` with a call argument beyond the source. The source field is
  *not* counted in the call arity: in `dyncall_at($1)` the field `$1` names the
  grammar and nothing is passed to it (zero call arguments, output only), while
  `dyncall_at($1, $2)` names the grammar with `$1` and passes exactly *one*
  argument, `$2` (the parser collects arity `[1]` for it). Every round-trip
  test uses the source-only form, so a `dyncall_at($1, $2)` call that passes an
  argument to a one-input grammar is not yet run end to end.
- Safety of loading untrusted `.wamo` files. Nothing in the source or tests
  addresses it; treat a `.wamo` as code you chose to run.
