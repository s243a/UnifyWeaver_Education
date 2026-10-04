<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Book: plawk, a Prolog awk

A guide to plawk (the `examples/plawk` app in UnifyWeaver): an awk-like surface language whose programs run over **typed binary records**, can carry **embedded Prolog**, and compile through UnifyWeaver's WAM-to-LLVM target to a native binary.

## Status: 🚧 Prototype-phase book about a prototype-phase tool

plawk is a prototype. Chapter 1 is written; Chapters 2-11 are skeletons (headings and the source facts each section will draw on). Every chapter separates what has been run from what is design, and Chapter 11 collects that line in one place. Claims were checked against UnifyWeaver revision `0af53d6a3`; they will go through fact-verification and external review before this book is called done.

## The arc

The book follows what makes plawk *different* from awk, and sends you to the sibling awk book for everything awk already does. Chapters 1-3 set the scene: *what plawk is, the awk-shaped surface and exactly where it stops being awk, and its typed arithmetic.* Chapters 4-6 are the novel interior: *binary records, tagged unions with `case` routing, and writing binary back out so stages chain with no text between them.* Chapters 7-9 are the Prolog story: *the Reader/Handler/Writer core, embedded `@prolog` blocks, and runtime-loaded grammars.* Chapters 10-11 close the loop: *compiling to a native binary, and an honest ledger of what is demonstrated versus what is design.*

## Contents

1. [Introduction](01_introduction.md) - What plawk is, the one-paragraph thesis for how it differs from awk, the running examples, and what runs today versus what is design - *written*
2. [The awk-like surface, and where it diverges](02_the_awk_like_surface.md) - The pattern-action shape that is shared with awk, the cross-compatible subset, and the constructs plawk does not parse - *skeleton*
3. [Typed arithmetic: `i64` and `float`](03_typed_arithmetic.md) - Native integer and double expressions, guarded division, scalar slot typing - *skeleton*
4. [Binary records](04_binary_records.md) - `BINFMT`, fixed and length-prefixed fields, bounded repetition and `foreach` - *skeleton*
5. [Tagged unions and `case` routing](05_tagged_unions_and_case.md) - One stream, several record kinds; arms, `case K { }` blocks and the `TAG == K` shorthand - *skeleton*
6. [Writing binary: `OUTFMT`, `writebin`, `writebin_arm`](06_writing_binary.md) - Typed writers, tagged output, and plawk-to-plawk pipelines - *skeleton*
7. [The Prolog core: Reader, Handler, Writer](07_the_prolog_core.md) - `process_all/4`, state threading, and the interpreter that defines the model - *skeleton*
8. [Foreign Prolog: `@prolog`, functions, bridged calls](08_foreign_prolog.md) - Prolog clauses compiled into the same binary and called from rules - *skeleton*
9. [Runtime-loaded grammars: `DYNLOAD` and `dyncall`](09_runtime_loaded_grammars.md) - Swapping behaviour without rebuilding the binary - *skeleton*
10. [Compiling to native: WAM to LLVM](10_compiling_to_native.md) - The CLI, the compilation path, and what the generated code looks like - *skeleton*
11. [Demonstrated vs design](11_demonstrated_vs_design.md) - The honest ledger, benchmarks in context, and the roadmap - *skeleton*

- Appendix A: Cross-compatibility cheat sheet (the plawk/awk/gawk subset, with the known output differences) - *planned*

## Prerequisites

- Working knowledge of awk. This book does not re-teach it; the sibling [AWK Target book](../../book-awk-target/README.md) covers awk and UnifyWeaver's *opposite* direction (emitting awk from Prolog). Any awk or gawk tutorial will do for the basics.
- SWI-Prolog (`swipl`) and `clang`, to build and run plawk programs.
- A checkout of UnifyWeaver (the book refers to `examples/plawk/`).
- Basic familiarity with Prolog facts and clauses (Chapters 7-9 lean on it; earlier chapters do not).
- Helpful: a feel for C-style struct layout and byte offsets (Chapter 4 builds it from the ground up).

## Quick Start

```bash
# from the UnifyWeaver repository root
examples/plawk/bin/plawk run program.plawk input.txt
```

Chapter 1 gives a first program and the output to expect.

## See Also

- `examples/plawk/README.md` and `examples/plawk/TUTORIAL.md` (in UnifyWeaver) - the project's own reference and tutorial
- `docs/design/PLAWK_*.md` (in UnifyWeaver) - the design documents: philosophy, specification, implementation plan, execution architecture, DCG binary readers
- [The AWK Target book](../../book-awk-target/README.md) - awk itself, and the Prolog-to-awk direction
