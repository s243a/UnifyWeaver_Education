<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 11: Demonstrated vs design

> *Skeleton. This chapter is the book's ledger; it should be the most carefully fact-checked.*

## What each subsystem has behind it

- Core interpreter: 16 unit tests in `core/plawk_core_tests.pl`.
- Parser and native codegen: no tests in that file; separate `tests/test_plawk_*.pl` suite (about 40 files, several hundred test clauses by rough count) that parse, inspect generated IR, and build and run binaries when `clang` is present. <!-- TODO: get an authoritative pass/fail run of the whole suite and the exact test count (recon follow-up); not run for this draft -->
- The initial recon read only the core test file and so reported the parser and codegen as untested; the wider suite corrects that. State this plainly in the book.

## The decoupling, stated precisely

- Interpreter does not run parser output; parser feeds codegen; plawk's own CLI never calls the interpreter (recon Q5).

## Ledger table

- Rows: feature, status (run / tested-in-suite / documented-only), evidence. Cover text surface, binary records, `lpsN`, `repK`, unions, `writebin`/`writebin case`, `@prolog`, bridged calls, `blob`, `dyncall`, `float` slots.

## Benchmarks in context

- The README's four workloads (W1-W4, N = 2,000,000, mawk baseline, best of 3) and the byte-identical-output gate on text jobs; single machine, environment-relative; the 13.8x binary figure is a comparison against text, not like-for-like. <!-- TODO: re-run `bench/bench.sh` and record the machine, or cite as the project's claim only (recon follow-up) -->

## Known gaps and roadmap

- Missing awk surface (Chapter 2 list); typed double slots; general control flow in functions.
- Design documents: `PLAWK_PHILOSOPHY`, `PLAWK_SPECIFICATION`, `PLAWK_IMPLEMENTATION_PLAN`, `PLAWK_EXECUTION_ARCHITECTURE`, `PLAWK_DCG_BINARY_READERS` (grammar-to-native-reader lowering; the `lpsN` reader is described as its first slice).
- Phase labelling: the project README's "Planned structure" lists `parser/` and `codegen/` as Phase 2 while describing them as built; reconcile with the implementation plan. <!-- TODO: confirm current phase numbering with the implementation plan (recon follow-up) -->
