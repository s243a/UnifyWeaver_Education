<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 7: The Prolog core: Reader, Handler, Writer

> *Skeleton. Source: `examples/plawk/core/plawk_core.pl` and `plawk_core_tests.pl` (16 tests), TUTORIAL "The three moving parts". This is the one subsystem with a unit-test suite in its own directory.*

## The driver: `process_all/4`

- `process_all(Reader, Handler, State0, StateN)`, `meta_predicate process_all(3, 4, +, -)` (`plawk_core.pl:21, 38-47`). Loop: call Reader, stop on `end_of_file`, call Handler, continue on `Continue == yes`.

## Reader, Handler, State, and the writer

- Reader `call(Reader, Item, S0, S1)`; concrete `text_file_reader/5` producing `record(text, Line, Fields)` (`plawk_core.pl:75-85`).
- Handler `call(Handler, Item, S0, S1, Continue)`: the compiled equivalent of a pattern-action block.
- State `state(InputStreams, OutputStreams, Counter, UserFields)`; there is no separate Writer argument, output is appended to state by `append_output/3`, `print_fields/3`, `print_item/3` (`plawk_core.pl:102-142`).

## Awk concepts as Prolog terms

- The mapping table: `$0` is `item_field(0, ...)`, `$N` is `item_field(N, ...)`, `NR` is `nr/2`, `NF` is `nf/2`, `FS`/`OFS` via `plawk_options/2`, `print` is `print_fields/3`.
- Worked example: the `print_error_fields` demo and its awk equivalent.

## What the 16 tests establish

- Stop on EOF; early break; 1-based vs `$0` indexing; `NF`/`NR`; default and custom `FS`/`OFS`; output order; the end-to-end error-line collector.

## What the core is, and is not

- A reference model of the Reader/Handler/Writer contract. It does not run parser output: the parser's AST feeds the native code generator, not `process_all/4` (recon Q5). Why the design keeps it anyway (determinism evidence for the compile step, per the implementation plan's Phase 0).
- <!-- TODO: confirm how the compiled-stream smoke (`tests/test_plawk_compiled_stream_core.pl`) relates this core to the native path, i.e. whether the core's own predicates are what get compiled (recon follow-up) -->
