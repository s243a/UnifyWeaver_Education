<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 10: Compiling to native: WAM to LLVM

> *Skeleton. Source: `bin/plawk`, `codegen/plawk_native_codegen.pl` (about 6.9k lines), `src/unifyweaver/targets/wam_llvm_target.pl`; see also the sibling LLVM target book (`../book-llvm-target/`).*

## The CLI

- `plawk build FILE.plawk -o OUT [--keep-ll]`, `plawk run FILE.plawk INPUT...`; produced binary reads `argv[1]`, `-` or no argument is stdin; exit codes 2 parse error, 3 compile error, 4 clang failure; `clang -O2` (`bin/plawk`, README). Requires `swipl` and `clang`.
- Observed: a build takes a few seconds on the author's checkout; a `WAM fallback` diagnostic is printed per Prolog predicate.

## The compilation path

- Source text, then `plawk_parse_source/3` (AST plus Prolog clauses), then `plawk_prolog_block_preds/2`, then `write_wam_llvm_project/3` (yields `wam_vm(InstrCount, LabelCount)`), then `plawk_program_native_driver_ir/4`, then a final LLVM module (recon Q3).

## Direct emission vs delegation

- plawk emits pattern guards, conversions, `printf` globals, scalar phi slots, assoc tables, foreign shims itself; it delegates stream framing to `llvm_emit_stream_driver_ir/3`, `llvm_emit_binary_stream_driver_ir/4`, `llvm_emit_varlen_stream_driver_ir/5` and reuses the target's guard and printf emitters (`plawk_native_codegen.pl:67-87, 3940-3980`).

## Reading the generated IR

- `--keep-ll`; a tour of one small program: tag switch, typed loads at fixed offsets, scalar phi nodes, the associative table walk (`wam_assoc_i64_iter_next`).
- Regexes: compile-time constants, `regcomp` once per site, cached.

## Where the interpreter fits

- Back-reference to Chapter 7: the compiled path does not go through `plawk_core.pl`.
