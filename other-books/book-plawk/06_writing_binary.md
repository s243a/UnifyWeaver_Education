<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 6: Writing binary: `OUTFMT`, `writebin`, `writebin_arm`

> *Skeleton. Source tests: `test_plawk_binary_writers.pl`, `test_plawk_union_out.pl`, `test_plawk_union_writebin.pl`, `test_plawk_varlen_writers.pl`, `test_plawk_rep_writer.pl`, `test_plawk_forin_writebin.pl`.*

## A text-to-binary converter

- `BEGIN { OUTFMT = "i64 f64" } { writebin $1, float($2) }`; AST `writebin(Fields)` (`plawk_parser.pl:1014-1017`). Works in text mode and in binary mode; `i64` promotes into `f64` slots.
- Rejections: `writebin` without `OUTFMT`, arity mismatch, oversized literals, numbers into string slots, doubles into `i64` slots.

## Normalising a union: per-arm writers

- Verified: `BINFMT = "case(i64 f64 | lps16 i64)"; OUTFMT = "i64 i64"` with `case 0 { { writebin $1, NR } } case 1 { { writebin $2, NR } }` emits 16-byte `(value, NR)` records for both arms. `OUTFMT` is program-wide; source fields type against each rule's own arm.

## Tagged output: `writebin case K`

- `OUTFMT = "case(i64 | i64 lps8)"` and `writebin case K, args`; AST `writebin_arm(Index, Fields)` (`plawk_parser.pl:998-1013`).
- Verified round trip: a splitter that retags metrics by threshold, whose output a second plawk program reads back with the matching `BINFMT` (`TAG == 0 { bigsum += $1 } ...`, output `350 1`).
- Codegen rejects a plain `writebin` against a union layout and an arm-targeted one against a flat layout (`plawk_native_codegen.pl:3097-3104`).

## Strings, lists, group-bys on the way out

- `sN` and `lpsN` output slots (length plus exactly the payload bytes, byte-compatible with the reader); `repK` passthrough; `END { for (k in counts) writebin k, counts[k] }` in binary-input mode.

## Pipelines with no text in the middle

- converter | aggregator; the project's byte-for-byte reader/writer compatibility claim; what is and is not shown by the tests.
- <!-- TODO: confirm whether any test pipes one compiled plawk binary into another through a real OS pipe, versus writing and re-reading a file (recon follow-up) -->
