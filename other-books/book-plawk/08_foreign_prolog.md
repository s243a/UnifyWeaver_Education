<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 8: Foreign Prolog: `@prolog`, functions, bridged calls

> *Skeleton. Source tests: `test_plawk_prolog_blocks.pl`, `test_plawk_surface_prolog_calls.pl`, `test_plawk_functions.pl`, `test_plawk_f64_foreign.pl`, `test_plawk_tier2_blob.pl`.*

## `@prolog ... @end` blocks

- Markers alone on a line; clauses (DCG rules included) read with `read_term/3` before the awk grammar parses; tagged form `@prolog-TAG ... @end-TAG` for text containing an `@end`-shaped line; unterminated block fails the parse (`plawk_parser.pl:50-119`).
- Entry points: `plawk_parse_source/3` returns program plus clauses; `plawk_prolog_block_preds/2` installs them (`plawk_native_codegen.pl:28-44`).

## Calling Prolog from a rule

- As a guard: `hot($1) { ... }` becomes `prolog_guard(Name, Args)` (`plawk_parser.pl:420-427`). As an expression: `total += severity_rank($1)` is `prolog_call(Name, Args)` (`plawk_parser.pl:1298-1305`), integer result, `0` on failure. Double result: `float(pred(...))` becomes `float_call` (`plawk_parser.pl:1332-1339`).
- Verified: the `weight`/`hot` program over three binary records prints `1175`.
- Arguments: fields, string literals, integers; i64 marshals as WAM integers and f64 as WAM floats in binary mode.

## `function` as sugar

- `function scale(a, b) { return a * b + 1 }` desugars to `scale(A, B, R) :- R is A * B + 1` (`plawk_parser.pl:136-177`). Single-expression only (Chapter 2). Verified in an integer `BINFMT` program; observed to be order-sensitive (a `function` before `BEGIN` failed to parse in a quick trial). <!-- TODO: confirm the required ordering of BEGIN, function, and rules in the grammar (recon follow-up) -->
- Observed: calling a function on *text* fields in `print` position gave `1` rather than the arithmetic result in a quick trial; document only what the tests cover (binary `i64` contexts) until confirmed. <!-- TODO: confirm text-mode field coercion for bridged calls (recon follow-up) -->

## How the bridge works

- Per-predicate wrapper functions around a lazily created shared `%WamState`; each call runs `wam_prepare_call` plus `run_loop`, then restores the heap top and rewinds the arena, so per-record calls use constant memory (README; `plawk_native_codegen.pl:619-660`). Project-quoted costs (about 5 microseconds per call; about 0.2 microseconds for the blob path) as the project's figures, not re-measured here.
- Error surface: a program calling an undefined predicate is a compile error (exit 3).

## Tier-2 payloads

- `BINFMT = "i64 blob32"` and `total += payload_sum($2)`: native loop frames the record, a WAM-compiled DCG parses the payload. One blob argument per call; NUL-free payloads.
