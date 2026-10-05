<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 8: Foreign Prolog: `@prolog`, functions, bridged calls

Chapter 7 ended on a warning: the Prolog core is a reference model, not what runs your program. This chapter is where Prolog does run. A `.plawk` file can carry its own Prolog clauses, and a rule can call them. Where awk reaches for a regex to decide whether a field is interesting, plawk can call a real predicate, with unification, arithmetic, and backtracking, compiled into the same binary as the record loop.

Every program below is exercised by a plunit test that builds and runs it; those tests need `clang`. The expected outputs are the tests' own. I did not re-run them for this chapter.

## `@prolog ... @end` blocks

A block is fenced by markers that sit alone on their line (leading and trailing blanks are fine). Everything between them is read as ordinary Prolog with `read_term/3`, one clause at a time, before the awk grammar sees the file; the block text never touches the awk parser (`plawk_parser.pl:50-119`). The test program:

```
@prolog
plawk_pbt_weight(I, F, R) :- R is I * F.
plawk_pbt_hot(X) :- X > 100.
@end

BEGIN { BINFMT = "i64 f64" }
plawk_pbt_hot($1) {
  wsum += float(plawk_pbt_weight($1, $2))
}
END { print wsum }
```

Over records `(200, 1.5)`, `(5, 2.5)`, `(300, 0.25)` it prints `375` (`test_plawk_prolog_blocks.pl`, `surface_embedded_predicates_end_to_end`): the first and third records pass the guard, and `200*1.5 + 300*0.25 = 375`. Blocks may appear anywhere between top-level program parts, and DCG rules (`-->`) are allowed and are expanded.

Two fencing details, both tested. If the Prolog text itself contains an `@end`-shaped line, use the tagged form: `@prolog-t2` closes only at `@end-t2`, and a mismatched tag leaves the block unterminated. An unterminated block fails the parse. Directives such as `:- dynamic foo/1.` are not clauses, and a block containing one is rejected (`tagged_markers_and_rejections`; the CLI reports "invalid @prolog block", exit 2).

The plumbing has two steps. `plawk_parse_source/3` returns the program and the clause list. `plawk_prolog_block_preds/2` asserts the clauses, expanding DCGs, and returns the `user:Name/Arity` list that `write_wam_llvm_project/3` takes (`plawk_native_codegen.pl:18-44`). `bin/plawk` runs exactly this sequence (`bin/plawk:102`). `plawk_parse_string/2` accepts the same source but discards the clauses, so it is for inspecting the AST, not for compiling.

## Calling Prolog from a rule

A call has two surface forms.

- **As a guard.** `plawk_is_error($1) { print $0 }` parses to `prolog_guard(plawk_is_error, [field(1)])`. The predicate's *success* is the pattern's truth; it needs the same arity as the call. Guards compose with native tests (`plawk_is_error($1) && $3 > 100`), negate (`!plawk_is_error($1)`), and work inside `if (...)`.
- **As a value.** `plawk_severity_rank($1)` parses to `prolog_call(Name, Args)`. The predicate takes one extra, final output argument that carries the result. The result is an integer, and a failed call yields `0`. It composes with arithmetic: `r = plawk_severity_rank($1) * 10 + 1`. To keep a fractional result, wrap the call in `float(...)`; a failed `float(...)` call contributes `0.0` (`test_plawk_f64_foreign.pl`, `surface_failed_float_call_contributes_zero`).

Arguments are fields, string literals, or integer literals (`check($0, "limit", -5)` parses). The tests define the predicates in Prolog rather than in a block, but the call surface is identical. On the four-line input `ERROR disk 300 / WARN cpu 50 / INFO net 15 / FATAL mem 8`, with `plawk_is_error` a two-fact guard and `plawk_severity_rank` a three-way if-then-else:

| program | output |
|---|---|
| `plawk_is_error($1) { print $0 }` | `ERROR disk 300` and `FATAL mem 8` |
| `plawk_is_error($1) && $3 > 100 { hits++ } END { print hits }` | `1` |
| `{ print $1, plawk_severity_rank($1) }` | `ERROR 3`, `WARN 2`, `INFO 1`, `FATAL 1` |
| `{ total += plawk_severity_rank($1) } END { print total }` | `7` |
| `{ r = plawk_severity_rank($1) * 10 + 1; total += r } END { print total }` | `74` |

(`test_plawk_surface_prolog_calls.pl`, `surface_prolog_*`.) The `FATAL` row is the point of the thesis: `plawk_is_error` is a fact table, and `WARN` and `INFO` fall to the default clause. No regex alternation is involved.

## `function` as sugar

For the common case of a one-line formula, plawk offers an awk-style `function`. `function scale(a, b) { return a * b + 1 }` desugars at parse time to the clause `scale(A, B, R) :- R is A * B + 1.` (`plawk_parser.pl:132-177`). The body must be a single `return` of an arithmetic expression over the parameters (awk precedence, with `%` mapping to Prolog `mod`). An identifier that is not a parameter fails the parse, and so does a body that is an assignment instead of a `return` (`test_plawk_functions.pl`, `function_rejections`). That is Chapter 2's single-expression rule seen from the Prolog side.

The generated clause joins the same clause list as any `@prolog` block and is called like any bridged predicate. Sugar and blocks mix in one program (`surface_functions_mix_with_prolog_blocks`):

```
@prolog
plawk_fnt_hot(X) :- X > 10.
@end
BEGIN { BINFMT = "i64 f64" }
function plawk_fnt_twice(a) { return a * 2 }
plawk_fnt_hot($1) { s += plawk_fnt_twice($1) }
END { print s }
```

On `i64` values 3, 20, 11 it prints `62`. A float-constant function works through `float(...)`: with `function plawk_fnt_half(x) { return x * 0.5 }`, the records `(3, 0.25)` and `(5, 0.25)` give `w = 1.5 + 2.5`, printed as `4` (`surface_functions_end_to_end`).

**Required order.** The program grammar is fixed as an optional `BEGIN` block, then all `function` definitions, then the rules, then an optional `END` (`program/3`, `plawk_parser.pl:124-130`). A `function` placed before `BEGIN` therefore does not parse: after the empty `BEGIN` alternative, the grammar goes straight to `function_defs`, and a trailing `BEGIN { ... }` is then not a valid rule. This explains the failure seen in an earlier trial; I derived it from the grammar and did not re-run it. `@prolog` blocks are exempt, since they are stripped out before the grammar runs. Put them anywhere.

**Text fields as arguments.** In binary mode, fields arrive typed: `i64` as a WAM integer, `f64` as a WAM float (`test_plawk_f64_foreign.pl`, `f64_field_arg_marshals_as_wam_float`). In text mode, the code generator marshals a field `$N` by interning its text as a Prolog **atom** (`plawk_native_codegen.pl:6310-6343`; `$0` is interned the same way). It is never converted to a number. So the supported text-mode argument is an atom to *classify*: `plawk_is_error($1)` and `plawk_severity_rank($1)` above, plus integer and string literals (`print plawk_double_plus(21)` prints `43`). Arithmetic over a text field, such as `print scale($1, $2)` on `3 4`, hands atoms `'3'` and `'4'` to `is/2`. That is not what the formula wants, and it matches the earlier observation that the result was not `13`. The tests contain no passing text-mode arithmetic case, so I document none. <!-- TODO(run-verify): reproduce `print scale($1,$2)` on text input, confirm the printed `1` is the failed-call/`is/2` fallback rather than something else, and decide whether atom-to-number coercion is a gap or by design. -->

## How the bridge works

The native loop does not call into an interpreter on every line. The driver emits, per predicate, a wrapper function (`@plawk_foreign_guard_<name>_<arity>` returning `i1`; `@plawk_foreign_call_<name>_<arity>` returning `{ i64, i1 }`, a value and a success flag; `@plawk_foreign_fcall_...` returning `{ double, i1 }`) around a single shared `%WamState` that is created lazily on first use (`@plawk_foreign_vm = internal global %WamState* null`). Each call runs `wam_prepare_call` and then the WAM `run_loop`. Before the call the wrapper saves the heap top; afterwards it restores the heap top and rewinds the arena, so per-record calls use constant memory (`test_plawk_surface_prolog_calls.pl`, `foreign_driver_ir_has_wrappers_and_lazy_vm`; `plawk_native_codegen.pl:619-660`). A program with no foreign call emits none of this (`plain_program_needs_no_vm_counts`).

The project quotes about 5 microseconds per bridged call and about 0.2 microseconds on the blob path. I did not measure either.

The error surface is deliberate. The CLI checks that every guard `name/N` and every value call `name/N+1` is defined by a block or a `function`; if not, it prints "the program calls ... but no @prolog block or function defines ..." and exits 3 (`bin/plawk:185-206`). A defined predicate that calls an undefined helper fails the compile with an `existence_error`; it used to lower to label 0 and fail silently at runtime (`calling_an_uncompiled_predicate_fails_the_compile`).

## Tier-2 payloads

A record can carry a variable-sized payload: with `BINFMT = "i64 blob32"`, the native loop frames each record and a DCG in the `@prolog` block parses the payload. One blob argument per call; payloads must be NUL-free. The test pairs `total += plawk_pbt_sum($2)` with a comma-separated-integers grammar, using `-->` rules, if-then-else, a cut, and `code_type/2`. On records `(1,"12,7")`, `(2,"100")`, `(-5,"9")`, guarded by `$1 > 0`, it prints `119` (`surface_embedded_dcg_parses_blob_payloads`, `surface_embedded_dcg_with_ite_cut_and_code_type`). A missing `code_type/2` builtin once made this grammar silently return 0; it is now a builtin, and unknown callees fail the compile.

Treat the demonstrated edge as these tests. The shapes above, from binary `i64`/`f64` fields and text atoms to blob payloads, are covered; anything past them, text-to-number coercion included, is open.
