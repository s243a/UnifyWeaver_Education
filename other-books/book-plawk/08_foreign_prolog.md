<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 8: Foreign Prolog: `@prolog`, functions, bridged calls

Chapter 7 ended on a warning: the Prolog core is a reference model, not what runs your program. This chapter is where Prolog does run. A `.plawk` file can carry its own Prolog clauses, and a rule can call them. Where awk reaches for a regex to decide whether a field is interesting, plawk can call a real predicate — with unification, arithmetic, and backtracking — compiled into the same binary as the record loop. The regex asks *does this text match a shape*; the predicate can ask *is this value in a table, does it satisfy a relation, does a small grammar accept it*. That is the whole point of compiling through a Prolog toolchain: the logic language is right there, and a rule can step into it.

Every program below is exercised by a plunit test that builds a native binary with `clang` and runs it, and I ran those tests for this book. The four files behind this chapter — `test_plawk_prolog_blocks.pl` (6/6), `test_plawk_surface_prolog_calls.pl` (16/16), `test_plawk_functions.pl` (5/5), and `test_plawk_f64_foreign.pl` (7/7) — all pass. Every number quoted below (`375`, `62`, `43`, and the rest) is an assertion in one of those passing tests, not a figure I typed from memory.

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

Read that program as awk with two of its names bound to Prolog. `plawk_pbt_hot($1)` is a *pattern* — a rule fires only when it succeeds — and `plawk_pbt_weight($1, $2)` is a *value*, the product the rule accumulates. Neither name is built in; both are defined in the block above, and the record loop calls them per line. Over records `(200, 1.5)`, `(5, 2.5)`, `(300, 0.25)` it prints `375` (`test_plawk_prolog_blocks.pl`, `surface_embedded_predicates_end_to_end`). The middle record is dropped because `plawk_pbt_hot(5)` fails (`5 > 100` is false); the first and third pass, and `200*1.5 + 300*0.25 = 375`. The `float(...)` wrapper matters: `plawk_pbt_weight` returns a WAM float, and without the wrapper the accumulator would be an `i64` and the fractional parts would be lost (Chapter 3's typing, reached through a bridged call).

Blocks may appear anywhere between top-level program parts, and DCG rules (`-->`) are allowed and are expanded, which is what makes the Tier-2 payload parsing at the end of this chapter possible.

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

On `i64` values 3, 20, 11 it prints `62`: the guard `plawk_fnt_hot` passes 20 and 11 (both `> 10`), each doubled, `40 + 22`. A float-constant function works through `float(...)` and mixes with an integer one in a single rule (`surface_functions_end_to_end`):

```
BEGIN { BINFMT = "i64 f64" }
function plawk_fnt_scale(a, b) { return a * b + 1 }
function plawk_fnt_half(x) { return x * 0.5 }
{ s += plawk_fnt_scale($1, $1)
  w += float(plawk_fnt_half($1)) }
END { print s, w }
```

On the records `(3, 0.25)` and `(5, 0.25)` it prints `36 4`. The integer accumulator is `s = (3*3+1) + (5*5+1) = 36`; the double accumulator is `w = float(3*0.5) + float(5*0.5) = 1.5 + 2.5 = 4`. Two desugared clauses, two slots of different types, one record loop.

**Required order.** The program grammar is fixed as an optional `BEGIN` block, then all `function` definitions, then the rules, then an optional `END` (`program/3`, `plawk_parser.pl:124-130`). A `function` placed before `BEGIN` therefore does not parse: after the empty `BEGIN` alternative, the grammar goes straight to `function_defs`, and a trailing `BEGIN { ... }` is then not a valid rule. This explains the failure seen in an earlier trial; I derived it from the grammar and did not re-run it. `@prolog` blocks are exempt, since they are stripped out before the grammar runs. Put them anywhere.

**Text fields as arguments, and the one sharp edge.** How a field reaches the predicate depends on the mode, and this is the place where a text-mode program can surprise an awk programmer.

In *binary* mode the field arrives already typed: an `i64` column is marshalled as a WAM integer and an `f64` column as a WAM float (`test_plawk_f64_foreign.pl`, `f64_field_arg_marshals_as_wam_float`). The bridged predicate then does real arithmetic on real numbers, which is why every arithmetic example in this chapter declares a `BINFMT`.

In *text* mode there is no column type to marshal, and the code generator makes a single, deliberate choice: it marshals a field `$N` by interning its raw bytes as a Prolog **atom** (`plawk_native_codegen.pl:6310-6343`; `$0` is interned the same way, through `wam_intern_atom`). The field is *not* parsed into a number. So the text of a field `3` becomes the atom `'3'`, not the integer `3`.

That makes the supported text-mode argument an atom to *classify* or *look up*, which is exactly the thesis of this chapter: `plawk_is_error($1)` tests an atom against a fact table, `plawk_severity_rank($1)` maps an atom to a rank, and both work on the four-line input above. Integer and string literals are supported too, and they arrive as themselves — `print plawk_double_plus(21)` prints `43`, doing integer arithmetic on the literal `21`.

What is *not* supported is arithmetic over a raw text field. A call such as `print scale($1, $2)` on the line `3 4` hands the atoms `'3'` and `'4'` to the clause's `is/2`, which is not fractional-or-integer `3` and `4`; the formula does not compute what an awk programmer, used to awk's automatic string-to-number coercion, would expect. The test suite contains no passing text-mode arithmetic case, and this book documents none.

The root cause is therefore known and narrow — text fields intern as atoms, with no numeric coercion step — rather than a mysterious failure. **Open note:** whether plawk *should* coerce a numeric-looking text field to a WAM number at the bridge (restoring awk's behaviour) or should keep the atom semantics and require an explicit conversion is an unsettled design question, not a bug with a known fix. It is carried in Chapter 11's roadmap alongside the double-as-pattern-guard question.

## How the bridge works

The native loop does not call into an interpreter on every line. The driver emits, per predicate, a wrapper function around a single shared WAM state. There are three shapes, keyed by how the call is used:

- `@plawk_foreign_guard_<name>_<arity>` returns `i1` — the guard's success is the pattern's truth.
- `@plawk_foreign_call_<name>_<arity>` returns `{ i64, i1 }` — an integer result and a success flag.
- `@plawk_foreign_fcall_<name>_<arity>` returns `{ double, i1 }` — the `float(...)` form, a double and a flag.

The exact signatures are asserted by the test, for example `define i1 @plawk_foreign_guard_plawk_is_error_1(%Value %a0)` and `define { i64, i1 } @plawk_foreign_call_plawk_severity_rank_1(%Value %a0)` (`test_plawk_surface_prolog_calls.pl`, `foreign_driver_ir_has_wrappers_and_lazy_vm`). The shared state is `@plawk_foreign_vm = internal global %WamState* null`, created lazily on first use, so a program that never bridges pays nothing for the machinery (`plain_program_needs_no_vm_counts` checks the IR carries no `@plawk_foreign_vm` at all).

Follow one line of `plawk_is_error($1) { total += plawk_severity_rank($1) }`. The record loop marshals `$1` into a `%Value`, calls the guard wrapper, and branches on the returned `i1`; a false result skips straight to the next line. On a true result it marshals `$1` again, calls the value wrapper, and the `{ i64, i1 }` it returns feeds the `add` into `total` — with the silent-zero rule from the previous section applying when the flag is false. Inside each wrapper the steps are fixed: load the predicate's entry (`load i32, i32* @plawk_is_error_start_pc`), run `wam_prepare_call` and the WAM `run_loop`, then unwind. The wrapper saves the heap top before the call and restores it afterward, rewinding the arena, so a million records cost the same constant memory as one (`plawk_native_codegen.pl:619-660`). The VM is natively compiled, but it is still an interpreter stepping WAM instructions, which is where the microsecond-scale per-call cost quoted below comes from and why the surrounding record loop stays native.

The project quotes about 5 microseconds per bridged call and about 0.2 microseconds on the blob path. I did not measure either.

The error surface is deliberate. The CLI checks that every guard `name/N` and every value call `name/N+1` is defined by a block or a `function`; if not, it prints "the program calls ... but no @prolog block or function defines ..." and exits 3 (`bin/plawk:185-206`). A defined predicate that calls an undefined helper fails the compile with an `existence_error`; it used to lower to label 0 and fail silently at runtime (`calling_an_uncompiled_predicate_fails_the_compile`).

## Tier-2 payloads

A record can carry a variable-sized payload: with `BINFMT = "i64 blob32"`, the native loop frames each record and a DCG in the `@prolog` block parses the payload. One blob argument per call; payloads must be NUL-free. The test pairs `total += plawk_pbt_sum($2)` with a comma-separated-integers grammar, using `-->` rules, if-then-else, a cut, and `code_type/2`. On records `(1,"12,7")`, `(2,"100")`, `(-5,"9")`, guarded by `$1 > 0`, it prints `119` (`surface_embedded_dcg_parses_blob_payloads`, `surface_embedded_dcg_with_ite_cut_and_code_type`). A missing `code_type/2` builtin once made this grammar silently return 0; it is now a builtin, and unknown callees fail the compile.

Treat the demonstrated edge as these tests. The shapes above, from binary `i64`/`f64` fields and text atoms to blob payloads, are covered; anything past them, text-to-number coercion included, is open.
