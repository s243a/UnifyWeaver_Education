<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 3: Typed arithmetic: `i64` and `float`

Chapter 1 showed two places where plawk and awk disagree before any binary record appears: `avg 7` versus `avg 7.5`, and `0` versus an empty line for an uninitialised scalar. This chapter explains both from the types up. The short version is that plawk has exactly two numeric types, a 64-bit signed integer (`i64`) and a double, and the compiler decides which one every expression and every scalar has. awk has one type that is, in effect, a double with string manners.

Source references are to `examples/plawk/` in the UnifyWeaver repository, read at commit `0af53d6a3`. The double-slot results in the "What is implemented today" section below were verified by running `tests/test_plawk_float_slots.pl` (13 tests, all passing). The integer- and float-expression outputs quoted elsewhere in this chapter are the values asserted by `tests/test_plawk_surface_arith_exprs.pl` and `tests/test_plawk_surface_float_exprs.pl`; those two suites were not separately re-run for this chapter.

## Integer expressions

The default is integer. `+ - * / %` work over native `i64` with awk precedence (`*` `/` `%` bind tighter than `+` `-`, both levels left-associative) and parentheses, so `{ print ($2 + $3) * $4, $2 + $3 * $4 }` means what an awk programmer expects. The parser builds `add_i64`, `sub_i64`, `mul_i64`, `div_i64`, `mod_i64` nodes (`parser/plawk_parser.pl`, `i64_binary_surface_expr`, around lines 1168-1233).

Operands are integer literals, `NR`, `NF`, `length($N)`, `int($N)`, `index($N, "lit")`, and a bare numeric `$N`, which coerces like `int($N)` inside arithmetic. A bare `$N` standing alone still prints as a byte slice; it is only inside an operator expression that it becomes a number.

That already explains the first divergence. Take the lines `1 7` and `2 8`:

```awk
{ s += $2 } END { print "avg", s / NR }
```

`s` accumulates `i64` values, `NR` is an `i64`, and `/` on two `i64`s is integer division: 15 / 2 is `7`. gawk converts everything to double and prints `7.5`. The test suite pins the same split on a single field: `{ print $2 / 2.0, $2 / 2 }` over `a 7` expects `3.5 3` (`test_plawk_surface_float_exprs.pl`, `surface_int_and_float_division_differ`). The `7.5` for `$2 / 2.0` holds in gawk too; only the integer-only form differs.

### Guarded division

A native `sdiv` traps on a zero divisor and on `INT64_MIN / -1`. awk has no integers to overflow and reports a fatal division-by-zero error. plawk does neither; it defines the result (README arithmetic paragraph, asserted in `test_plawk_surface_arith_exprs.pl`):

| Expression | Result | Test |
|---|---|---|
| `7 / 0`, `7 % 0` | `0`, `0` | `surface_division_by_zero_yields_zero` |
| `INT64_MIN / -1`, `INT64_MIN % -1` | `INT64_MIN`, `0` | `surface_int64_min_overflow_division_is_defined` |
| `-7 / 2`, `-7 % 2` | `-3`, `-1` | `surface_division_and_modulo` |

The last row shows truncation toward zero, the C convention. A program that divides by a count that might be zero therefore keeps running and silently produces `0`. That is a design choice for a compiled filter that must not crash mid-stream; gawk would stop with an error instead, so treat the zero as a value to be aware of, not as awk's behaviour.

### Text that is not a number

A field that is not a strict signed decimal coerces to `0` in integer arithmetic. `{ print $2 + $3 }` over `a nope 5` prints `5` (`surface_nonnumeric_fields_coerce_to_zero`). "Strict" matters: it is the whole field, so trailing junk makes the whole field `0` here. Contrast `float($N)` below, which is lenient.

## Float expressions

You opt into floating point; nothing becomes a double by accident. A tree is double-typed when any leaf is a float literal, a `float($N)` coercion, or a `float(pred(args...))` call (`plawk_expr_is_double`, `codegen/plawk_native_codegen.pl` around 5577-5591). Any `i64` operand elsewhere in that tree is promoted with LLVM `sitofp`, so mixed expressions just work: `print float($2) + $3` over `a 0.5 2` prints `2.5`, and `print NR * 0.5` over two lines prints `0.5` then `1`.

Two details of how leaves are built:

- **Literals are exact ratios.** `1.5` parses to `float_const(15, 10)` (`plawk_parser.pl` around 1313-1323) and is emitted as `fdiv double 15.0, 10.0`, which gives the correctly rounded double rather than depending on a decimal-string parse at run time. The test `surface_decimal_literals_round_correctly` asserts `{ print 0.1 + 0.2 }` prints `0.3` (see `%g` below for why).
- **`float($N)` has `strtod` semantics:** leading number taken, trailing text ignored, `0` when nothing parses. Over `3.14`, `2.5rest`, `abc` the test expects `3.14`, `2.5`, `0`. In a binary-record program (Chapter 4), `float($N)` on an `f64` field is just the native load.

So the cure for `avg 7` is to make one operand a double:

```awk
{ s += float($2) } END { print "avg", s / NR }
```

This prints `avg 7.5` on the two-line input. The division is IEEE `fdiv`. There is **no** zero guard on float division: dividing by zero gives `inf` or `nan` as in C, where the integer path would give `0`. Doubles print with `%g`, so `2.0 * 10` prints `20`, not `20.0`, and `0.1 + 0.2` prints `0.3` because `%g` keeps six significant digits.

The expected-output contrast with gawk, for `$2 = 7`:

| Program | plawk | gawk |
|---|---|---|
| `print $2 / 2` | `3` | `3.5` |
| `print $2 / 2.0` | `3.5` | `3.5` |

## Scalar slot typing

The second divergence is about scalars. Every scalar is a compiler-managed *slot*, carried through the record loop as an LLVM phi, and a slot has a type fixed at compile time. Its initial value is the zero of that type: `0` for an `i64` slot, `0.0` for a double slot (`plawk_slot_zero_ir`, `plawk_native_codegen.pl` around 1705-1715). That is why `$1 == "NOPE" { c++ } END { print c }` prints `0`: `c` is an `i64` slot that was never touched, not an unset string. There is no "empty" state to print.

Which type does a slot get? A fixpoint inference (`plawk_scalar_double_fixpoint`, around 1902-1954). A scalar is a **double** if any update assigns it a double-typed expression, or reads an already-double scalar; everything else is `i64`. The read rule makes it transitive: in `{ a = 1.5 ; b = a + 1 }`, `a` is double because of the literal, and `b` is double because it reads `a` (`fixpoint_promotes_transitive_reads`). `i64` operands inside a double update are promoted with `sitofp` at the update site. Slots are per name, not per use, so one program can mix types:

```awk
{ sum += float($2) * 1.5 ; n++ } END { print n, sum }
```

Here `sum` becomes a double slot (double phis, `fadd` updates) while `n++` stays `i64` (`i64_slots_stay_i64`, `surface_double_accumulator_text`). Both `+=` and `=` forms exist for double slots (`surface_double_set_overwrites`), and they work through `if`/`else`, `next` and `break`, and rule chains.

### What is implemented today

The project README contradicts itself here. One paragraph says doubles are "expression-level only in this slice: scalar slots, guards, and `END` expressions stay `i64`, and assigning a double expression to a scalar is rejected at codegen; typed double slots are the documented follow-up." A later paragraph describes typed double scalar slots as working. The source — and the test suite — side with the later paragraph; the earlier text is stale. Running `tests/test_plawk_float_slots.pl` passes all 13 of its tests, which exercise exactly the double-slot behaviour below:

- **Double slots exist.** `scalar_double(Name)` is a slot kind with LLVM type `double` and zero `0.0`; update operations for `add` and `set` on it emit `fadd double` (`plawk_native_codegen.pl` around 1705-1715, 5304-5320). The test `double_slot_ir_uses_double_phis_and_fadd` asserts `phi double [0.0, ...]` in the generated IR, and `i64_slots_stay_i64` confirms an untouched integer slot keeps no `phi double` or `fadd`.
- **Double `END` expressions exist.** `END { print sum + 1 }` promotes to f64 and `END { print sum / NR }` is an IEEE `fdiv`; float literals in `END` do the same, as in `END { print n * 1.5 }`. The test comment on `end_arith_on_double_slot_promotes_to_f64` records that this "was rejected before the f64 END slice". The code path is `plawk_end_scalar_operand_expr` accepting `float_const` (around 4885-4890), with double slot reads substituted as `ssa_f64` (around 4912-4934).
- **Binary mode too.** `$1 > 10 { sum += float($2) }` over `i64 f64` records accumulates a native double (`surface_binary_double_accumulator`).

What the tests do **not** establish is a double used as the *guard* operand, e.g. a pattern such as `sum > 3` where `sum` is a double slot. I found no test of it, and I did not trace the guard code to a verdict. Treat it as unconfirmed rather than as either supported or rejected. <!-- TODO: confirm whether a double scalar or float expression is accepted as a pattern guard operand; test it and update this paragraph -->

Where guards *are* shown with floats, the guard is an ordinary string/integer test and the double only appears in the action: `$1 == "ERROR" { print $3 * 1.5 }` prints `15` and `4.5` for the two `ERROR` lines (`surface_float_composes_with_guards`).

## `printf`

`printf` is the one place the types surface in format strings. The supported native formats are `%%`, `%s`, `%d`/`%i`/`%ld` for `i64` values, and `%f`/`%g`/`%e` for doubles with optional precision. `{ printf "%.2f;", $2 / 3.0 }` over `10` and `1` prints `3.33;0.33;`, and `%g|%e` of `float($2) * 2.0` and `float($2)` over `1.25` prints `2.5|1.250000e+00`. Unlike `print`, `printf` adds no `OFS` and no trailing newline, and a `%s` of a field slice lowers to `%.*s`. A format that does not match the operand's type, such as `%d` given a double, is outside this list; the README says double expressions into `i64` formats are rejected. <!-- TODO: confirm exact failure mode of a type-mismatched printf format -->

## Summary

| awk habit | plawk behaviour | To get awk's |
|---|---|---|
| `/` is real division | integer division when both sides are `i64` | make one operand `float($N)` or a float literal |
| `x / 0` is an error | `0` (integer), `inf`/`nan` (float) | test the divisor yourself |
| unset scalar is `""` / `0` | slot is `0` (or `0.0`), never empty | no equivalent |
| `"abc" + 1` is `1` | `0` for a whole non-numeric field; `float()` keeps a leading number | `float($N)` for lenient parsing |

## Next

Chapter 4: Binary records. Typed arithmetic becomes cheaper once `$1` is a single load of a declared type rather than text parsed on each use.
