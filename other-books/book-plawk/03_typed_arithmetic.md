<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 3: Typed arithmetic: `i64` and `float`

> *Skeleton.*

## Integer expressions

- `+ - * / %` over native `i64` with awk precedence and parentheses; operands are literals, `NR`, `NF`, `length($N)`, `int($N)`, `index($N, "lit")`, bare numeric `$N` (parser `add_i64`..`mod_i64`, `plawk_parser.pl:1168-1233`; README arithmetic paragraph).
- Guarded division: zero divisor yields `0`; `INT64_MIN / -1` wraps; `% -1` yields `0`.
- Failed numeric parse of a text field yields `0` (`int($N)`).

## Float expressions

- A double expression arises from a float literal or `float($N)` (`strtod` semantics); `i64` operands promote with `sitofp`. Literals kept as exact ratios (`float_const(Mantissa, Denominator)`, `plawk_parser.pl:1313-1323`).
- Float division has IEEE semantics (no zero guard); doubles print with `%g`.
- Contrast with awk: `print $2 / 2` is `3` here and `3.5` in gawk for `$2 = 7`; `print $2 / 2.0` is `3.5` in both.

## Scalar slot typing

- A scalar becomes a double if any update assigns a float expression or reads an already-double scalar (fixpoint inference); `n++` stays `i64` in the same program.
- Documented limit: doubles in guards and in some `END` positions; "typed double slots are the documented follow-up" (README). <!-- TODO: confirm which of the README's stated double-slot limits are still current, since later README paragraphs describe double scalar slots as implemented (recon follow-up) -->

## `printf`

- Supported native formats: `%%`, `%s`, `%d`/`%i`/`%ld`, and `%f`/`%g`/`%e` with precision for doubles; no implicit newline or `OFS`; `%s` of a field slice lowers to `%.*s` (README).
