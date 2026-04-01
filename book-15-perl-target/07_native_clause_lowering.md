<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 7: Native Clause Body Lowering

Write natural Prolog predicates and let UnifyWeaver compile them into Perl subroutines with `if`/`elsif`/`else` chains.

## How It Works

```prolog
classify(X, small) :- X > 0, X < 10.
classify(X, large) :- X >= 10.
```

Generates:

```perl
sub classify {
    my ($arg1) = @_;
    if ($arg1 > 0 && $arg1 < 10) {
        return "small";
    } elsif ($arg1 >= 10) {
        return "large";
    } else {
        die "No matching clause for classify/2\n";
    }
}
```

## Basic Examples

### Arithmetic

```prolog
double(X, R) :- R is X * 2.
```

```perl
sub double {
    my ($arg1) = @_;
    return ($arg1 * 2);
}
```

### Absolute Value

```prolog
abs_val(X, R) :- (X >= 0 -> R = X ; R is -X).
```

```perl
sub abs_val {
    my ($arg1) = @_;
    return ($arg1 >= 0) ? $arg1 : (-$arg1);
}
```

Perl's ternary `? :` is used for if-then-else expressions.

## Perl-Specific Syntax

| Prolog | Perl |
|--------|------|
| `X > 0, X < 10` | `$arg1 > 0 && $arg1 < 10` |
| `X =:= 0` | `$arg1 == 0` |
| `R is abs(X)` | `abs($arg1)` |
| `R is X mod 2` | `($arg1 % 2)` |
| `(C -> T ; E)` | `C ? T : E` (ternary) |
| No match | `die "...\n"` |

## Summary

- Multi-clause predicates compile to Perl `if`/`elsif`/`else`
- `$` prefix on all variables
- `die` for unmatched clauses
- Ternary `? :` for inline if-then-else

---

## Navigation

**←** [Previous: Chapter 5: JSON Pipeline](05_json_pipeline) | [Book 15: Perl Target](./)
