<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 6: Native Clause Body Lowering

Write natural Prolog predicates and let UnifyWeaver compile them into Ruby methods with `if`/`elsif`/`else`/`end` chains.

## How It Works

```prolog
classify(X, small) :- X > 0, X < 10.
classify(X, large) :- X >= 10.
```

Generates:

```ruby
def classify(arg1)
  if arg1 > 0 && arg1 < 10
    "small"
  elsif arg1 >= 10
    "large"
  else
    raise "No matching clause for classify/2"
  end
end
```

Ruby's implicit returns mean the last expression in each branch is the return value — no `return` keyword needed.

## Basic Examples

### Arithmetic

```prolog
double(X, R) :- R is X * 2.
```

```ruby
def double(arg1)
  (arg1 * 2)
end
```

### If-Then-Else

```prolog
abs_val(X, R) :- (X >= 0 -> R = X ; R is -X).
```

```ruby
def abs_val(arg1)
  arg1 >= 0 ? arg1 : (-arg1)
end
```

## Ruby-Specific Syntax

| Prolog | Ruby |
|--------|------|
| `X > 0, X < 10` | `arg1 > 0 && arg1 < 10` |
| `X =:= 0` | `arg1 == 0` |
| `R is abs(X)` | `arg1.abs` |
| `R is X mod 2` | `arg1 % 2` |
| `(C -> T ; E)` | `C ? T : E` (ternary) |
| No match | `raise "..."` |

## Summary

- Multi-clause predicates compile to Ruby `if`/`elsif`/`else`/`end`
- Implicit returns — no `return` keyword
- `raise` for unmatched clauses
- `.abs` method for absolute value

---

## Navigation

**←** [Previous: Chapter 5: JSON & Rails](05_json_rails) | [Book 16: Ruby Target](./)
