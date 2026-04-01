<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 6: Native Clause Body Lowering

Write natural Prolog predicates and let UnifyWeaver automatically compile them into Python functions with `if`/`elif`/`else` chains and ternary expressions.

## How It Works

```prolog
classify(X, small) :- X > 0, X < 10.
classify(X, large) :- X >= 10.
```

Generates:

```python
def classify(arg1):
    if arg1 > 0 and arg1 < 10:
        return "small"
    elif arg1 >= 10:
        return "large"
    else:
        raise ValueError("No matching clause for classify/2")
```

## Basic Examples

### Three-Way Classification

```prolog
grade(X, low)  :- X < 50.
grade(X, mid)  :- X >= 50, X < 80.
grade(X, high) :- X >= 80.
```

```python
def grade(arg1):
    if arg1 < 50:
        return "low"
    elif arg1 >= 50 and arg1 < 80:
        return "mid"
    elif arg1 >= 80:
        return "high"
    else:
        raise ValueError("No matching clause for grade/2")
```

### Arithmetic

```prolog
double(X, R) :- R is X * 2.
```

```python
def double(arg1):
    return (arg1 * 2)
```

## If-Then-Else

Prolog's `(Cond -> Then ; Else)` compiles to Python's ternary expression:

```prolog
abs_val(X, R) :- (X >= 0 -> R = X ; R is -X).
```

```python
def abs_val(arg1):
    return arg1 if arg1 >= 0 else (-arg1)
```

Python's ternary `x if cond else y` is a natural fit for Prolog's if-then-else.

### Nested If-Then-Else

```prolog
range_classify(X, R) :-
    (X < 0 -> R = negative
    ; (X =:= 0 -> R = zero
    ; R = positive)).
```

```python
def range_classify(arg1):
    return "negative" if arg1 < 0 else ("zero" if arg1 == 0 else "positive")
```

## Python-Specific Features

| Prolog | Python |
|--------|--------|
| `X > 0, X < 10` | `arg1 > 0 and arg1 < 10` |
| `X =:= 0` | `arg1 == 0` |
| `X =\= 0` | `arg1 != 0` |
| `R is abs(X)` | `abs(arg1)` |
| `R is X mod 2` | `(arg1 % 2)` |
| `(C -> T ; E)` | `T if C else E` (ternary) |
| No match | `raise ValueError("...")` |

## Verified Output

```bash
$ python3 -c "
def classify(arg1):
    if arg1 > 0 and arg1 < 10:
        return 'small'
    elif arg1 >= 10:
        return 'large'
    else:
        raise ValueError('No matching clause')

print(classify(5))    # small
print(classify(25))   # large
"
small
large
```

## Summary

- Multi-clause Prolog predicates compile to Python `if`/`elif`/`else`
- Prolog `(-> ;)` becomes Python ternary `x if cond else y`
- `raise ValueError` for unmatched clauses
- `abs()` maps to Python's built-in `abs()`
- Generated code runs directly with `python3`

---

## Navigation

**←** [Previous: Chapter 5: Semantic Predicates](05_semantic_predicates) | [Book 5: Python Target](./)
