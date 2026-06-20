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

## What Python Native Lowering Avoids

Python is easiest to understand when the predicate can become ordinary code. A
classification predicate, arithmetic helper, or generator-style recursive
search can often stay in Python-native form. That avoids carrying a full WAM
state object through every step.

The boundary appears when the source program needs behavior that native Python
lowering does not naturally provide:

| Requirement | Native Python pressure | WAM-backed answer |
|---|---|---|
| Open-ended backtracking | hard to express with one return value | generator/runtime choice points |
| Deep unification | ad hoc recursive matching grows complex | WAM unifier and trail |
| Runtime term parsing | Python needs a Prolog parser | compiled or native parser path |
| Cross-target parity | Python idioms may drift | shared WAM item contract |

So Python-native lowering and Hybrid WAM are not competitors. Native lowering
is the simple path when the predicate shape allows it. WAM-backed execution is
the path when the program needs full logic semantics or parser behavior.

## Hybrid WAM Boundary

Python's native target material is best understood as the high-level side of
the Hybrid WAM boundary. Many examples can stay as procedural or generator
code. When a workload needs full WAM semantics or a runtime parser, Book 17's
model explains the alternate path.

- Default generation path in this book: Python-native lowering where the
  predicate shape is simple enough.
- Symbolic WAM text: useful for debugging and parser-related tests when using
  WAM-backed Python paths.
- Target-specific emphasis: readability, generator-style search, and clear
  separation between native Python lowering and WAM-backed execution.

## Summary

- Multi-clause Prolog predicates compile to Python `if`/`elif`/`else`
- Prolog `(-> ;)` becomes Python ternary `x if cond else y`
- `raise ValueError` for unmatched clauses
- `abs()` maps to Python's built-in `abs()`
- Generated code runs directly with `python3`

---

## Navigation

**←** [Previous: Chapter 5: Semantic Predicates](05_semantic_predicates) | [Book 5: Python Target](./)
