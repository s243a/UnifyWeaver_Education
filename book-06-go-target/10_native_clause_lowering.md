<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 10: Native Clause Body Lowering

Write natural Prolog predicates and let UnifyWeaver compile them into Go functions with `if`/`else if`/`else` chains.

## How It Works

```prolog
classify(X, small) :- X > 0, X < 10.
classify(X, large) :- X >= 10.
```

Generates:

```go
package main

import "fmt"

func classify(arg1 interface{}) interface{} {
    if arg1 > 0 && arg1 < 10 {
        return "small"
    } else if arg1 >= 10 {
        return "large"
    } else {
        panic("No matching clause for classify/2")
    }
}

func main() {
    fmt.Println(classify(0))
}
```

## Basic Examples

### Three-Way Classification

```prolog
grade(X, low)  :- X < 50.
grade(X, mid)  :- X >= 50, X < 80.
grade(X, high) :- X >= 80.
```

```go
func grade(arg1 interface{}) interface{} {
    if arg1 < 50 {
        return "low"
    } else if arg1 >= 50 && arg1 < 80 {
        return "mid"
    } else if arg1 >= 80 {
        return "high"
    } else {
        panic("No matching clause for grade/2")
    }
}
```

### Arithmetic

```prolog
double(X, R) :- R is X * 2.
```

```go
func double(arg1 interface{}) interface{} {
    return (arg1 * 2)
}
```

## If-Then-Else

```prolog
abs_val(X, R) :- (X >= 0 -> R = X ; R is -X).
```

```go
func abs_val(arg1 interface{}) interface{} {
    if arg1 >= 0 {
        return arg1
    } else {
        return (-arg1)
    }
}
```

Go doesn't have a ternary operator, so all if-then-else patterns use full `if`/`else` blocks with `return`.

## Go-Specific Features

| Prolog | Go |
|--------|-----|
| `X > 0, X < 10` | `arg1 > 0 && arg1 < 10` |
| `X =:= 0` | `arg1 == 0` |
| `X =\= 0` | `arg1 != 0` |
| `R is abs(X)` | Uses `math.Abs()` |
| `R is X mod 2` | `arg1 % 2` |
| No match | `panic("...")` |
| Return type | `interface{}` (generic) |

## Verified Output

```bash
# classify(5) → "small", classify(25) → "large"
```

## Summary

- Multi-clause predicates compile to Go `if`/`else if`/`else`
- Uses `interface{}` types for generic input/output
- `panic()` for unmatched clauses follows Go conventions
- No ternary — all conditionals use explicit `if`/`else` blocks

---

## Navigation

**←** [Previous: Chapter 9](09_pipeline_testing) | [Book 6: Go Target](./)
