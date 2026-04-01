<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 5: Native Clause Body Lowering

Write natural Prolog predicates and let UnifyWeaver automatically compile them into idiomatic Rust functions with `if`/`else if`/`else` chains. No special syntax required.

## How It Works

Given multi-clause Prolog predicates with guard conditions:

```prolog
classify(X, small) :- X > 0, X < 10.
classify(X, large) :- X >= 10.
```

UnifyWeaver generates:

```rust
fn classify(arg1: i64) -> &'static str {
    if arg1 > 0 && arg1 < 10 {
        "small"
    } else if arg1 >= 10 {
        "large"
    } else {
        panic!("No matching clause for classify/2")
    }
}
```

The compiler detects:
- **Guard conditions** in each clause body (comparisons like `X > 0`)
- **Output value** in the last head argument (`small`, `large`)
- Maps these to Rust's expression-based `if`/`else` blocks

## Basic Examples

### Multi-Clause Classification

```prolog
grade(X, low)  :- X < 50.
grade(X, mid)  :- X >= 50, X < 80.
grade(X, high) :- X >= 80.

?- compile_predicate_to_rust(grade/2, [], Code).
```

```rust
fn grade(arg1: i64) -> &'static str {
    if arg1 < 50 {
        "low"
    } else if arg1 >= 50 && arg1 < 80 {
        "mid"
    } else if arg1 >= 80 {
        "high"
    } else {
        panic!("No matching clause for grade/2")
    }
}
```

### Arithmetic Output

```prolog
double(X, R) :- R is X * 2.
```

```rust
fn double(arg1: i64) -> i64 {
    (arg1 * 2)
}
```

Rust uses expression-based returns — the last expression in the block is the return value, no `return` keyword needed.

## If-Then-Else

Prolog's `(Cond -> Then ; Else)` compiles to Rust's `if`/`else`:

```prolog
abs_val(X, R) :- (X >= 0 -> R = X ; R is -X).
```

```rust
fn abs_val(arg1: i64) -> i64 {
    if arg1 >= 0 { arg1 } else { (-arg1) }
}
```

### Nested If-Then-Else

```prolog
range_classify(X, R) :-
    (X < 0 -> R = negative
    ; (X =:= 0 -> R = zero
    ; R = positive)).
```

```rust
fn range_classify(arg1: i64) -> &'static str {
    if arg1 < 0 {
        "negative"
    } else {
        if arg1 == 0 { "zero" } else { "positive" }
    }
}
```

## Rust-Specific Features

| Prolog | Rust |
|--------|------|
| `X > 0, X < 10` | `arg1 > 0 && arg1 < 10` |
| `X =:= 0` | `arg1 == 0` |
| `X =\= 0` | `arg1 != 0` |
| `R is X * 2` | `(arg1 * 2)` |
| `R is abs(X)` | `arg1.abs()` |
| `R is X mod 2` | `(arg1 % 2)` |
| No match | `panic!("...")` |

Rust's expression-based semantics mean the generated code is concise — no explicit `return` statements for the happy path.

## Compiling and Running

```bash
# Generate
swipl -g "
    use_module('src/unifyweaver/targets/rust_target'),
    assert(user:(classify(X, small) :- X > 0, X < 10)),
    assert(user:(classify(X, large) :- X >= 10)),
    compile_predicate_to_rust(classify/2, [], Code),
    write_to_file('classify.rs', Code)
" -t halt

# Compile and run
rustc classify.rs -o classify
./classify
```

Verified output: `classify(5)` returns `"small"`, `classify(25)` returns `"large"`.

## Summary

- Multi-clause Prolog predicates compile to Rust `if`/`else if`/`else` chains
- Expression-based Rust means clean, return-free code
- Prolog `(-> ;)` becomes Rust `if {} else {}`
- `panic!()` for unmatched clauses follows Rust conventions
- Generated code compiles with `rustc` without modification

---

## Navigation

**←** [Previous: Chapter 4: Advanced Features](04_advanced_features) | [Book 9: Rust Target](./)
