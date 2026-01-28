<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Basic Compilation - Implementation Details

This document provides function-level documentation for Rust target compilation.

**Source**: `src/unifyweaver/targets/rust_target.pl`

---

## compile_predicate_to_rust/3

Compiles a Prolog predicate to Rust code.

### Signature

```prolog
compile_predicate_to_rust(+Predicate/Arity, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicate/Arity` | `atom/integer` | Predicate to compile |
| `Options` | `list` | Compilation options |
| `Code` | `string` | Generated Rust source |

### Options

| Option | Values | Default | Description |
|--------|--------|---------|-------------|
| `field_delimiter(D)` | `colon`, `tab`, `comma` | `colon` | Output delimiter |

### Example: Facts

```prolog
user(john, 25).
user(jane, 30).

?- compile_predicate_to_rust(user/2, [], Code).
```

**Generated Rust**:
```rust
use std::collections::HashSet;

fn main() {
    let mut facts = HashSet::new();
    facts.insert("john:25");
    facts.insert("jane:30");
    for fact in &facts {
        println!("{}", fact);
    }
}
```

---

## compile_facts_to_rust/3

Generates struct-based Rust code with helper functions.

### Signature

```prolog
rust_target:compile_facts_to_rust(+Predicate, +Arity, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicate` | `atom` | Predicate name |
| `Arity` | `integer` | Number of arguments |
| `Code` | `string` | Generated Rust source |

### Generated Structure

For predicate `foo/N`:

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
struct FOO {
    arg1: String,
    arg2: String,
    // ... argN
}

fn get_all_foo() -> Vec<FOO> { ... }
fn stream_foo() -> impl Iterator<Item = FOO> { ... }
fn contains_foo(target: &FOO) -> bool { ... }
```

### Example

```prolog
user(john, 25).
user(jane, 30).

?- rust_target:compile_facts_to_rust(user, 2, Code).
```

**Generated Rust**:
```rust
#[derive(Debug, Clone, PartialEq, Eq)]
struct USER {
    arg1: String,
    arg2: String,
}

fn get_all_user() -> Vec<USER> {
    vec![
        USER { arg1: "john".to_string(), arg2: "25".to_string() },
        USER { arg1: "jane".to_string(), arg2: "30".to_string() },
    ]
}

fn stream_user() -> impl Iterator<Item = USER> {
    get_all_user().into_iter()
}

fn contains_user(target: &USER) -> bool {
    get_all_user().iter().any(|f| f == target)
}
```

### Benefits

- Derives `Debug`, `Clone`, `PartialEq`, `Eq`
- Type-safe struct field access
- Works with Rust's ownership system
- IDE autocompletion support

---

## Compiling Rules

Rules become stream processors reading from stdin.

### Example

```prolog
adult(Name, Age) :- person(Name, Age), Age >= 18.

?- compile_predicate_to_rust(adult/2, [field_delimiter(colon)], Code).
```

**Generated Rust**:
```rust
use std::io::{self, BufRead};

fn main() {
    let stdin = io::stdin();
    for line in stdin.lock().lines() {
        let line = line.unwrap();
        let parts: Vec<&str> = line.split(':').collect();
        if parts.len() >= 2 {
            let name = parts[0];
            let age: i32 = parts[1].parse().unwrap_or(0);
            if age >= 18 {
                println!("{}:{}", name, age);
            }
        }
    }
}
```

### Usage

```bash
echo -e "alice:25\nbob:10" | ./adult
# Output: alice:25
```

---

## Recursion Patterns

### Tail Recursion: compile_tail_recursion_rust/3

For accumulator patterns, generates O(1) stack code.

```prolog
sum_list([], Acc, Acc).
sum_list([H|T], Acc, S) :- Acc1 is Acc + H, sum_list(T, Acc1, S).

?- rust_target:compile_tail_recursion_rust(sum_list/3, [], Code).
```

**Generated Rust**:
```rust
fn sum_list(items: &[i32], acc: i32) -> i32 {
    let mut result = acc;
    for &item in items {
        result += item;
    }
    result
}
```

### Linear Recursion: compile_linear_recursion_rust/3

For overlapping subproblems, generates HashMap memoization.

```prolog
triangular(0, 0).
triangular(1, 1).
triangular(N, F) :- N > 1, N1 is N - 1, triangular(N1, F1), F is F1 + N.

?- rust_target:compile_linear_recursion_rust(triangular/2, [], Code).
```

**Generated Rust**:
```rust
use std::cell::RefCell;
use std::collections::HashMap;

thread_local! {
    static TRIANGULAR_MEMO: RefCell<HashMap<i32, i32>> = RefCell::new(HashMap::new());
}

fn triangular(n: i32) -> i32 {
    if let Some(result) = TRIANGULAR_MEMO.with(|m| m.borrow().get(&n).copied()) {
        return result;
    }

    let result = if n == 0 {
        0
    } else if n == 1 {
        1
    } else {
        triangular(n - 1) + n
    };

    TRIANGULAR_MEMO.with(|m| m.borrow_mut().insert(n, result));
    result
}
```

### Mutual Recursion: compile_mutual_recursion_rust/3

For predicates calling each other, generates shared memoization.

```prolog
is_even(0).
is_even(N) :- N > 0, N1 is N - 1, is_odd(N1).
is_odd(1).
is_odd(N) :- N > 1, N1 is N - 1, is_even(N1).

?- rust_target:compile_mutual_recursion_rust([is_even/1, is_odd/1], [], Code).
```

**Generated Rust**:
```rust
thread_local! {
    static IS_EVEN_IS_ODD_MEMO: RefCell<HashMap<String, bool>> = RefCell::new(HashMap::new());
}

fn is_even(n: i32) -> bool {
    let key = format!("is_even:{}", n);
    if let Some(result) = IS_EVEN_IS_ODD_MEMO.with(|m| m.borrow().get(&key).copied()) {
        return result;
    }

    let result = if n == 0 {
        true
    } else if n > 0 {
        is_odd(n - 1)
    } else {
        false
    };

    IS_EVEN_IS_ODD_MEMO.with(|m| m.borrow_mut().insert(key, result));
    result
}

fn is_odd(n: i32) -> bool {
    // Similar structure...
}
```

---

## Recursion Pattern Summary

| Pattern | API | Generated Rust |
|---------|-----|----------------|
| Tail Recursion | `compile_tail_recursion_rust/3` | for loop |
| Linear Recursion | `compile_linear_recursion_rust/3` | HashMap memo |
| Mutual Recursion | `compile_mutual_recursion_rust/3` | thread_local! HashMap |

---

## write_rust_program/2

Writes generated code to a file.

### Signature

```prolog
write_rust_program(+Code, +Filename)
```

### Example

```prolog
?- compile_predicate_to_rust(user/2, [], Code),
   write_rust_program(Code, 'user.rs').
```

---

## Build Workflow

```bash
# 1. Generate Rust code
swipl -g compile_facts -t halt facts.pl

# 2. Compile with rustc
rustc user.rs

# 3. Run
./user
```

For larger projects, use `cargo` (see Book 9 Chapter 3).

---

## Related Documentation

- [Book 9 Chapter 1: Introduction](../01_introduction.md)
- [Book 9 Chapter 3: Project Generation](../03_project_generation.md)
- [Rust Target Source](../../../../src/unifyweaver/targets/rust_target.pl)
