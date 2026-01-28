<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Basic Compilation - Questions

Q&A companion for [02_basic_compilation_impl.md](./02_basic_compilation_impl.md).

---

## Question Index

1. [What does compile_predicate_to_rust/3 do?](#b09c02-q-compile-predicate)
2. [How are facts compiled to Rust?](#b09c02-q-facts)
3. [What is compile_facts_to_rust/3?](#b09c02-q-compile-facts)
4. [What helper functions are generated?](#b09c02-q-helper-functions)
5. [How are rules compiled to Rust?](#b09c02-q-rules)
6. [What recursion patterns are supported?](#b09c02-q-recursion-patterns)
7. [How does tail recursion compilation work?](#b09c02-q-tail-recursion)
8. [How does linear recursion use memoization?](#b09c02-q-linear-recursion)
9. [How does mutual recursion work?](#b09c02-q-mutual-recursion)
10. [What is the build workflow?](#b09c02-q-workflow)

---

## Questions and Answers

### <a id="b09c02-q-compile-predicate"></a>Q1: What does compile_predicate_to_rust/3 do?

**Answer**: `compile_predicate_to_rust/3` compiles a Prolog predicate to Rust source code:

```prolog
compile_predicate_to_rust(+Predicate/Arity, +Options, -Code)
```

For facts, it generates a `HashSet`. For rules, it generates a stdin stream processor.

**See**: [compile_predicate_to_rust/3](./02_basic_compilation_impl.md#compile_predicate_to_rust3)

---

### <a id="b09c02-q-facts"></a>Q2: How are facts compiled to Rust?

**Answer**: Facts become a `HashSet<&str>`:

```prolog
user(john, 25).
user(jane, 30).
```

Generates:
```rust
let mut facts = HashSet::new();
facts.insert("john:25");
facts.insert("jane:30");
```

**See**: [compile_predicate_to_rust/3](./02_basic_compilation_impl.md#compile_predicate_to_rust3)

---

### <a id="b09c02-q-compile-facts"></a>Q3: What is compile_facts_to_rust/3?

**Answer**: `compile_facts_to_rust/3` generates struct-based Rust code:

```prolog
rust_target:compile_facts_to_rust(user, 2, Code)
```

Creates:
- `struct USER { arg1: String, arg2: String }`
- Derives: `Debug`, `Clone`, `PartialEq`, `Eq`
- Helper functions: `get_all`, `stream`, `contains`

**See**: [compile_facts_to_rust/3](./02_basic_compilation_impl.md#compile_facts_to_rust3)

---

### <a id="b09c02-q-helper-functions"></a>Q4: What helper functions are generated?

**Answer**: Three helper functions:

1. `get_all_foo() -> Vec<FOO>` - Returns all facts as a vector
2. `stream_foo() -> impl Iterator<Item = FOO>` - Lazy iterator
3. `contains_foo(target: &FOO) -> bool` - Membership test

These work with Rust's ownership system.

**See**: [Generated Structure](./02_basic_compilation_impl.md#generated-structure)

---

### <a id="b09c02-q-rules"></a>Q5: How are rules compiled to Rust?

**Answer**: Rules become stdin stream processors:

```prolog
adult(Name, Age) :- person(Name, Age), Age >= 18.
```

Generates:
```rust
for line in stdin.lock().lines() {
    let parts: Vec<&str> = line.split(':').collect();
    let age: i32 = parts[1].parse().unwrap_or(0);
    if age >= 18 {
        println!("{}:{}", parts[0], age);
    }
}
```

Usage: `echo "alice:25" | ./adult`

**See**: [Compiling Rules](./02_basic_compilation_impl.md#compiling-rules)

---

### <a id="b09c02-q-recursion-patterns"></a>Q6: What recursion patterns are supported?

**Answer**: Three patterns:

| Pattern | API | Generated Rust |
|---------|-----|----------------|
| Tail Recursion | `compile_tail_recursion_rust/3` | for loop |
| Linear Recursion | `compile_linear_recursion_rust/3` | HashMap memo |
| Mutual Recursion | `compile_mutual_recursion_rust/3` | thread_local! HashMap |

**See**: [Recursion Pattern Summary](./02_basic_compilation_impl.md#recursion-pattern-summary)

---

### <a id="b09c02-q-tail-recursion"></a>Q7: How does tail recursion compilation work?

**Answer**: Tail recursion (accumulator pattern) compiles to a for loop with O(1) stack:

```prolog
sum_list([], Acc, Acc).
sum_list([H|T], Acc, S) :- Acc1 is Acc + H, sum_list(T, Acc1, S).
```

Generates:
```rust
fn sum_list(items: &[i32], acc: i32) -> i32 {
    let mut result = acc;
    for &item in items {
        result += item;
    }
    result
}
```

**See**: [Tail Recursion](./02_basic_compilation_impl.md#tail-recursion-compile_tail_recursion_rust3)

---

### <a id="b09c02-q-linear-recursion"></a>Q8: How does linear recursion use memoization?

**Answer**: Linear recursion with overlapping subproblems uses `thread_local!` HashMap:

```rust
thread_local! {
    static TRIANGULAR_MEMO: RefCell<HashMap<i32, i32>> = RefCell::new(HashMap::new());
}

fn triangular(n: i32) -> i32 {
    // Check memo first
    if let Some(result) = TRIANGULAR_MEMO.with(|m| m.borrow().get(&n).copied()) {
        return result;
    }
    // Compute and store in memo
    ...
}
```

This avoids redundant computation.

**See**: [Linear Recursion](./02_basic_compilation_impl.md#linear-recursion-compile_linear_recursion_rust3)

---

### <a id="b09c02-q-mutual-recursion"></a>Q9: How does mutual recursion work?

**Answer**: Predicates calling each other share a memoization table:

```prolog
is_even(0).
is_even(N) :- N > 0, N1 is N - 1, is_odd(N1).
is_odd(1).
is_odd(N) :- N > 1, N1 is N - 1, is_even(N1).
```

Generates a shared `thread_local!` HashMap with keys like `"is_even:5"` and `"is_odd:4"`.

**See**: [Mutual Recursion](./02_basic_compilation_impl.md#mutual-recursion-compile_mutual_recursion_rust3)

---

### <a id="b09c02-q-workflow"></a>Q10: What is the build workflow?

**Answer**: Three steps:

```bash
# 1. Generate Rust code
swipl -g compile_facts -t halt facts.pl

# 2. Compile with rustc
rustc user.rs

# 3. Run
./user
```

For larger projects, use `cargo` and project generation (see Book 9 Chapter 3).

**See**: [Build Workflow](./02_basic_compilation_impl.md#build-workflow)

---

## Summary

Rust target compilation provides:
- HashSet-based fact storage
- Struct-based export with helpers
- Stdin stream processing for rules
- Three recursion patterns with memoization
- Integration with Rust's type system
