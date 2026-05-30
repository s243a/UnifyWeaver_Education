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

## How Rust Makes Hybrid WAM Concrete

Rust forces the target to be explicit about ownership and storage. A WAM value
cannot be an informal blob; it needs a representation such as an enum for
atoms, integers, variables, lists, and compounds. Runtime state must describe
who owns registers, heap cells, trail entries, choice points, and external fact
source handles.

These are not hypothetical types — they ship as Mustache templates under
`templates/targets/rust_wam/`. The value universe (`value.rs.mustache`,
abridged) is:

```rust
pub enum Value {
    Atom(String),
    Integer(i64),
    Float(f64),
    Str(String, Vec<Value>),   // compound term: functor + args
    List(Vec<Value>),
    Ref(usize),                // heap reference
    Unbound(String),           // unbound variable — a named hole, not an id
    Bool(bool),
    Uninit,                    // empty register-slot sentinel
}
```

An unbound variable is `Unbound(String)`, not an interned integer, so debug
dumps stay readable. (A separate `u32` atom-interning layer — `atom_intern` /
`ffi_facts` in the state — exists *only* for the LMDB/FFI fast path; ordinary
WAM terms keep their string names.)

The runtime state (`state.rs.mustache`, heavily abridged — the real struct has
~30 fields) makes ownership explicit:

```rust
pub struct WamState {
    pub pc: usize,
    pub regs: Vec<Value>,          // A1->0.., X1->100.., Y1->200..
    pub stack: Arc<Vec<StackEntry>>,   // Arc so WamState: Send for Rayon
    pub heap: Vec<Value>,
    pub trail: Vec<TrailEntry>,
    pub choice_points: Vec<ChoicePoint>,
    pub bindings: HashMap<String, Value>,
    // ... indexed_atom_fact2, ffi_facts, lazy_lookups, foreign_predicates, ...
}
```

The register file is a single `Vec<Value>` with the flat encoding shared across
targets (argument registers at `0..`, temporaries at `100..`, permanents at
`200..` — see [Book 17, Chapter 5](../book-17-wam-target/05_symbolic_to_targets.md)).
Wrapping the stack in `Arc` is a concrete ownership decision: it gives O(1)
choice-point clones via copy-on-write and keeps `WamState: Send` so queries can
run under Rayon.

Hybrid lowering decides which pieces stay in generic WAM state and which can
become Rust-native structures. A fact-only predicate may become an indexed
store. A graph predicate may become a kernel call. A deterministic helper may
be emitted as Rust code that manipulates `WamState` directly.

| Predicate shape | Rust-friendly lowering | Why |
|---|---|---|
| Many static facts | `HashMap`/`BTreeMap`/LMDB fact source | Keeps lookup cost tied to bound arguments. |
| Deterministic arithmetic | direct helper function | Avoids instruction dispatch overhead. |
| Recursive graph search | native kernel through WAM FFI | Uses optimized Rust algorithm while preserving logic results. |
| Open-ended backtracking | WAM runtime path | Keeps complete Prolog search semantics. |

The foreign-call boundary is also ownership-sensitive. The runtime must convert
register values into host inputs, call the Rust function or kernel, then unify
returned values with WAM variables. That unification step is what keeps a Rust
kernel from becoming an unrelated side channel.

## What the Generated Hybrid WAM Output Looks Like

When a predicate keeps the full WAM runtime path (rather than being lowered to a
plain function), `write_wam_rust_project/3` emits the program as a
`Vec<Instruction>`. The two-clause fact base

```prolog
parent(alice, bob).
parent(bob, charlie).
```

becomes an instruction vector that mirrors the symbolic listing one-to-one:

```rust
// parent/2
vec![
    Instruction::TryMeElse("L_parent_2_2"),
    Instruction::GetConstant(Value::Atom("alice".into()), "A1"),
    Instruction::GetConstant(Value::Atom("bob".into()),   "A2"),
    Instruction::Proceed,
    // L_parent_2_2:
    Instruction::TrustMe,
    Instruction::GetConstant(Value::Atom("bob".into()),     "A1"),
    Instruction::GetConstant(Value::Atom("charlie".into()), "A2"),
    Instruction::Proceed,
]
```

Each variant is the real signature from `instructions.rs.mustache`
(`GetConstant(Value, String)`, `TryMeElse(String)`, `Proceed`). Register names
stay as strings and are resolved to indices at runtime.

The interpreter dispatches on these variants. The generated body for
`GetConstant` (emitted by the `wam_instruction_arm` rules in
`wam_rust_target.pl`) is the three-way unify-a-constant logic:

```rust
Instruction::GetConstant(c, ai) => {
    let raw_val = self.get_reg_raw(ai);
    let val = raw_val.map(|v| self.deref_var(&v));
    match val {
        Some(v) if v == *c => { self.pc += 1; true }      // already equal: advance
        Some(Value::Unbound(ref var_name)) => {           // unbound: bind + trail
            self.trail_binding(ai);
            self.set_reg_str(ai, c.clone());
            self.bind_var(var_name, c.clone());
            self.pc += 1;
            true
        }
        _ => false,                                       // mismatch: backtrack
    }
}
```

Reading the generated file, that is the whole trick: each `Instruction::*` arm
is a small, self-contained step over `WamState`, and `false` is what hands
control to the most recent choice point. If you can read this arm, you can read
the rest — they all follow the same shape.

For the kernel/FFI path, the same project also emits the `Value`, `WamState`,
fact-source, and (when `lmdb_materialisation` is enabled) LMDB modules from the
templates above, so a recursive graph predicate can be backed by an indexed or
LMDB fact source instead of inlined facts.

## Hybrid WAM Role

Rust should be read as the systems target where Hybrid WAM choices become
explicit: ownership, storage layout, external fact sources, materialization,
and native kernels all have to be represented safely. Book 17 covers the
shared architecture; this chapter focuses on why Rust-specific lowering is a
good fit for memory-safe WAM state and host-native acceleration.

- Default generation path: structured WAM items or target-ready WAM data
  should feed Rust code generation directly.
- Symbolic WAM text: useful for explanations, debug dumps, and tests, but not
  the preferred compiler transport.
- Target-specific emphasis: ownership-aware WAM state, LMDB or other external
  fact sources, indexed lookup, and FFI/kernel dispatch.

## Summary

- Multi-clause Prolog predicates compile to Rust `if`/`else if`/`else` chains
- Expression-based Rust means clean, return-free code
- Prolog `(-> ;)` becomes Rust `if {} else {}`
- `panic!()` for unmatched clauses follows Rust conventions
- Generated code compiles with `rustc` without modification

---

## Navigation

**←** [Previous: Chapter 4: Advanced Features](04_advanced_features) | [Book 9: Rust Target](./)
