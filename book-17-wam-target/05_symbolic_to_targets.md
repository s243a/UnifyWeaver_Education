<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 5: Symbolic WAM, Item-by-Item, Across Targets

The earlier chapters used symbolic WAM text because it is readable. This chapter
makes the translation concrete: it lines up the *same* symbolic instruction
against the actual item shape each target emits. The goal is that when you read a
generated file later, you can recognize each line as "oh, that is just
`get_constant alice, A1`."

These tables are not invented sketches. They mirror the real instruction
literals produced by the target emitters
(`wam_rust_target.pl`, `wam_go_target.pl`, `wam_cpp_target.pl`) and the runtime
type definitions shipped in the templates
(`templates/targets/rust_wam/*.mustache`). Where a target makes a different
representational choice, that difference is called out rather than smoothed over.

## The Running Example

We compile a tiny two-clause fact base plus one rule:

```prolog
parent(alice, bob).
parent(bob, charlie).

grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

The symbolic WAM for `parent/2` (see Chapter 3) is:

```wam
parent/2:
    try_me_else L_parent_2_2
    get_constant alice, A1
    get_constant bob, A2
    proceed
L_parent_2_2:
    trust_me
    get_constant bob, A1
    get_constant charlie, A2
    proceed
```

Every target consumes the *same* item stream behind that listing. What differs
is how each target spells the items and what it does with them at runtime.

## One Shared Decision: How Registers Are Named

Before the per-target tables, one cross-cutting fact explains most of the
surface differences below.

The symbolic notation writes registers as `A1`, `X1`, `Y1`. Internally every
target uses the **same flat encoding**:

| Register class | Meaning | Index |
|---|---|---|
| `A1, A2, …` | argument registers | `0, 1, …` |
| `X1, X2, …` | temporary registers | `100, 101, …` |
| `Y1, Y2, …` | permanent (env-frame) variables | `200, 201, …` |

The Rust runtime stores them in one `Vec<Value>` with exactly this layout
(`templates/targets/rust_wam/state.rs.mustache`), and the Go emitter resolves
`A1 → 0`, `X1 → 100`, `Y1 → 200` with `go_reg_index/2`. The only question each
target answers differently is *when* the name becomes an index: Go bakes the
integer into the emitted item, while Rust and C++ keep the string name in the
item and resolve it at runtime.

## Target Table: Rust (`Instruction` enum)

Rust emits a `Vec<Instruction>` where each variant carries a typed `Value` and
string register names. The enum lives in
`templates/targets/rust_wam/instructions.rs.mustache`; the `Value` universe is
in `value.rs.mustache`.

| Symbolic WAM | Rust item literal | Enum signature |
|---|---|---|
| `get_constant alice, A1` | `Instruction::GetConstant(Value::Atom("alice".into()), "A1")` | `GetConstant(Value, String)` |
| `get_variable X1, A1` | `Instruction::GetVariable("X1", "A1")` | `GetVariable(String, String)` |
| `get_value X1, A2` | `Instruction::GetValue("X1", "A2")` | `GetValue(String, String)` |
| `put_value Y1, A1` | `Instruction::PutValue("Y1", "A1")` | `PutValue(String, String)` |
| `put_variable Y1, A2` | `Instruction::PutVariable("Y1", "A2")` | `PutVariable(String, String)` |
| `call parent/2, 2` | `Instruction::Call("parent/2", 2)` | `Call(String, usize)` |
| `execute parent/2` | `Instruction::Execute("parent/2")` | `Execute(String)` |
| `proceed` | `Instruction::Proceed` | `Proceed` |
| `try_me_else L2` | `Instruction::TryMeElse("L2")` | `TryMeElse(String)` |
| `trust_me` | `Instruction::TrustMe` | `TrustMe` |

The carried `Value` is the same enum Prolog terms deref to at runtime:

```rust
pub enum Value {
    Atom(String),
    Integer(i64),
    Float(f64),
    Str(String, Vec<Value>),   // compound term: functor + args
    List(Vec<Value>),
    Ref(usize),                // heap reference
    Unbound(String),           // unbound variable, named (not an interned id)
    Bool(bool),
    Uninit,                    // empty register slot sentinel
}
```

Note that an unbound variable is `Unbound(String)` — a *named* hole, not an
interned integer. (There is a separate `u32` atom-interning layer used only by
the LMDB/FFI fast path; ordinary WAM values stay string-named so dumps stay
readable.)

## Target Table: Go (struct items, integer registers)

Go emits a `[]Instruction` of small structs implementing an interface. The
register name is already resolved to its integer index in the emitted literal.

| Symbolic WAM | Go item literal |
|---|---|
| `get_constant alice, A1` | `&GetConstant{C: &Atom{Name: "alice"}, Ai: 0}` |
| `get_variable X1, A1` | `&GetVariable{Xn: 100, Ai: 0}` |
| `get_value X1, A2` | `&GetValue{Xn: 100, Ai: 1}` |
| `get_structure pair/2, A1` | `&GetStructure{Functor: "pair/2", Ai: 0}` |
| `get_list A1` | `&GetList{Ai: 0}` |

The Go runtime then dispatches on the concrete struct type. Because the index is
baked in, the Go interpreter never has to parse `"A1"` — it indexes
`vm.Regs[i.Ai]` directly.

When the project is built through `write_wam_go_project/3`, atom literals are
additionally interned: instead of the inline `&Atom{Name: "alice"}` shown above,
the emitter references a shared package variable such as `wamAtom_alice` so every
occurrence of the atom is pointer-equal. The inline form is what you get from
ad-hoc bytecode-to-Go conversion (e.g. in tests).

## Target Table: C++ (`Instruction` factory, string registers)

C++ builds instructions through static factory functions on an `Instruction`
struct with an `Op` enum tag. Like Rust, it keeps register names as strings.

| Symbolic WAM | C++ item literal |
|---|---|
| `get_constant alice, A1` | `Instruction::GetConstant(Value::Atom("alice"), "A1")` |
| `get_variable X1, A1` | `Instruction::GetVariable("X1", "A1")` |
| `get_value X1, A2` | `Instruction::GetValue("X1", "A2")` |
| `proceed` | `Instruction::Proceed()` |

## Reading the Differences

Putting the three side by side for the single line `get_constant alice, A1`:

| Aspect | Rust | Go | C++ |
|---|---|---|---|
| Item carrier | enum variant | interface struct | struct + `Op` tag |
| Register form | string `"A1"` | integer `0` | string `"A1"` |
| Constant form | `Value::Atom(...)` | `&Atom{Name: "alice"}` | `Value::Atom(...)` |
| Resolves register | at runtime | at emit time | at runtime |

None of these change the *semantics*. Each item, however spelled, means: read
argument register `A1`; if it already holds `alice`, advance; if it is unbound,
bind it to `alice` and trail the binding; otherwise fail and let the choice
point try the next clause. The shared contract — registers, unification, choice
points, trailing — is what makes the symbolic listing a faithful description of
all three.

## From Item to Behavior (Rust, one instruction in full)

To close the loop, here is the real runtime body the Rust target generates for
`GetConstant` (from the `wam_instruction_arm` rules in `wam_rust_target.pl`):

```rust
Instruction::GetConstant(c, ai) => {
    let raw_val = self.get_reg_raw(ai);
    let val = raw_val.map(|v| self.deref_var(&v));
    match val {
        Some(v) if v == *c => { self.pc += 1; true }          // already equal
        Some(Value::Unbound(ref var_name)) => {               // bind + trail
            self.trail_binding(ai);
            self.set_reg_str(ai, c.clone());
            self.bind_var(var_name, c.clone());
            self.pc += 1;
            true
        }
        _ => false,                                            // mismatch -> backtrack
    }
}
```

This is exactly the three-way behavior described above: equal, bind, or fail.
The symbolic `get_constant alice, A1` is a one-line shorthand for this match.

## What To Carry Into the Target Books

- The symbolic listing is a faithful, target-neutral description; each target
  book shows only its own item spelling and runtime.
- Register naming (`A1`/`X1`/`Y1`) is shared; the index encoding
  (`0`/`100`/`200`) is shared; only the *timing* of name-to-index resolution
  differs.
- A value is a `Value`/term, not a raw blob. Each target picks a concrete
  representation but must preserve atom/integer/compound/unbound distinctions so
  unification behaves identically.

The next step is target-specific. See each target book for how it generates,
builds, and runs hybrid WAM output — and for the escape hatches (lowered
helpers, external fact sources, FFI, kernels) introduced in
[Chapter 4](04_fallback_hub.md).
