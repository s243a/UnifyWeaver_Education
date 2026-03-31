# Book 17: WAM Target

The WAM (Warren Abstract Machine) target serves as the universal low-level fallback for UnifyWeaver. It implements the classic Prolog execution model using a symbolic instruction set.

## Why WAM?

While other targets like Python, Go, and **TypR** (our specialized Typed R target) use "native lowering" to map Prolog clauses to idiomatic language constructs, some Prolog features are difficult to lower efficiently:

- **Non-deterministic choice points**: back-to-back fact matching or rule selection.
- **Deep structure unification**: matching complex nested terms.
- **Mutual recursion**: complex inter-dependent predicates.

The WAM target provides a robust fallback by compiling these features into low-level instructions that preserve Prolog's semantics perfectly.

## Instruction Set

The WAM target uses a symbolic instruction set based on the original Warren Abstract Machine:

| Category | Instructions |
|----------|--------------|
| **Head Unification** | `get_variable`, `get_value`, `get_constant`, `get_structure`, `unify_variable`, `unify_constant` |
| **Body Construction** | `put_variable`, `put_value`, `put_constant`, `put_structure`, `set_variable`, `set_value`, `set_constant` |
| **Control Flow** | `allocate`, `deallocate`, `call`, `execute`, `proceed` |
| **Choice Points** | `try_me_else`, `retry_me_else`, `trust_me` |

## Usage

```prolog
?- use_module('src/unifyweaver/targets/wam_target').
?- wam_target:compile_predicate_to_wam(user:ancestor/2, [], Code).
```

## Chapters

1. [Introduction to WAM](01_introduction.md)
2. [Instruction Set Architecture](02_isa.md)
3. [Compiling Rules and Recursion](03_compilation.md)
4. [WAM as a Fallback Hub](04_fallback_hub.md)
