# Book 17: WAM and Hybrid WAM

The WAM (Warren Abstract Machine) material in UnifyWeaver has grown from a
fallback target into the shared conceptual layer behind many hybrid targets.
This book teaches that layer: WAM items, symbolic WAM notation, lowered
emitters, foreign calls, external fact sources, kernels, and cross-target
parity.

Symbolic WAM text is used throughout the book because it is readable. It is
the teaching, debugging, testing, and interchange notation. The preferred
compiler path for hybrid targets is structured WAM items or another
target-ready WAM representation consumed directly by the target emitter. The
symbolic-text path remains useful, but it is not the default internal
transport.

## Why WAM Became Hybrid WAM

Classic WAM gives UnifyWeaver a precise execution model for unification,
choice points, recursion, and backtracking. Native lowering gives each target
a chance to use efficient host-language constructs. Hybrid WAM is the layer
where those goals meet: preserve Prolog semantics, but lower deterministic
code, indexed fact access, foreign calls, kernels, and target-specific runtime
structures when that is the better execution strategy.

## Instruction Set And Item Stream

The book uses symbolic WAM listings as a compact way to show the item stream:

| Category | Instructions |
|----------|--------------|
| **Head Unification** | `get_variable`, `get_value`, `get_constant`, `get_structure`, `unify_variable`, `unify_constant` |
| **Body Construction** | `put_variable`, `put_value`, `put_constant`, `put_structure`, `set_variable`, `set_value`, `set_constant` |
| **Control Flow** | `allocate`, `deallocate`, `call`, `execute`, `proceed` |
| **Choice Points** | `try_me_else`, `retry_me_else`, `trust_me` |

The listing is a readable view. Hybrid targets should normally consume WAM
items or target-ready WAM data directly rather than printing symbolic text and
parsing it back.

## Usage

```prolog
?- use_module('src/unifyweaver/targets/wam_target').
?- wam_target:compile_predicate_to_wam(user:ancestor/2, [output(text)], Code).
```

The `output(text)` form is useful when you want a readable symbolic listing.
Compiler and target integration paths should prefer structured WAM items when
available.

## How Target Books Use This Book

Target-specific books should link here for the shared model and then explain
only the target's own role: native lowering, lowered WAM emission, runtime
interpretation, FFI, indexing, parser support, or kernel dispatch. No target
book should duplicate the full Hybrid WAM architecture.

## Chapters

1. [Introduction to WAM](01_introduction.md)
2. [Instruction Set Architecture](02_isa.md)
3. [Compiling Rules and Recursion](03_compilation.md)
4. [WAM as a Hybrid Routing Layer](04_fallback_hub.md)
5. [Symbolic WAM Across Targets](05_symbolic_to_targets.md)
