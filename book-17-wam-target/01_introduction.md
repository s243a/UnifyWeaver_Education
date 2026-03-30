# Chapter 1: Introduction to WAM

The WAM (Warren Abstract Machine) is an abstract architecture for implementing Prolog. It's the "assembly language" for logic programming.

## Philosophy

Most UnifyWeaver targets (Python, Go, Rust) use a "Native Lowering" philosophy. They attempt to translate Prolog predicates into idiomatic constructs of the target language. For example:

- `factorial(N, R)` becomes `def factorial(n): return ...` in Python.
- `parent(X, Y)` becomes a lookup in a `HashMap` in Rust.

But logic programming is more powerful than simple function calls:

1. **Backtracking**: exploring multiple possible branches of execution.
2. **Multi-directional calls**: using the same predicate for both "find parent of bob" and "find children of alice".
3. **Unification**: complex term matching that is more powerful than simple assignment.

When these features are needed, a native translation becomes very complex. WAM solves this by providing a standardized set of instructions specifically designed for these logic programming operations.

## The WAM Strategy in UnifyWeaver

UnifyWeaver uses WAM as a "hub". If a predicate cannot be compiled into clean, idiomatic code for a specific target, it can be compiled to WAM instead.

This WAM bytecode can then be fanned out to other bytecode-based environments:

- **WAT** (WebAssembly Text) for browser or Node.js.
- **Jamaica** or **Krakatau** for the Java Virtual Machine.

By implementing the WAM target once, we provide a robust path for all these environments to support full Prolog semantics.
