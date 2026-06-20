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

UnifyWeaver uses WAM as a shared semantic layer. If a predicate cannot be
compiled into clean, idiomatic code for a specific target, it can still be
represented with WAM-shaped items and executed by a target runtime. If a
predicate is simple, deterministic, indexed, or kernel-shaped, a hybrid target
can lower that same WAM-shaped program into host-language structures or direct
helper functions.

Symbolic WAM text is still useful because people can read it. In the compiler,
however, the preferred path is to produce structured WAM items or target-ready
WAM data directly and let the target emitter consume that representation. The
symbolic text path remains available for debugging, tests, and interchange.
