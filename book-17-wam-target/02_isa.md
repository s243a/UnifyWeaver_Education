# Chapter 2: Instruction Set Architecture (ISA)

The UnifyWeaver WAM target generates a subset of the standard WAM instruction set. These instructions are symbolic and designed to be easy to lower into other bytecodes.

## Registers

- **Ai (Argument Registers)**: Used to pass arguments into a predicate. `A1` is the first argument, `A2` the second, etc.
- **Xi (Temporary Registers)**: Used within a clause to store intermediate terms or variables.
- **Yi (Permanent Variables)**: *Note: Currently, UnifyWeaver uses Xi registers for all variables within a clause. Full permanent variable allocation (Yi) on the environment stack is a planned enhancement.*

## Core Instructions

### 1. Unification (Head)
Used to match the incoming arguments in `Ai`.

- `get_constant(C, Ai)`: Verifies that argument `Ai` is the constant `C`.
- `get_variable(Xn, Ai)`: Binds argument `Ai` to temporary register `Xn`.
- `get_value(Xn, Ai)`: Unifies argument `Ai` with the current value of `Xn`.
- `get_structure(F/N, Ai)`: Verifies that `Ai` is a structure with functor `F` and arity `N`.

### 2. Term Construction (Body)
Used to prepare arguments in `Ai` before calling another predicate.

- `put_constant(C, Ai)`: Loads constant `C` into argument register `Ai`.
- `put_variable(Xn, Ai)`: Creates a new variable and stores it in `Xn` and `Ai`.
- `put_value(Xn, Ai)`: Loads the value of `Xn` into `Ai`.

### 3. Control
Manages the execution flow and stack frames.

- `allocate`: Pushes a new environment onto the stack.
- `deallocate`: Pops the current environment.
- `call(P/N, Arity)`: Calls predicate `P/N`. `Arity` is the number of registers to preserve.
- `execute(P/N)`: Jumps to predicate `P/N` (Tail Call Optimization).
- `proceed`: Returns from the current predicate (successful unification).

### 4. Choice Points
Used for predicates with multiple clauses.

- `try_me_else(Label)`: Sets up a choice point, trying the current clause and pointing to `Label` as the alternative.
- `retry_me_else(Label)`: Updates an existing choice point to the next alternative.
- `trust_me`: Removes the choice point and tries the final alternative.
