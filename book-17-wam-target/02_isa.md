# Chapter 2: Instruction Set Architecture (ISA)

The UnifyWeaver WAM target generates a subset of the standard WAM instruction set. These instructions are symbolic and designed to be easy to lower into other bytecodes.

## Registers

- **Ai (Argument Registers)**: Used to pass arguments into a predicate. `A1` is the first argument, `A2` the second, etc.
- **Xi (Temporary Registers)**: Used within a clause to store intermediate terms or variables.
- **Yi (Permanent Variables)**: Stored in the environment frame on the stack. Used for variables that must survive across `call` instructions (i.e., variables referenced in more than one body goal). The compiler automatically identifies permanent variables and assigns them Yi registers. `allocate` creates the environment frame, and `deallocate` removes it.

## Core Instructions

### 1. Unification (Head)
Used to match the incoming arguments in `Ai`.

- `get_constant(C, Ai)`: Verifies that argument `Ai` is the constant `C`.
- `get_variable(Xn, Ai)`: Binds argument `Ai` to temporary register `Xn`.
- `get_value(Xn, Ai)`: Unifies argument `Ai` with the current value of `Xn`.
- `get_structure(F/N, Ai)`: Operates in two modes. **Read mode**: if `Ai` holds a compound term, verifies functor `F` and arity `N`, then subsequent `unify_*` instructions match sub-arguments. **Write mode**: if `Ai` is unbound, allocates a structure on the heap and subsequent `unify_*` instructions build sub-arguments.
- `unify_variable(Xn)`: In read mode, binds the next sub-argument to register `Xn`. In write mode, creates a new variable on the heap and binds `Xn` to it.
- `unify_value(Xn)`: In read mode, checks that the next sub-argument matches `Xn`. In write mode, pushes the value of `Xn` onto the heap.
- `unify_constant(C)`: In read mode, checks that the next sub-argument equals constant `C`. In write mode, pushes `C` onto the heap.

### 2. Term Construction (Body)
Used to prepare arguments in `Ai` before calling another predicate.

- `put_constant(C, Ai)`: Loads constant `C` into argument register `Ai`.
- `put_variable(Xn, Ai)`: Creates a new variable and stores it in `Xn` and `Ai`.
- `put_value(Xn, Ai)`: Loads the value of `Xn` into `Ai`.
- `put_structure(F/N, Ai)`: Begins constructing a compound term with functor `F` and arity `N` on the heap, storing a reference in `Ai`. Must be followed by `N` set instructions.
- `set_variable(Xn)`: Pushes a new unbound variable onto the heap and binds `Xn` to it. Used inside `put_structure` sequences.
- `set_value(Xn)`: Pushes the current value of `Xn` onto the heap. Used inside `put_structure` sequences.
- `set_constant(C)`: Pushes constant `C` onto the heap. Used inside `put_structure` sequences.

### 3. Control
Manages the execution flow and stack frames.

- `allocate`: Pushes a new environment onto the stack.
- `deallocate`: Pops the current environment.
- `call(P/N, Arity)`: Calls predicate `P/N`. `Arity` is the number of registers to preserve.
- `execute(P/N)`: Jumps to predicate `P/N` (Tail Call Optimization).
- `proceed`: Returns from the current predicate (successful unification).
- `builtin_call(P/N, Arity)`: Evaluates a built-in predicate (e.g., `is/2`, `>/2`, `</2`) using the current argument registers. Does not jump to compiled code — the operation is performed inline by the runtime.

### 4. Choice Points
Used for predicates with multiple clauses.

- `try_me_else(Label)`: Sets up a choice point, trying the current clause and pointing to `Label` as the alternative.
- `retry_me_else(Label)`: Updates an existing choice point to the next alternative.
- `trust_me`: Removes the choice point and tries the final alternative.

### 5. Indexing
Optimizes clause selection by dispatching on the first argument.

- `switch_on_constant(Key1:Label1, Key2:Label2, ...)`: If A1 matches a key, jump directly to the corresponding label. Falls through if A1 is unbound or not in the table. Generated automatically when all clauses have atomic first arguments.
