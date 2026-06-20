# Chapter 3: Compiling Rules and Recursion

This chapter demonstrates how the WAM target handles complex Prolog structures like rules and recursive predicates.

## Fact Bases

A collection of facts is compiled using choice points to allow backtracking.

### Prolog
```prolog
parent(alice, bob).
parent(bob, charlie).
```

### WAM Output
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

## Rules and Control Flow

Rules involve argument preparation and predicate calls. The compiler automatically classifies variables as **temporary** (Xi) or **permanent** (Yi). A variable is permanent if it must survive across a `call` instruction — i.e., it is used in a body goal after the first one.

### Prolog
```prolog
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

### WAM Output
```wam
grandparent/2:
    allocate                % Create environment frame for Yi registers
    get_variable X1, A1     % X is temporary (only used in goal 1)
    get_variable Y2, A2     % Z is permanent (used in goal 2, after the call)
    put_value X1, A1        % Setup X for first call
    put_variable Y1, A2     % Y is permanent (spans both goals), stored in env
    call parent/2, 2
    put_value Y1, A1        % Read Y from env frame (survived the call)
    put_value Y2, A2        % Read Z from env frame
    deallocate              % Remove env frame (after Yi reads, before execute)
    execute parent/2        % Tail call optimization
```

Note that `allocate` appears before the head instructions so that `get_variable Yi` can immediately store into the environment frame. The `deallocate` is placed after argument setup but before `execute`, ensuring Yi values are read before the frame is removed.

## Compound Body Arguments (`put_structure`)

When a body goal contains a compound term as an argument, the compiler uses `put_structure` followed by `set_value`/`set_constant` to build the term on the heap before the call.

### Prolog
```prolog
wrap(X) :- check(pair(X, done)).
```

### WAM Output
```wam
wrap/1:
    get_variable X1, A1    % Bind X to X1
    put_structure pair/2, A1  % Begin building pair(X, done) in A1
    set_value X1           % First sub-arg: X (already in X1)
    set_constant done      % Second sub-arg: the atom 'done'
    execute check/1        % Tail call
```

The `put_structure` instruction allocates a structure cell on the heap. Each subsequent `set_*` instruction appends one sub-argument. After all `N` sub-arguments are set, `Ai` holds a reference to the completed compound term.

## Hybrid Compilation: Items First

The symbolic listings in this chapter are meant for humans. Internally, the
useful boundary is the WAM item stream: labels and instruction terms that a
target emitter can consume without printing text and parsing it back.

For a small fact:

```prolog
parent(alice, bob).
```

the teaching notation is:

```wam
parent/2:
    get_constant alice, A1
    get_constant bob, A2
    proceed
```

The compiler-facing shape is closer to:

```prolog
[ label("parent/2"),
  get_constant("alice", "A1"),
  get_constant("bob", "A2"),
  proceed
]
```

That item list is what a hybrid target should prefer. A symbolic text dump can
still be produced for debugging, golden tests, or interchange, but the normal
path does not need a print/parse round trip.

| Symbolic WAM | Item shape | Why the target cares |
|---|---|---|
| `parent/2:` | `label("parent/2")` | Creates a dispatch entry or generated helper name. |
| `get_constant alice, A1` | `get_constant("alice", "A1")` | Reads argument register `A1` and checks or binds it to `alice`. |
| `proceed` | `proceed` | Returns success to the caller or current continuation. |

## What The Lowered Emitter Decides

A lowered emitter is not just a pretty-printer for WAM text. It receives a
WAM-shaped program and chooses which parts can become direct host-language
code. A typical decision sequence is:

1. **Can this predicate be represented as data?** Fact-only predicates may
   become arrays, maps, indexes, LMDB lookups, or generated tables.
2. **Is the predicate deterministic enough to lower?** A single-clause helper
   may become a normal host-language function over WAM registers.
3. **Does the predicate call a known builtin or kernel?** The emitter may
   replace a WAM call with a host function call, then unify returned values.
4. **Does the predicate need full backtracking?** If so, the target keeps a
   WAM runtime path with choice points, trail handling, and continuation or
   dispatch logic.

The same symbolic instruction can therefore lead to different target shapes:

| Symbolic WAM | Go sketch | Rust sketch | C sketch |
|---|---|---|---|
| `get_constant alice, A1` | `vm.GetConstant("alice", 1)` | `state.get_constant(atom_alice, A1)` | `wam_get_constant(vm, atom_alice, 1)` |
| `call parent/2, 2` | `vm.Call("parent/2")` or `vm.PredParent2()` | `dispatch_parent_2(state)` | `wam_call(vm, PARENT_2)` |
| `try_me_else L2` | push choice point struct | push `ChoicePoint` | push frame on explicit stack |

The table is illustrative. The shared contract is semantic: registers,
unification, choice points, and calls must behave the same even when the host
representation is different. For the *real* item literals each target emits —
grounded in the actual emitters and runtime templates — see
[Chapter 5: Symbolic WAM Across Targets](05_symbolic_to_targets.md).

## Recursion and Tail Call Optimization (TCO)

The WAM target automatically identifies the last call in a rule and uses the `execute` instruction instead of `call` + `proceed`.

### Prolog
```prolog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

### WAM Output
```wam
ancestor/2:
    try_me_else L_ancestor_2_2
    execute parent/2          % First clause: single goal, no allocate needed
L_ancestor_2_2:
    trust_me
    allocate
    get_variable X1, A1       % X is temporary (goal 1 only)
    get_variable Y2, A2       % Y is permanent (goal 2, after call)
    put_value X1, A1
    put_variable Y1, A2       % Z is permanent (spans both goals)
    call parent/2, 2
    put_value Y1, A1          % Read Z from env
    put_value Y2, A2          % Read Y from env
    deallocate
    execute ancestor/2
```
