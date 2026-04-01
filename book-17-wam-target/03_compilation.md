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
