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

Rules involve argument preparation and predicate calls.

### Prolog
```prolog
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

### WAM Output
```wam
grandparent/2:
    get_variable X1, A1    % Preserve X
    get_variable X2, A2    % Preserve Z
    put_variable X1, A1    % Setup X for first call
    put_variable X3, A2    % Setup new Y
    call parent/2, 2
    put_variable X3, A1    % Setup Y for second call
    put_variable X2, A2    % Setup Z
    execute parent/2       % Tail call
```

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
    execute parent/2
L_ancestor_2_2:
    trust_me
    get_variable X1, A1
    get_variable X2, A2
    put_variable X1, A1
    put_variable X3, A2
    call parent/2, 2
    put_variable X3, A1
    put_variable X2, A2
    execute ancestor/2
```
