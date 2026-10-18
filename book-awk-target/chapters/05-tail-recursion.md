<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 5: Tail Recursion

This chapter covers numeric tail-recursive predicates and their lowering to AWK
while loops, plus specialized graph recursion through the shared compiler.

## What is Tail Recursion?

Tail recursion is a special form of recursion where the recursive call is the last operation in the function. This allows the compiler to optimize it into a loop, avoiding stack overflow.

```prolog
% Tail-recursive: recursive call is last
factorial(0, Acc, Acc).
factorial(N, Acc, Result) :-
    N > 0,
    N1 is N - 1,
    Acc1 is Acc * N,
    factorial(N1, Acc1, Result).  % Last operation

% NOT tail-recursive: operation after recursive call
factorial_bad(0, 1).
factorial_bad(N, Result) :-
    N > 0,
    N1 is N - 1,
    factorial_bad(N1, R),
    Result is R * N.  % Operation AFTER recursion
```

## Why Tail Recursion for AWK?

AWK supports recursive user-defined functions, including in gawk. UnifyWeaver's
restrictions concern translating Prolog recursion, variable binding and search
semantics. For recognized numeric accumulator patterns, the direct AWK target
replaces recursive calls with updates in a while loop:

```
Tail-Recursive Prolog → AWK While Loop
```

This transformation avoids growing a function-call stack and makes the iteration
explicit. It does not imply support for every tail-recursive Prolog predicate.

The AWK snippets below illustrate the algorithmic lowering; they are not literal
compiler output or verified wrapper interfaces. The [complete example](#complete-example)
uses the tested direct API and input layout. Graph patterns and the shared API
have separate [entry-point caveats](#entry-point-caveats).

## Tail Recursion Pattern

### Required Structure

```prolog
% Base case
predicate(BaseCondition, Acc, Result) :-
    BaseCondition,
    Result = Acc.

% Recursive case
predicate(Input, Acc, Result) :-
    RecursiveCondition,
    NewInput is ...,
    NewAcc is ...,
    predicate(NewInput, NewAcc, Result).  % MUST be last
```

### Illustrative Lowering

```awk
function predicate(Input, Acc) {
    while (1) {
        if (BaseCondition) {
            return Acc
        }

        # Update variables
        NewInput = ...
        NewAcc = ...

        # Loop instead of recurse
        Input = NewInput
        Acc = NewAcc
    }
}
```

## Example: Factorial

### Prolog Definition

```prolog
factorial(0, Acc, Acc).
factorial(N, Acc, Result) :-
    N > 0,
    N1 is N - 1,
    Acc1 is Acc * N,
    factorial(N1, Acc1, Result).

% Entry point (wrapper)
fact(N, Result) :-
    factorial(N, 1, Result).
```

### Illustrative AWK

```awk
function factorial(N, Acc) {
    while (1) {
        if (N == 0) {
            return Acc
        }
        if (N > 0) {
            N1 = N - 1
            Acc1 = Acc * N
            N = N1
            Acc = Acc1
        }
    }
}

{
    N = $1
    Result = factorial(N, 1)
    print Result
}
```

## Example: Fibonacci

### Prolog Definition

```prolog
% Tail-recursive Fibonacci with accumulators
fib(0, A, _, A).
fib(N, A, B, Result) :-
    N > 0,
    N1 is N - 1,
    Sum is A + B,
    fib(N1, B, Sum, Result).

% Entry point
fibonacci(N, Result) :-
    fib(N, 0, 1, Result).
```

### Illustrative AWK

```awk
function fib(N, A, B) {
    while (1) {
        if (N == 0) {
            return A
        }
        if (N > 0) {
            N1 = N - 1
            Sum = A + B
            N = N1
            temp = B
            B = Sum
            A = temp
        }
    }
}

{
    N = $1
    Result = fib(N, 0, 1)
    print Result
}
```

## Example: Sum from N to 1

### Prolog Definition

```prolog
% Sum numbers from N down to 1
sum_to(0, Acc, Acc).
sum_to(N, Acc, Result) :-
    N > 0,
    N1 is N - 1,
    Acc1 is Acc + N,
    sum_to(N1, Acc1, Result).

sum(N, Result) :-
    sum_to(N, 0, Result).
```

### Illustrative AWK

```awk
function sum_to(N, Acc) {
    while (1) {
        if (N == 0) {
            return Acc
        }
        N1 = N - 1
        Acc1 = Acc + N
        N = N1
        Acc = Acc1
    }
}

{
    N = $1
    Result = sum_to(N, 0)
    print Result
}
```

## Example: GCD (Greatest Common Divisor)

### Prolog Definition

```prolog
% Euclidean algorithm
gcd(A, 0, A).
gcd(A, B, Result) :-
    B > 0,
    R is A mod B,
    gcd(B, R, Result).
```

### Illustrative AWK

```awk
function gcd(A, B) {
    while (1) {
        if (B == 0) {
            return A
        }
        R = A % B
        A = B
        B = R
    }
}

{
    A = $1
    B = $2
    Result = gcd(A, B)
    print Result
}
```

## Example: Power (Exponentiation)

### Prolog Definition

```prolog
% Power with accumulator
power(_, 0, Acc, Acc).
power(Base, Exp, Acc, Result) :-
    Exp > 0,
    Exp1 is Exp - 1,
    Acc1 is Acc * Base,
    power(Base, Exp1, Acc1, Result).

pow(Base, Exp, Result) :-
    power(Base, Exp, 1, Result).
```

### Illustrative AWK

```awk
function power(Base, Exp, Acc) {
    while (1) {
        if (Exp == 0) {
            return Acc
        }
        Exp1 = Exp - 1
        Acc1 = Acc * Base
        Exp = Exp1
        Acc = Acc1
    }
}

{
    Base = $1
    Exp = $2
    Result = power(Base, Exp, 1)
    print Result
}
```

## Accumulator Pattern

The key to tail recursion is the **accumulator** - a parameter that builds up the result:

```prolog
% Without accumulator (NOT tail-recursive)
sum_bad(0, 0).
sum_bad(N, Result) :-
    N > 0,
    N1 is N - 1,
    sum_bad(N1, R),
    Result is R + N.  % Operation after recursion!

% With accumulator (tail-recursive)
sum_good(0, Acc, Acc).
sum_good(N, Acc, Result) :-
    N > 0,
    N1 is N - 1,
    Acc1 is Acc + N,  % Build result in accumulator
    sum_good(N1, Acc1, Result).  % Recursive call is last
```

## Limitations

### Specialized Patterns, Not General Prolog Recursion

Numeric tail loops are one supported pattern. The shared compiler also recognizes
canonical binary transitive closure, and the direct target has a specialized
ternary hop traversal, described below. These do not establish support for
arbitrary linear, tree or mutual recursion. For example, the following definitions
are outside the verified recipes; obtaining code from a compiler entry point is
not sufficient evidence that it implements them correctly:

```prolog
% Outside the verified patterns: numeric work after recursion
bad_factorial(0, 1).
bad_factorial(N, Result) :-
    N > 0,
    N1 is N - 1,
    bad_factorial(N1, R),
    Result is R * N.  % Operation after recursion

% Outside the verified patterns: mutual recursion
even(0).
even(N) :- N > 0, N1 is N - 1, odd(N1).

odd(N) :- N > 0, N1 is N - 1, even(N1).
```

### Detecting Tail Position

For the numeric tail-loop pattern, the recursive call must be the **last goal** in
the clause body. Tail position is necessary for this lowering, not a general
guarantee of compiler support:

```prolog
% Tail position (conceptual pattern)
foo(N, Acc, Result) :-
    N > 0,
    N1 is N - 1,
    Acc1 is Acc + N,
    foo(N1, Acc1, Result).  % ✓ Last goal

% Not a tail-loop pattern
foo(N, Acc, Result) :-
    N > 0,
    foo(N1, Acc1, Result),  % ✗ Not last
    N1 is N - 1,
    Acc1 is Acc + N.
```

## Complete Example

Save this as `factorial_recipe.pl` in the **UnifyWeaver compiler root**. It compiles
`factorial/3` directly, without the illustrative `fact/2` wrapper:

```prolog
% SPDX-License-Identifier: MIT OR Apache-2.0
% file: factorial_recipe.pl (run from the UnifyWeaver root)
:- encoding(utf8).
:- use_module('src/unifyweaver/targets/awk_target').

factorial(0, Acc, Acc).
factorial(N, Acc, Result) :-
    N > 0,
    N1 is N - 1,
    Acc1 is Acc * N,
    factorial(N1, Acc1, Result).

generate :-
    awk_target:compile_predicate_to_awk(
        factorial/3, [field_separator('\t')], Code),
    awk_target:write_awk_script(Code, 'factorial.awk').

:- initialization(generate, main).
```

Generate and run it from that root:

```bash
swipl -q -s factorial_recipe.pl
printf '5\t1\t0\n0\t1\t0\n' | awk -f factorial.awk
# 120
# 1
```

The input fields are `N`, the initial accumulator and a result placeholder. The
third field is present for the predicate's arity; use `0` as the placeholder.
This verified case uses nonnegative integer `N` and a numeric accumulator. It
does not establish a wrapper or one-field input interface, nor full Prolog modes
or unification for the result argument. The writer's argument order is
`write_awk_script(Code, FilePath)`.

The reusable [factorial example](../examples/factorial.pl) generates into an
output directory. With Education checked out separately, run from the compiler root:

```bash
export EDUCATION_ROOT=/absolute/path/to/UnifyWeaver_Education
swipl -q -s init.pl -s "$EDUCATION_ROOT/book-awk-target/examples/factorial.pl" -- output/awk-book
printf '5\t1\t0\n' | awk -f output/awk-book/factorial.awk
# 120
```

## Graph Recursion

For canonical binary reachability, use the **shared recursive compiler**:

```prolog
edge(a, b).
edge(a, c).
edge(b, d).
edge(c, d).
edge(d, b).

path(X, Y) :- edge(X, Y).
path(X, Y) :- edge(X, Z), path(Z, Y).

% After loading recursive_compiler and the definitions in user:
recursive_compiler:compile_recursive(
    path/2, [target(awk), input(embedded)], Code),
awk_target:write_awk_script(Code, 'path.awk').
```

The emitted code embeds the edge facts in adjacency arrays. Breadth-first search
(BFS) uses a queue and visited set so cycles terminate. Its `check` function tests
one source/target pair; `find_all` enumerates reachable vertices from one source.
`input(embedded)` emits the functions and facts without a command-line driver;
the example's [query wrapper](../examples/graph_query.awk) calls them from `END`.
The [runnable graph example](../examples/graph_recursion.pl) loads the modules and
generates both the shared binary closure and direct ternary traversal:

```bash
# From the compiler root, with EDUCATION_ROOT set as above:
swipl -q -s init.pl -s "$EDUCATION_ROOT/book-awk-target/examples/graph_recursion.pl" -- output/awk-book
awk -f output/awk-book/path.awk -f "$EDUCATION_ROOT/book-awk-target/examples/graph_query.awk" -v start=a -v target=d /dev/null
# a:d (exit 0)
awk -f output/awk-book/path.awk -f "$EDUCATION_ROOT/book-awk-target/examples/graph_query.awk" -v start=a /dev/null | sort
# a:b
# a:c
# a:d
```

These computations have different meanings:

| Query | Result | Traversal semantics |
|-------|--------|---------------------|
| Single-source reachability | Distinct vertices reachable from one source | Shared BFS; one visit per vertex |
| All reachable pairs | A relation of distinct `(source, target)` pairs | A pair fixpoint, or repeated source queries with their documented conventions |
| All simple paths | One result per path with no repeated vertex | Direct ternary traversal; explicit DFS stack and a visited set per path |

For the specialized ternary form, a base clause such as
`path_hops(X,Y,1) :- edge(X,Y)` and a recursive clause with
`path_hops(Z,Y,H1), H is H1+1` select the direct target's graph traversal.
It emits source, destination and hop count for each simple path from every source,
not the full vertex sequence. Different paths can produce identical triples.
This is not a shortest-path query or a set of distinct reachable pairs. Per-path
visited state prevents repeated vertices, but the number of simple paths can still
grow rapidly. This generated cycle policy also differs from unrestricted
enumeration of the unguarded Prolog relation on a cyclic graph. This specialized
implementation is not a general recursion engine.

## Entry-Point Caveats

The [validation snapshot](../examples/VALIDATION.md) records the source revision,
tool versions, exact commands and checked outputs. At compiler `a56714b`:

- Direct `awk_target:compile_predicate_to_awk/3` works for the numeric factorial
  above. Shared `recursive_compiler:compile_recursive/3` with `target(awk)` rejects
  that same predicate because the AWK tail-recursion hook is missing.
- The direct API can intercept canonical binary `path/2` in its native clause
  pass and return invalid AWK containing unsupported-goal comments and a dangling
  `else`. Use the tested shared closure entry point for that recipe. Compilation
  returning a value does not guarantee valid or semantically correct AWK.
- Shared BFS marks the start vertex visited before traversal, so it excludes
  `start:start` even when a cycle returns to the start. A pair fixpoint can derive
  self pairs from cycles. Choose and test the convention your query requires.

These are observed compiler behaviors, not restrictions imposed by AWK itself.
Run the [recursion checks](../examples/run_recursion_tests.sh) against your checkout
before relying on a different revision.

## Summary

In this chapter, you learned:

- What tail recursion is and why it matters
- How tail recursion compiles to AWK while loops
- The accumulator pattern for building results
- A runnable direct factorial recipe and illustrative accumulator algorithms
- Shared graph reachability versus specialized all-simple-path traversal
- Entry-point restrictions and the limits of compilation-only evidence
- How to identify tail position in clause bodies

## Next Chapter

In Chapter 6, we'll explore regex pattern matching with the `match/2`, `match/3`, and `match/4` predicates.
