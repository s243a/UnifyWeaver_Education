<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
-->

# Chapter 3: Recursion Patterns

The Perl target supports three recursion optimization strategies:

| Pattern | Detection | Optimization |
|---------|-----------|--------------|
| Transitive closure | Base + recursive clause | Semi-naive iteration |
| Accumulator | Last call with updated accumulators | While loop |
| Linear (fibonacci) | Multiple recursive calls | Memoization |

## Semi-naive Recursion

For Datalog-style transitive closure predicates:

```prolog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
```

The compiler generates semi-naive iteration:

```perl
sub ancestor {
    my $callback = shift;
    my @delta;      # Worklist of new tuples
    my %seen;       # Deduplication set

    # Base case: seed with parent facts
    parent(sub {
        my ($p0, $p1) = @_;
        my $key = join('\0', $p0, $p1);
        unless ($seen{$key}++) {
            push @delta, [$p0, $p1];
        }
    });

    # Fixpoint iteration
    while (@delta) {
        my $item = shift @delta;
        $callback->(@$item);  # Emit result

        # Recursive expansion
        parent(sub {
            my ($p2, $p3) = @_;
            return unless $p3 eq $item->[0];  # Join condition
            my $key = join('\0', $p2, $item->[1]);
            unless ($seen{$key}++) {
                push @delta, [$p2, $item->[1]];
            }
        });
    }
}
```

### How It Works

1. **Base case seeding**: All base case results go into `@delta`
2. **Deduplication**: `%seen` hash prevents duplicate outputs
3. **Worklist processing**: Process one tuple at a time
4. **Expansion**: Each tuple generates new candidates via recursive clause
5. **Termination**: Stops when no new tuples are discovered

### Key Generation

The `join('\0', ...)` pattern creates unique keys:
- Null byte (`\0`) separator is unlikely in data
- Allows any values including strings with spaces

## Tail Recursion Optimization

For accumulator-style predicates where recursion is the last call:

```prolog
factorial(0, Acc, Acc).
factorial(N, Acc, Result) :-
    N > 0,
    N1 is N - 1,
    Acc1 is Acc * N,
    factorial(N1, Acc1, Result).
```

The compiler generates an efficient loop:

```perl
sub factorial {
    my $callback = shift;
    my ($arg1, $arg2, $arg3) = @_;

    # Tail recursion optimized to loop
    while (1) {
        if ($arg1 == 0) {
            $callback->($arg2);
            return;
        }
        my $tmp0 = ($arg1 - 1);
        my $tmp1 = ($arg2 * $arg1);
        $arg1 = $tmp0;
        $arg2 = $tmp1;
    }
}
```

### Detection Criteria

A predicate qualifies for tail recursion optimization when:

1. **Two clauses**: Base case + recursive case
2. **Base case**: Returns an accumulator directly
3. **Recursive case**: Last call is the recursive call
4. **Accumulators**: Variables are updated before recursion

### Usage

```perl
factorial(sub { print "Result: $_[0]\n"; }, 5, 1, undef);
# Output: Result: 120
```

Note: Arguments are positional - `(N, Acc, Result)` becomes `($arg1, $arg2, $arg3)`.

## Linear Recursion with Memoization

For predicates with multiple recursive calls (fibonacci pattern):

```prolog
fib(0, 0).
fib(1, 1).
fib(N, Result) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, R1),
    fib(N2, R2),
    Result is R1 + R2.
```

The compiler generates memoized CPS:

```perl
{
    my %memo;  # Closure-scoped cache
    sub fib {
        my $callback = shift;
        my ($arg1) = @_;
        my $key = join('\0', $arg1);

        # Cache hit - return immediately
        if (exists $memo{$key}) {
            return $callback->($memo{$key});
        }

        # Base cases
        if ($arg1 == 0) {
            $memo{$key} = 0;
            return $callback->(0);
        }
        if ($arg1 == 1) {
            $memo{$key} = 1;
            return $callback->(1);
        }

        # Recursive case with memoization
        my $tmp0 = ($arg1 - 1);
        my $tmp1 = ($arg1 - 2);
        fib(sub {
            my ($r0) = @_;
            fib(sub {
                my ($r1) = @_;
                my $result = ($r0 + $r1);
                $memo{$key} = $result;
                $callback->($result);
            }, $tmp1);
        }, $tmp0);
    }
}
```

### Key Features

1. **Closure scoping**: `{ my %memo; sub fib { ... } }` keeps memo private
2. **Key generation**: Input arguments form the cache key
3. **Early return**: Cache hit returns immediately via callback
4. **Nested CPS**: Multiple recursive calls become nested callbacks
5. **Result caching**: Final result stored before invoking callback

### Complexity

| Without memoization | With memoization |
|--------------------|------------------|
| O(2^n) | O(n) |
| Exponential time | Linear time |
| Repeated computation | Each value computed once |

## Combining Patterns

The compiler analyzes each predicate independently:

```prolog
% Semi-naive (transitive closure)
path(X, Y) :- edge(X, Y).
path(X, Z) :- edge(X, Y), path(Y, Z).

% Tail recursion (accumulator)
sum_list([], Acc, Acc).
sum_list([H|T], Acc, Sum) :-
    Acc1 is Acc + H,
    sum_list(T, Acc1, Sum).

% Linear with memo (multiple calls)
tree_size(leaf, 1).
tree_size(node(L, R), Size) :-
    tree_size(L, SL),
    tree_size(R, SR),
    Size is SL + SR + 1.
```

## Exercises

### Exercise 3.1: Identify the Pattern

For each predicate, identify which recursion pattern applies:

```prolog
% 1
length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

% 2
reverse([], Acc, Acc).
reverse([H|T], Acc, Rev) :- reverse(T, [H|Acc], Rev).

% 3
reachable(X, X).
reachable(X, Z) :- edge(X, Y), reachable(Y, Z).
```

<details>
<summary>Solutions</summary>

1. **Linear with memoization** - recursive call followed by computation
2. **Tail recursion** - accumulator pattern, recursion is last call
3. **Semi-naive** - transitive closure pattern

</details>

### Exercise 3.2: Trace Memoization

Given `fib(4)`, trace which values are computed and cached:

<details>
<summary>Solution</summary>

```
fib(4) - cache miss
  fib(3) - cache miss
    fib(2) - cache miss
      fib(1) - base case, cache: {1 => 1}
      fib(0) - base case, cache: {0 => 0, 1 => 1}
      result: 1, cache: {0 => 0, 1 => 1, 2 => 1}
    fib(1) - cache hit! returns 1
    result: 2, cache: {..., 3 => 2}
  fib(2) - cache hit! returns 1
  result: 3, cache: {..., 4 => 3}
```

</details>

## Next Steps

In the next chapter, we'll explore how aggregations like `count`, `sum`, `min`, `max`, and `avg` are compiled.

→ [Chapter 4: Aggregations](04_aggregations.md)
