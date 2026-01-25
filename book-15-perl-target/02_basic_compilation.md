<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
-->

# Chapter 2: Basic Compilation

## Compiling Facts

Facts are the simplest case. Each fact becomes an element in an array:

```prolog
:- dynamic employee/2.
employee(alice, engineering).
employee(bob, marketing).
employee(charlie, engineering).
```

Compile with:
```prolog
?- compile_predicate_to_perl(employee/2, [], Code), format('~s', [Code]).
```

Generated Perl:
```perl
sub employee {
    my $callback = shift;
    my @facts = (
        ['alice', 'engineering'],
        ['bob', 'marketing'],
        ['charlie', 'engineering']
    );
    foreach my $fact (@facts) {
        $callback->(@$fact);
    }
}
```

### Key Points

1. **Array of arrays**: Facts become `@facts` array
2. **Iteration**: `foreach` loops through all facts
3. **Spread**: `@$fact` unpacks the array for the callback

## Compiling Simple Rules

Rules with a single goal compile to nested callbacks:

```prolog
engineer(Name) :- employee(Name, engineering).
```

Compile with:
```prolog
?- compile_predicate_to_perl(engineer/1, [], Code), format('~s', [Code]).
```

Generated Perl:
```perl
sub engineer {
    my $callback = shift;
    employee(sub {
        my ($p0, $p1) = @_;
        return unless $p1 eq 'engineering';  # Filter
        $callback->($p0);                     # Project
    });
}
```

### The Pattern

1. **Call dependency**: Inner callback calls `employee`
2. **Filter**: `return unless` checks the condition
3. **Project**: Only relevant variables passed to callback

## Compiling Joins

When rules have multiple goals sharing variables, the compiler generates join conditions:

```prolog
same_dept(A, B) :- employee(A, Dept), employee(B, Dept), A \= B.
```

Compile with:
```prolog
?- compile_predicate_to_perl(same_dept/2, [], Code), format('~s', [Code]).
```

Generated Perl:
```perl
sub same_dept {
    my $callback = shift;
    employee(sub {
        my ($p0, $p1) = @_;  # A=p0, Dept=p1
        employee(sub {
            my ($p2, $p3) = @_;  # B=p2, Dept=p3
            return unless $p3 eq $p1;  # Join: Dept must match
            return unless $p2 ne $p0;  # Filter: A != B
            $callback->($p0, $p2);     # Project: (A, B)
        });
    });
}
```

### Understanding Join Compilation

```
employee(A, Dept)  →  outer callback gets (p0, p1)
                      A binds to p0, Dept binds to p1

employee(B, Dept)  →  inner callback gets (p2, p3)
                      B binds to p2, Dept binds to p3

                      Join condition: p3 eq p1 (same Dept)
```

### Variable Naming

The compiler uses unique names (`p0`, `p1`, `p2`, ...) to avoid shadowing:

```perl
# Without unique names (WRONG - shadowing!)
employee(sub {
    my ($name, $dept) = @_;
    employee(sub {
        my ($name, $dept) = @_;  # Shadows outer variables!
        ...
    });
});

# With unique names (CORRECT)
employee(sub {
    my ($p0, $p1) = @_;
    employee(sub {
        my ($p2, $p3) = @_;  # No shadowing
        return unless $p3 eq $p1;  # Can reference p1
        ...
    });
});
```

## Compiling Multiple Clauses (OR)

Multiple clauses for the same predicate compile to sequential calls:

```prolog
manager(alice).
manager(bob).
manager(Name) :- employee(Name, Dept), dept_head(Dept, Name).
```

Compile with:
```prolog
?- compile_predicate_to_perl(manager/1, [], Code), format('~s', [Code]).
```

Generated Perl:
```perl
sub manager {
    my $callback = shift;

    # Clause 1: manager(alice)
    $callback->('alice');

    # Clause 2: manager(bob)
    $callback->('bob');

    # Clause 3: rule
    employee(sub {
        my ($p0, $p1) = @_;
        dept_head(sub {
            my ($p2, $p3) = @_;
            return unless $p2 eq $p1;  # Dept match
            return unless $p3 eq $p0;  # Name match
            $callback->($p0);
        });
    });
}
```

## Exercises

### Exercise 2.1: Trace a Join

Given:
```prolog
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

And facts:
```prolog
parent(alice, bob).
parent(bob, charlie).
```

Trace the execution manually:
1. First `parent` callback: `($p0, $p1)` = `('alice', 'bob')`
2. Second `parent` callback: `($p2, $p3)` = `('alice', 'bob')`, then `('bob', 'charlie')`
3. Join check: `$p2 eq $p1` means `'alice' eq 'bob'` (false), `'bob' eq 'bob'` (true!)
4. Result: `$callback->('alice', 'charlie')`

### Exercise 2.2: Predict the Output

What Perl code is generated for:

```prolog
sibling(A, B) :- parent(P, A), parent(P, B), A \= B.
```

<details>
<summary>Solution</summary>

```perl
sub sibling {
    my $callback = shift;
    parent(sub {
        my ($p0, $p1) = @_;  # P=p0, A=p1
        parent(sub {
            my ($p2, $p3) = @_;  # P=p2, B=p3
            return unless $p2 eq $p0;  # Same parent
            return unless $p3 ne $p1;  # Different children
            $callback->($p1, $p3);     # (A, B)
        });
    });
}
```

</details>

## Next Steps

In the next chapter, we'll explore how recursive predicates are compiled using semi-naive iteration, tail recursion optimization, and memoization.

→ [Chapter 3: Recursion Patterns](03_recursion_patterns.md)
