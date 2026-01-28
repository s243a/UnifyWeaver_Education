<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Basic Compilation - Implementation Details

This document provides function-level documentation for Perl target compilation.

**Source**: `src/unifyweaver/targets/perl_target.pl`

---

## Overview: Compilation Patterns

| Prolog Construct | Perl Output |
|------------------|-------------|
| Fact `p(a, b).` | Element in `@facts` array |
| Rule with filter | `return unless` check |
| Rule with join | Nested callbacks |
| Multiple clauses | Sequential callback calls |

---

## compile_predicate_to_perl/3

Compiles a Prolog predicate to Perl code.

### Signature

```prolog
compile_predicate_to_perl(+Predicate/Arity, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicate/Arity` | `atom/integer` | Predicate to compile |
| `Options` | `list` | Compilation options |
| `Code` | `string` | Generated Perl code |

### Example

```prolog
?- compile_predicate_to_perl(employee/2, [], Code), format('~s', [Code]).
```

---

## Fact Compilation

### Pattern

```prolog
employee(alice, engineering).
employee(bob, marketing).
employee(charlie, engineering).
```

### Generated Structure

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

### Key Elements

| Element | Purpose |
|---------|---------|
| `my $callback = shift` | Receive callback function |
| `@facts` | Array of array references |
| `foreach` | Iterate all facts |
| `@$fact` | Dereference and spread |

---

## Simple Rule Compilation

### Pattern

```prolog
engineer(Name) :- employee(Name, engineering).
```

### Generated Structure

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

### Transformation Steps

1. **Call dependency**: Pass anonymous sub to `employee`
2. **Destructure**: `my ($p0, $p1) = @_`
3. **Filter**: `return unless` for condition
4. **Project**: Call callback with selected variables

---

## Join Compilation

### Pattern

```prolog
same_dept(A, B) :- employee(A, Dept), employee(B, Dept), A \= B.
```

### Generated Structure

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

### Variable Binding Flow

```
employee(A, Dept)  →  outer: (p0, p1)
                      A binds to p0, Dept binds to p1

employee(B, Dept)  →  inner: (p2, p3)
                      B binds to p2, Dept binds to p3

                      Join: p3 eq p1 (same Dept)
```

---

## Variable Naming Strategy

The compiler uses unique positional names (`p0`, `p1`, `p2`, ...) to avoid variable shadowing in nested closures.

### Problem: Shadowing

```perl
# WRONG - shadowing
employee(sub {
    my ($name, $dept) = @_;
    employee(sub {
        my ($name, $dept) = @_;  # Shadows outer!
        # Cannot reference outer $name or $dept
    });
});
```

### Solution: Unique Names

```perl
# CORRECT - unique names
employee(sub {
    my ($p0, $p1) = @_;
    employee(sub {
        my ($p2, $p3) = @_;  # No shadowing
        return unless $p3 eq $p1;  # Can reference $p1
    });
});
```

---

## Multiple Clause Compilation

### Pattern

```prolog
manager(alice).
manager(bob).
manager(Name) :- employee(Name, Dept), dept_head(Dept, Name).
```

### Generated Structure

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

### Semantics

- Facts call callback directly
- Rules nest callbacks
- All clauses execute (OR semantics)

---

## Perl Operators

### String Comparison

| Prolog | Perl |
|--------|------|
| `A = B` (unify) | `$a eq $b` |
| `A \= B` | `$a ne $b` |

### Numeric Comparison

| Prolog | Perl |
|--------|------|
| `A > B` | `$a > $b` |
| `A < B` | `$a < $b` |
| `A >= B` | `$a >= $b` |
| `A =< B` | `$a <= $b` |

---

## Callback Pattern

All generated predicates follow the callback pattern:

```perl
sub predicate {
    my $callback = shift;
    # ... iterate/filter/join ...
    $callback->(@result);
}
```

### Usage

```perl
# Collect results
my @results;
predicate(sub { push @results, [@_] });

# Print each result
predicate(sub { print "@_\n" });

# First result only
my $found;
predicate(sub { $found //= [@_] });
```

---

## Related Documentation

- [Book 15 Chapter 1: Introduction](../01_introduction.md)
- [Book 15 Chapter 3: Recursion Patterns](../03_recursion_patterns.md)
- [Perl Target Source](../../../../src/unifyweaver/targets/perl_target.pl)
