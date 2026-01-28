<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Basic Compilation - Questions

Q&A companion for [02_basic_compilation_impl.md](./02_basic_compilation_impl.md).

---

## Question Index

1. [What does compile_predicate_to_perl/3 do?](#b15c02-q-compile)
2. [How are facts compiled?](#b15c02-q-facts)
3. [How are simple rules compiled?](#b15c02-q-simple-rules)
4. [How are joins compiled?](#b15c02-q-joins)
5. [Why use unique variable names?](#b15c02-q-variable-names)
6. [How are multiple clauses compiled?](#b15c02-q-multiple-clauses)
7. [What is the callback pattern?](#b15c02-q-callback)
8. [How do I collect results?](#b15c02-q-collect)
9. [How is `return unless` used?](#b15c02-q-return-unless)
10. [What operators are used for comparison?](#b15c02-q-operators)

---

## Questions and Answers

### <a id="b15c02-q-compile"></a>Q1: What does compile_predicate_to_perl/3 do?

**Answer**: Compiles a Prolog predicate to Perl code:

```prolog
?- compile_predicate_to_perl(employee/2, [], Code).
```

Generates a Perl subroutine with callback-based iteration.

**See**: [compile_predicate_to_perl/3](./02_basic_compilation_impl.md#compile_predicate_to_perl3)

---

### <a id="b15c02-q-facts"></a>Q2: How are facts compiled?

**Answer**: Facts become an array of array references with `foreach` iteration:

```perl
my @facts = (['alice', 'engineering'], ['bob', 'marketing']);
foreach my $fact (@facts) {
    $callback->(@$fact);
}
```

**See**: [Fact Compilation](./02_basic_compilation_impl.md#fact-compilation)

---

### <a id="b15c02-q-simple-rules"></a>Q3: How are simple rules compiled?

**Answer**: Rules pass anonymous subs to dependencies:

```prolog
engineer(Name) :- employee(Name, engineering).
```

```perl
employee(sub {
    my ($p0, $p1) = @_;
    return unless $p1 eq 'engineering';
    $callback->($p0);
});
```

**See**: [Simple Rule Compilation](./02_basic_compilation_impl.md#simple-rule-compilation)

---

### <a id="b15c02-q-joins"></a>Q4: How are joins compiled?

**Answer**: Shared variables become nested callbacks with equality checks:

```perl
employee(sub {
    my ($p0, $p1) = @_;  # A, Dept
    employee(sub {
        my ($p2, $p3) = @_;  # B, Dept
        return unless $p3 eq $p1;  # Join condition
        $callback->($p0, $p2);
    });
});
```

**See**: [Join Compilation](./02_basic_compilation_impl.md#join-compilation)

---

### <a id="b15c02-q-variable-names"></a>Q5: Why use unique variable names?

**Answer**: To avoid shadowing in nested closures:

```perl
# Shadowing problem - inner $name hides outer
employee(sub {
    my ($name, $dept) = @_;
    employee(sub {
        my ($name, $dept) = @_;  # SHADOWS!
    });
});
```

Solution: use `p0`, `p1`, `p2`, etc.

**See**: [Variable Naming Strategy](./02_basic_compilation_impl.md#variable-naming-strategy)

---

### <a id="b15c02-q-multiple-clauses"></a>Q6: How are multiple clauses compiled?

**Answer**: Sequential callback calls (OR semantics):

```perl
sub manager {
    my $callback = shift;
    $callback->('alice');        # Clause 1 (fact)
    $callback->('bob');          # Clause 2 (fact)
    employee(sub { ... });       # Clause 3 (rule)
}
```

**See**: [Multiple Clause Compilation](./02_basic_compilation_impl.md#multiple-clause-compilation)

---

### <a id="b15c02-q-callback"></a>Q7: What is the callback pattern?

**Answer**: All predicates receive a callback as first argument:

```perl
sub predicate {
    my $callback = shift;
    # ... iterate/filter ...
    $callback->(@result);
}
```

**See**: [Callback Pattern](./02_basic_compilation_impl.md#callback-pattern)

---

### <a id="b15c02-q-collect"></a>Q8: How do I collect results?

**Answer**: Push to array in callback:

```perl
my @results;
predicate(sub { push @results, [@_] });
```

Or print: `predicate(sub { print "@_\n" });`

**See**: [Usage](./02_basic_compilation_impl.md#usage)

---

### <a id="b15c02-q-return-unless"></a>Q9: How is `return unless` used?

**Answer**: For guard conditions that skip non-matching rows:

```perl
return unless $p1 eq 'engineering';  # Skip if not engineering
$callback->($p0);                     # Only reaches here if matched
```

**See**: [Transformation Steps](./02_basic_compilation_impl.md#transformation-steps)

---

### <a id="b15c02-q-operators"></a>Q10: What operators are used for comparison?

**Answer**:

| Prolog | Perl (string) | Perl (numeric) |
|--------|---------------|----------------|
| `A = B` | `eq` | `==` |
| `A \= B` | `ne` | `!=` |
| `A > B` | `gt` | `>` |
| `A < B` | `lt` | `<` |

**See**: [Perl Operators](./02_basic_compilation_impl.md#perl-operators)

---

## Summary

Perl target compilation provides:
- Facts as array of arrays with `foreach`
- Rules as nested anonymous subs
- Joins via shared variable equality checks
- Unique variable names to avoid shadowing
- `return unless` guards for filtering
- Callback pattern for result streaming
