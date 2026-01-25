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
?- compile_predicate_to_ruby(employee/2, [], Code), format('~s', [Code]).
```

Generated Ruby:
```ruby
def employee
  facts = [
    ["alice", "engineering"],
    ["bob", "marketing"],
    ["charlie", "engineering"]
  ]
  facts.each { |fact| yield(*fact) }
end
```

### Key Points

1. **Array of arrays**: Facts become `facts` array
2. **Iterator**: `each` iterates through all facts
3. **Splat**: `*fact` unpacks the array for yielding

## Compiling Simple Rules

Rules with a single goal compile to nested blocks:

```prolog
engineer(Name) :- employee(Name, engineering).
```

Compile with:
```prolog
?- compile_predicate_to_ruby(engineer/1, [], Code), format('~s', [Code]).
```

Generated Ruby:
```ruby
def engineer
  employee do |p0, p1|
    next unless p1 == "engineering"  # Filter
    yield(p0)                         # Project
  end
end
```

### The Pattern

1. **Call dependency**: Block calls `employee`
2. **Guard**: `next unless` skips non-matching rows
3. **Project**: Only relevant variables yielded

## Compiling Joins

When rules have multiple goals sharing variables, the compiler generates join conditions:

```prolog
same_dept(A, B) :- employee(A, Dept), employee(B, Dept), A \= B.
```

Compile with:
```prolog
?- compile_predicate_to_ruby(same_dept/2, [], Code), format('~s', [Code]).
```

Generated Ruby:
```ruby
def same_dept
  employee do |p0, p1|  # A=p0, Dept=p1
    employee do |p2, p3|  # B=p2, Dept=p3
      next unless p3 == p1  # Join: Dept must match
      next unless p2 != p0  # Filter: A != B
      yield(p0, p2)         # Project: (A, B)
    end
  end
end
```

### Understanding Join Compilation

```
employee(A, Dept)  →  outer block gets (p0, p1)
                      A binds to p0, Dept binds to p1

employee(B, Dept)  →  inner block gets (p2, p3)
                      B binds to p2, Dept binds to p3

                      Join condition: p3 == p1 (same Dept)
```

### Variable Naming

The compiler uses unique names (`p0`, `p1`, `p2`, ...) to avoid shadowing:

```ruby
# Without unique names (WRONG - shadowing!)
employee do |name, dept|
  employee do |name, dept|  # Shadows outer variables!
    # Can't reference outer name/dept
  end
end

# With unique names (CORRECT)
employee do |p0, p1|
  employee do |p2, p3|  # No shadowing
    next unless p3 == p1  # Can reference p1
  end
end
```

## Compiling Multiple Clauses (OR)

Multiple clauses for the same predicate compile to sequential yielding:

```prolog
manager(alice).
manager(bob).
manager(Name) :- employee(Name, Dept), dept_head(Dept, Name).
```

Compile with:
```prolog
?- compile_predicate_to_ruby(manager/1, [], Code), format('~s', [Code]).
```

Generated Ruby:
```ruby
def manager
  # Clause 1: manager(alice)
  yield("alice")

  # Clause 2: manager(bob)
  yield("bob")

  # Clause 3: rule
  employee do |p0, p1|
    dept_head do |p2, p3|
      next unless p2 == p1  # Dept match
      next unless p3 == p0  # Name match
      yield(p0)
    end
  end
end
```

## Ruby Idioms in Generated Code

### `next unless` vs `if`

Ruby's `next unless` is cleaner for guards:

```ruby
# With next unless (generated)
employee do |p0, p1|
  next unless p1 == "engineering"
  yield(p0)
end

# Equivalent with if (more verbose)
employee do |p0, p1|
  if p1 == "engineering"
    yield(p0)
  end
end
```

### Block Syntax

The compiler uses `do...end` for multi-line and `{...}` for single-line:

```ruby
# Multi-line blocks
employee do |p0, p1|
  next unless condition
  yield(p0)
end

# Single-line iteration
facts.each { |fact| yield(*fact) }
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
1. First `parent` block: `(p0, p1)` = `("alice", "bob")`
2. Second `parent` block: `(p2, p3)` = `("alice", "bob")`, then `("bob", "charlie")`
3. Join check: `p2 == p1` means `"alice" == "bob"` (false), `"bob" == "bob"` (true!)
4. Result: `yield("alice", "charlie")`

### Exercise 2.2: Predict the Output

What Ruby code is generated for:

```prolog
sibling(A, B) :- parent(P, A), parent(P, B), A \= B.
```

<details>
<summary>Solution</summary>

```ruby
def sibling
  parent do |p0, p1|  # P=p0, A=p1
    parent do |p2, p3|  # P=p2, B=p3
      next unless p2 == p0  # Same parent
      next unless p3 != p1  # Different children
      yield(p1, p3)         # (A, B)
    end
  end
end
```

</details>

## Next Steps

In the next chapter, we'll explore how recursive predicates are compiled using semi-naive iteration, tail recursion optimization, and memoization.

→ [Chapter 3: Recursion Patterns](03_recursion_patterns.md)
