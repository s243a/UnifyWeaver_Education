<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Basic Compilation - Implementation Details

This document provides function-level documentation for Ruby target compilation.

**Source**: `src/unifyweaver/targets/ruby_target.pl`

---

## Overview: Compilation Patterns

| Prolog Construct | Ruby Output |
|------------------|-------------|
| Fact `p(a, b).` | Element in `facts` array |
| Rule with filter | `next unless` guard |
| Rule with join | Nested blocks |
| Multiple clauses | Sequential `yield` calls |

---

## compile_predicate_to_ruby/3

Compiles a Prolog predicate to Ruby code.

### Signature

```prolog
compile_predicate_to_ruby(+Predicate/Arity, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicate/Arity` | `atom/integer` | Predicate to compile |
| `Options` | `list` | Compilation options |
| `Code` | `string` | Generated Ruby code |

### Example

```prolog
?- compile_predicate_to_ruby(employee/2, [], Code), format('~s', [Code]).
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

### Key Elements

| Element | Purpose |
|---------|---------|
| `facts` | Array of arrays |
| `each` | Iterator |
| `*fact` | Splat operator for unpacking |
| `yield` | Pass values to block |

---

## Simple Rule Compilation

### Pattern

```prolog
engineer(Name) :- employee(Name, engineering).
```

### Generated Structure

```ruby
def engineer
  employee do |p0, p1|
    next unless p1 == "engineering"  # Filter
    yield(p0)                         # Project
  end
end
```

### Transformation Steps

1. **Call dependency**: Pass block to `employee`
2. **Block params**: `|p0, p1|` receives values
3. **Guard**: `next unless` skips non-matches
4. **Project**: `yield` only selected variables

---

## Join Compilation

### Pattern

```prolog
same_dept(A, B) :- employee(A, Dept), employee(B, Dept), A \= B.
```

### Generated Structure

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

### Variable Binding Flow

```
employee(A, Dept)  →  outer block: |p0, p1|
                      A binds to p0, Dept binds to p1

employee(B, Dept)  →  inner block: |p2, p3|
                      B binds to p2, Dept binds to p3

                      Join: p3 == p1 (same Dept)
```

---

## Variable Naming Strategy

The compiler uses unique positional names (`p0`, `p1`, `p2`, ...) to avoid variable shadowing in nested blocks.

### Problem: Shadowing

```ruby
# WRONG - shadowing
employee do |name, dept|
  employee do |name, dept|  # Shadows outer variables!
    # Cannot reference outer name or dept
  end
end
```

### Solution: Unique Names

```ruby
# CORRECT - unique names
employee do |p0, p1|
  employee do |p2, p3|  # No shadowing
    next unless p3 == p1  # Can reference p1
  end
end
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

### Semantics

- Facts yield directly
- Rules nest blocks
- All clauses execute (OR semantics)

---

## Ruby Idioms

### `next unless` vs `if`

Ruby's `next unless` is more idiomatic for guards:

```ruby
# Preferred (generated)
employee do |p0, p1|
  next unless p1 == "engineering"
  yield(p0)
end

# Equivalent but more verbose
employee do |p0, p1|
  if p1 == "engineering"
    yield(p0)
  end
end
```

### Block Syntax

| Syntax | Use Case |
|--------|----------|
| `do...end` | Multi-line blocks |
| `{...}` | Single-line blocks |

```ruby
# Multi-line
employee do |p0, p1|
  next unless condition
  yield(p0)
end

# Single-line
facts.each { |fact| yield(*fact) }
```

---

## Block/Yield Pattern

All generated methods use Ruby's block pattern:

```ruby
def predicate
  # ... iterate/filter/join ...
  yield(*result)
end
```

### Usage

```ruby
# Collect results
results = []
predicate { |*args| results << args }

# Print each result
predicate { |*args| puts args.join(", ") }

# Enumerate
predicate.each { |*args| process(args) }
```

### With Enumerator

```ruby
def predicate
  return enum_for(:predicate) unless block_given?
  # ... iteration logic ...
end

# Now supports Enumerable
predicate.map { |a, b| [a, b] }
predicate.first(5)
predicate.to_a
```

---

## Related Documentation

- [Book 16 Chapter 1: Introduction](../01_introduction.md)
- [Book 16 Chapter 3: Recursion Patterns](../03_recursion_patterns.md)
- [Ruby Target Source](../../../../src/unifyweaver/targets/ruby_target.pl)
