<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Basic Compilation - Questions

Q&A companion for [02_basic_compilation_impl.md](./02_basic_compilation_impl.md).

---

## Question Index

1. [What does compile_predicate_to_ruby/3 do?](#b16c02-q-compile)
2. [How are facts compiled?](#b16c02-q-facts)
3. [How are simple rules compiled?](#b16c02-q-simple-rules)
4. [How are joins compiled?](#b16c02-q-joins)
5. [Why use unique variable names?](#b16c02-q-variable-names)
6. [How are multiple clauses compiled?](#b16c02-q-multiple-clauses)
7. [What is the block/yield pattern?](#b16c02-q-yield)
8. [How do I collect results?](#b16c02-q-collect)
9. [What does `next unless` do?](#b16c02-q-next-unless)
10. [How do I make results enumerable?](#b16c02-q-enumerable)

---

## Questions and Answers

### <a id="b16c02-q-compile"></a>Q1: What does compile_predicate_to_ruby/3 do?

**Answer**: Compiles a Prolog predicate to Ruby code:

```prolog
?- compile_predicate_to_ruby(employee/2, [], Code).
```

Generates a Ruby method with block-based iteration.

**See**: [compile_predicate_to_ruby/3](./02_basic_compilation_impl.md#compile_predicate_to_ruby3)

---

### <a id="b16c02-q-facts"></a>Q2: How are facts compiled?

**Answer**: Facts become an array with `each` iteration:

```ruby
facts = [["alice", "engineering"], ["bob", "marketing"]]
facts.each { |fact| yield(*fact) }
```

Splat operator `*fact` unpacks array.

**See**: [Fact Compilation](./02_basic_compilation_impl.md#fact-compilation)

---

### <a id="b16c02-q-simple-rules"></a>Q3: How are simple rules compiled?

**Answer**: Rules pass blocks to dependencies:

```prolog
engineer(Name) :- employee(Name, engineering).
```

```ruby
def engineer
  employee do |p0, p1|
    next unless p1 == "engineering"
    yield(p0)
  end
end
```

**See**: [Simple Rule Compilation](./02_basic_compilation_impl.md#simple-rule-compilation)

---

### <a id="b16c02-q-joins"></a>Q4: How are joins compiled?

**Answer**: Shared variables become nested blocks with equality checks:

```ruby
employee do |p0, p1|  # A, Dept
  employee do |p2, p3|  # B, Dept
    next unless p3 == p1  # Join condition
    yield(p0, p2)
  end
end
```

**See**: [Join Compilation](./02_basic_compilation_impl.md#join-compilation)

---

### <a id="b16c02-q-variable-names"></a>Q5: Why use unique variable names?

**Answer**: To avoid shadowing in nested blocks:

```ruby
# Shadowing problem
employee do |name, dept|
  employee do |name, dept|  # SHADOWS outer!
  end
end
```

Solution: use `p0`, `p1`, `p2`, etc.

**See**: [Variable Naming Strategy](./02_basic_compilation_impl.md#variable-naming-strategy)

---

### <a id="b16c02-q-multiple-clauses"></a>Q6: How are multiple clauses compiled?

**Answer**: Sequential yield calls (OR semantics):

```ruby
def manager
  yield("alice")          # Clause 1 (fact)
  yield("bob")            # Clause 2 (fact)
  employee do |p0, p1|    # Clause 3 (rule)
    # ...
  end
end
```

**See**: [Multiple Clause Compilation](./02_basic_compilation_impl.md#multiple-clause-compilation)

---

### <a id="b16c02-q-yield"></a>Q7: What is the block/yield pattern?

**Answer**: All predicates use Ruby's block pattern:

```ruby
def predicate
  # ... iterate/filter ...
  yield(*result)
end
```

Caller provides block: `predicate { |a, b| puts "#{a}, #{b}" }`

**See**: [Block/Yield Pattern](./02_basic_compilation_impl.md#blockyield-pattern)

---

### <a id="b16c02-q-collect"></a>Q8: How do I collect results?

**Answer**: Push to array in block:

```ruby
results = []
predicate { |*args| results << args }
```

Or print: `predicate { |*args| puts args.join(", ") }`

**See**: [Usage](./02_basic_compilation_impl.md#usage)

---

### <a id="b16c02-q-next-unless"></a>Q9: What does `next unless` do?

**Answer**: Skips current iteration if condition is false:

```ruby
employee do |p0, p1|
  next unless p1 == "engineering"  # Skip if not engineering
  yield(p0)                         # Only reaches here if matched
end
```

More idiomatic than `if` for guards.

**See**: [Ruby Idioms](./02_basic_compilation_impl.md#ruby-idioms)

---

### <a id="b16c02-q-enumerable"></a>Q10: How do I make results enumerable?

**Answer**: Return `enum_for` when no block given:

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

**See**: [With Enumerator](./02_basic_compilation_impl.md#with-enumerator)

---

## Summary

Ruby target compilation provides:
- Facts as arrays with `each` and splat
- Rules as nested blocks
- Joins via shared variable equality checks
- Unique variable names to avoid shadowing
- `next unless` guards for filtering
- Block/yield pattern for result streaming
- Optional Enumerator support
