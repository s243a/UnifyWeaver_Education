<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Book 16: Ruby Target

**Block-Based CPS for Idiomatic Ruby**

*Part of the [UnifyWeaver Education Series](../README.md)*

This book covers how to use UnifyWeaver to compile Prolog predicates into Ruby methods. The Ruby target generates idiomatic Ruby code using blocks and `yield`, suitable for Rails applications and Ruby data processing.

## What You'll Learn

By completing this book, you will be able to:

- Compile Prolog predicates to Ruby methods using block-based continuation-passing style
- Understand how joins compile to nested blocks with `next unless` guards
- Use tail recursion optimization for accumulator-style predicates
- Apply memoization for linear recursive predicates (fibonacci-style)
- Generate aggregations (count, sum, min, max, avg) from `aggregate_all/3`
- Create JSON output wrappers for API endpoints
- Use the 150+ FFI bindings for Ruby standard library methods

## Prerequisites

**Required:**
- [Book 1: Foundations](../book-01-foundations/README.md)

**Recommended:**
- [Book 15: Perl Target](../book-15-perl-target/README.md) - Similar CPS concepts
- Basic Ruby knowledge (blocks, yield, iterators)

**Technical:**
- Ruby 2.0+ installed
- UnifyWeaver with Ruby target support

## Learning Path

**1. Introduction** (`01_introduction.md`)
- Why use the Ruby target?
- Block-based CPS architecture
- Comparison with Perl and Python targets

**2. Basic Compilation** (`02_basic_compilation.md`)
- Compiling facts with `each`
- Compiling rules with nested blocks
- Join conditions with `next unless`

**3. Recursion Patterns** (`03_recursion_patterns.md`)
- Semi-naive iteration with `Set`
- Tail recursion to `loop do ... end`
- Linear recursion with `@memo`

**4. Aggregations** (`04_aggregations.md`)
- Using `aggregate_all/3`
- Supported templates
- Practical examples

**5. JSON and Rails Integration** (`05_json_rails.md`)
- JSON output mode
- Pipeline mode
- Rails service integration

## Quick Start

```prolog
% Load the Ruby target
:- use_module('src/unifyweaver/targets/ruby_target').

% Define facts
:- dynamic parent/2.
parent(alice, bob).
parent(bob, charlie).

% Compile to Ruby
?- compile_predicate_to_ruby(parent/2, [json_output], Code),
   format('~s', [Code]).
```

Output:
```ruby
#!/usr/bin/env ruby
require 'set'
require 'json'

def parent
  facts = [
    ["alice", "bob"],
    ["bob", "charlie"]
  ]
  facts.each { |fact| yield(*fact) }
end

def parent_json
  results = []
  parent { |*args| results << [args[1], args[2]] }
  puts results.to_json
end

parent_json if __FILE__ == $0
```

## Key Concepts

### Block-Based CPS

Every generated method yields results to a block:

```ruby
def predicate
  # ... compute results ...
  yield(result1, result2)  # Pass results to block
end

# Usage
predicate { |a, b| puts "#{a} -> #{b}" }
```

### Join Compilation

Multi-goal rules compile to nested blocks with guard conditions:

```prolog
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

```ruby
def grandparent
  parent do |p0, p1|
    parent do |p2, p3|
      next unless p2 == p1  # Join condition
      yield(p0, p3)
    end
  end
end
```

### Recursion Optimization

| Pattern | Optimization | Ruby Idiom |
|---------|--------------|------------|
| Transitive closure | Semi-naive with `Set` | `seen = Set.new` |
| Accumulator | Loop | `loop do ... break ... end` |
| Multiple calls | Instance memoization | `@memo ||= {}` |

## Related Books

- [Book 15: Perl Target](../book-15-perl-target/README.md) - Callback-based CPS
- [Book 7: Cross-Target Glue](../book-07-cross-target-glue/README.md) - Orchestrating multiple targets

## License

This educational content is licensed under CC BY 4.0.
Code examples are dual-licensed under MIT OR Apache-2.0.
