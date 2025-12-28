<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
-->

# Chapter 1: Introduction to the Ruby Target

## Why Ruby?

Ruby's block syntax makes it ideal for expressing continuation-passing style naturally. The Ruby target generates code that:

- **Uses native Ruby idioms**: Blocks, `yield`, and iterators
- **Integrates with Rails**: Easy to use in models and services
- **Leverages RubyGems**: FFI bindings for standard library and gems
- **Reads naturally**: Generated code looks like hand-written Ruby

## Architecture Overview

The Ruby target uses **Block-Based Continuation-Passing Style**, where every generated method yields results to a block:

```
┌─────────────────┐
│  Prolog Clause  │
└────────┬────────┘
         │ compile_predicate_to_ruby/3
         ▼
┌─────────────────┐
│   Ruby Method   │
│ (block + yield) │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Results via    │
│     yield       │
└─────────────────┘
```

### Why Blocks?

Ruby blocks are perfect for CPS:

1. **Native syntax**: `do |args| ... end` is idiomatic Ruby
2. **Implicit passing**: No need to explicitly pass callbacks
3. **Clean nesting**: Nested blocks read naturally
4. **Early exit**: `break` and `next` control flow

## Comparison with Other Targets

| Feature | Ruby | Perl | Python |
|---------|------|------|--------|
| Result mechanism | `yield` to block | `$callback->()` | Generator `yield` |
| Join style | Nested blocks | Nested callbacks | Nested generators |
| Deduplication | `Set.new` | `%seen` hash | `set()` |
| Memoization | `@memo` instance var | `%memo` in closure | `@functools.cache` |
| Best for | Rails, scripts | Unix pipelines | Data science |

## Compilation API

```prolog
compile_predicate_to_ruby(+Pred/Arity, +Options, -RubyCode)
```

### Parameters

- `Pred/Arity`: The predicate to compile (e.g., `parent/2`)
- `Options`: List of compilation options
- `RubyCode`: Output string containing Ruby code

### Options

| Option | Effect |
|--------|--------|
| `[]` | Basic compilation |
| `[json_output]` | Add `_json` method with `to_json` |
| `[pipeline]` | Add `run_pipeline` for stdin/stdout |

## Your First Compilation

```prolog
:- use_module('src/unifyweaver/targets/ruby_target').

% Define a simple predicate
:- dynamic color/1.
color(red).
color(green).
color(blue).

% Compile it
?- compile_predicate_to_ruby(color/1, [], Code),
   format('~s', [Code]).
```

Output:
```ruby
#!/usr/bin/env ruby
require 'set'

def color
  facts = [
    ["red"],
    ["green"],
    ["blue"]
  ]
  facts.each { |fact| yield(*fact) }
end
```

## Using Generated Code

Save the output and use it:

```ruby
#!/usr/bin/env ruby
require_relative 'predicates'

# Print all colors
color { |c| puts "Color: #{c}" }

# Collect into array
colors = []
color { |c| colors << c }
puts "Found #{colors.size} colors"

# Find specific color
found = nil
color do |c|
  if c == 'green'
    found = c
    break  # Early termination
  end
end
puts "Found: #{found}"
```

## Ruby-Specific Features

### Splat Operator

The `*fact` unpacks arrays for yielding:

```ruby
facts.each { |fact| yield(*fact) }
# Equivalent to: yield(fact[0], fact[1], ...)
```

### Set for Deduplication

Ruby's `Set` class provides O(1) membership testing:

```ruby
require 'set'
seen = Set.new
seen.add([key])
seen.include?([key])  # Fast lookup
```

### Instance Variable Memoization

Ruby's `||=` idiom for lazy initialization:

```ruby
@memo ||= {}  # Create hash only if nil
```

## Next Steps

In the next chapter, we'll explore how facts and rules compile to Ruby code, and how join conditions are generated.

→ [Chapter 2: Basic Compilation](02_basic_compilation.md)
