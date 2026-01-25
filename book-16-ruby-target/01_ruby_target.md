# Ruby Target

The Ruby target (`target(ruby)`) generates Ruby methods from Prolog predicates using blocks and `yield`. It provides streaming evaluation with Ruby's native iteration patterns, making generated code idiomatic and easy to integrate with existing Ruby applications.

## Overview

Ruby programs use blocks and `yield` to stream results, leveraging Ruby's powerful iteration model. This makes the generated code feel natural to Ruby developers.

```prolog
% Compile to Ruby
?- compile_predicate_to_ruby(my_predicate/2, [], RubyCode).
```

## Features

### Core Capabilities

| Feature | Status | Description |
|---------|--------|-------------|
| Facts | ✅ | Direct fact compilation to arrays with `each` |
| Single Rules | ✅ | Body-to-code translation with nested blocks |
| Multiple Rules (OR) | ✅ | Sequential clause evaluation |
| Recursion | ✅ | Semi-naive iteration with `Set` deduplication |
| Joins | ✅ | Inner joins with proper variable binding |

## Block-Based Iteration

All predicates generate methods that yield results to a block:

```ruby
def parent
  facts = [
    ["alice", "bob"],
    ["bob", "charlie"]
  ]
  facts.each { |fact| yield(*fact) }
end

# Usage: stream all parent relationships
parent { |x, y| puts "#{x} -> #{y}" }
```

## Join Handling

When rules contain multiple goals sharing variables, the compiler generates proper join conditions using `next unless`:

```prolog
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

Generates:

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

Key implementation details:
- **Unique parameter names** (`p0`, `p1`, etc.) avoid variable shadowing in nested blocks
- **Join conditions** (`next unless`) enforce variable equality between goals
- **Immediate projection** yields only the head variables

## Recursive Predicates

Recursive predicates use semi-naive iteration to ensure termination and avoid duplicate outputs:

```prolog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
```

Generates:

```ruby
def ancestor
  delta = []
  seen = Set.new

  # Base cases - seed the worklist
  parent do |p0, p1|
    key = [p0, p1]
    unless seen.include?(key)
      seen.add(key)
      delta << key
    end
  end

  # Semi-naive iteration
  until delta.empty?
    item = delta.shift
    yield(*item)  # Output current result

    # Expand: find new results via recursive clause
    parent do |p0, p1|
      next unless p1 == item[0]  # Join on shared variable
      key = [p0, item[1]]
      unless seen.include?(key)
        seen.add(key)
        delta << key
      end
    end
  end
end
```

Key features:
- **`delta` array**: Contains tuples to process; new results are appended here
- **`seen` Set**: Prevents duplicate outputs using array keys
- **Fixpoint computation**: Loop terminates when no new tuples are discovered

## Bindings

The Ruby target includes 151 FFI bindings organized by category:

### String Operations
```prolog
declare_binding(ruby, string_split/3, '.split', [string, string], [array], [...])
declare_binding(ruby, string_upcase/2, '.upcase', [string], [string], [...])
declare_binding(ruby, string_gsub/4, '.gsub', [string, string, string], [string], [...])
```

### Array Operations
```prolog
declare_binding(ruby, array_map/3, '.map', [array, block], [array], [...])
declare_binding(ruby, array_select/3, '.select', [array, block], [array], [...])
declare_binding(ruby, array_reduce/4, '.reduce', [array, any, block], [any], [...])
```

### Hash Operations
```prolog
declare_binding(ruby, hash_fetch/3, '.fetch', [hash, any], [any], [...])
declare_binding(ruby, hash_merge/3, '.merge', [hash, hash], [hash], [...])
declare_binding(ruby, hash_transform_keys/3, '.transform_keys', [hash, block], [hash], [...])
```

### I/O Operations
```prolog
declare_binding(ruby, file_read/2, 'File.read', [string], [string], [...])
declare_binding(ruby, puts/1, 'puts', [any], [], [...])
```

## Custom Components

The `custom_ruby` component type allows injecting custom Ruby code:

```prolog
declare_component(source, my_transform, custom_ruby, [
    code("input.upcase"),
    requires(['json', 'set'])
]).
```

Generates:

```ruby
require 'json'
require 'set'

class CompMy_transform
  def invoke(input)
    input.upcase
  end
end
```

## Performance Characteristics

- **Memory**: O(n) where n is the number of unique output tuples (required for deduplication in recursive predicates)
- **Time**: Nested-loop joins; adequate for modest data sets
- **Streaming**: Results are yielded immediately as they're discovered

## Limitations

- No aggregation support yet
- No window functions
- Inner joins only (no outer join patterns)
- String-based comparison (`==`) for all types

## Usage Example

```ruby
#!/usr/bin/env ruby
require 'set'

# ... generated code ...

# Print all ancestors
puts "Ancestors:"
ancestor { |x, y| puts "  #{x} is ancestor of #{y}" }

# Collect results into array
results = []
ancestor { |x, y| results << [x, y] }
puts "Total: #{results.length}"

# Filter results
ancestor { |x, y| puts "#{x} -> #{y}" if x == "alice" }
```

## Roadmap

1. Add aggregation support (`count`, `sum`, `min`, `max`)
2. Implement outer join patterns
3. Add Enumerator support for lazy evaluation
4. Support method chaining patterns
