<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
-->

# Chapter 3: Recursion Patterns

The Ruby target supports three recursion optimization strategies:

| Pattern | Detection | Optimization |
|---------|-----------|--------------|
| Transitive closure | Base + recursive clause | Semi-naive with `Set` |
| Accumulator | Last call with updated accumulators | `loop do ... end` |
| Linear (fibonacci) | Multiple recursive calls | `@memo` memoization |

## Semi-naive Recursion

For Datalog-style transitive closure predicates:

```prolog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
```

The compiler generates semi-naive iteration with Ruby's `Set`:

```ruby
require 'set'

def ancestor
  delta = []       # Worklist of new tuples
  seen = Set.new   # Deduplication set

  # Base case: seed with parent facts
  parent do |p0, p1|
    key = [p0, p1]
    unless seen.include?(key)
      seen.add(key)
      delta << key
    end
  end

  # Fixpoint iteration
  until delta.empty?
    item = delta.shift
    yield(*item)  # Emit result

    # Recursive expansion
    parent do |p2, p3|
      next unless p3 == item[0]  # Join condition
      key = [p2, item[1]]
      unless seen.include?(key)
        seen.add(key)
        delta << key
      end
    end
  end
end
```

### How It Works

1. **Base case seeding**: All base case results go into `delta`
2. **Set deduplication**: `Set` provides O(1) membership testing
3. **Worklist processing**: Process one tuple at a time with `shift`
4. **Expansion**: Each tuple generates new candidates
5. **Termination**: `until delta.empty?` stops when no new tuples

### Key Generation

Ruby arrays work directly as Set keys:

```ruby
key = [p0, p1]        # Array as key
seen.add(key)         # Add to set
seen.include?(key)    # Check membership
```

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

```ruby
def factorial(arg1, arg2, arg3, &block)
  # Tail recursion optimized to loop
  loop do
    if arg1 == 0
      block.call(arg2)
      break
    end
    tmp0 = (arg1 - 1)
    tmp1 = (arg2 * arg1)
    arg1 = tmp0
    arg2 = tmp1
  end
end
```

### Detection Criteria

A predicate qualifies for tail recursion optimization when:

1. **Two clauses**: Base case + recursive case
2. **Base case**: Returns an accumulator directly
3. **Recursive case**: Last call is the recursive call
4. **Accumulators**: Variables are updated before recursion

### Usage

```ruby
factorial(5, 1, nil) { |result| puts "Result: #{result}" }
# Output: Result: 120
```

### Ruby's `loop do`

The `loop do ... end` construct with `break`:

```ruby
loop do
  # ... processing ...
  break if done_condition
  # ... update variables ...
end
```

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

```ruby
def fib(arg1, &block)
  @memo ||= {}  # Lazy initialization
  key = [arg1]

  # Cache hit - return immediately
  if @memo.key?(key)
    return block.call(@memo[key])
  end

  # Base cases
  if arg1 == 0
    @memo[key] = 0
    return block.call(0)
  end
  if arg1 == 1
    @memo[key] = 1
    return block.call(1)
  end

  # Recursive case with memoization
  tmp0 = (arg1 - 1)
  tmp1 = (arg1 - 2)
  fib(tmp0) do |r0|
    fib(tmp1) do |r1|
      result = (r0 + r1)
      @memo[key] = result
      block.call(result)
    end
  end
end
```

### Key Features

1. **`@memo ||= {}`**: Lazy hash initialization using instance variable
2. **Array keys**: `[arg1]` as cache key
3. **Early return**: Cache hit returns immediately via block
4. **Nested blocks**: Multiple recursive calls become nested blocks
5. **Result caching**: Final result stored before invoking block

### The `||=` Idiom

Ruby's conditional assignment:

```ruby
@memo ||= {}
# Equivalent to:
@memo = {} if @memo.nil?
```

### Complexity

| Without memoization | With memoization |
|--------------------|------------------|
| O(2^n) | O(n) |
| Exponential time | Linear time |
| Repeated computation | Each value computed once |

## Explicit Block Parameter

Note that recursive methods use `&block` parameter:

```ruby
def fib(arg1, &block)
  # ...
  fib(tmp0) do |r0|
    fib(tmp1) do |r1|
      block.call(result)  # Call the original block
    end
  end
end
```

This allows passing the block through nested calls.

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

### Exercise 3.2: Compare Ruby Idioms

How does Ruby's approach differ from Perl for memoization?

<details>
<summary>Solution</summary>

| Aspect | Ruby | Perl |
|--------|------|------|
| Storage | `@memo` instance var | `%memo` in closure |
| Initialization | `||= {}` | `my %memo` outside sub |
| Scope | Object-level | Lexical closure |
| Thread safety | Shared across calls | Lexically scoped |

Ruby uses instance variables, while Perl uses closure-scoped lexical variables.

</details>

## Next Steps

In the next chapter, we'll explore how aggregations are compiled.

→ [Chapter 4: Aggregations](04_aggregations.md)
