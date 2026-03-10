# Chapter 2: Facts and Rules

## Facts → Module Attributes

Prolog facts become Elixir module attributes with accessor functions.

### Source Prolog

```prolog
parent(tom, bob).
parent(bob, jim).
parent(jim, ann).
```

### Compilation

```prolog
?- compile_facts_to_elixir(parent, 2, Code), write(Code).
```

### Generated Elixir

```elixir
defmodule Generated.Parent do
  @moduledoc "Generated facts for parent/2"

  @facts [
    {"tom", "bob"},
    {"bob", "jim"},
    {"jim", "ann"}
  ]

  @doc "Return all facts"
  def all, do: @facts

  @doc "Check if tuple is a fact"
  def member?(args), do: args in @facts

  @doc "Stream all facts lazily"
  def stream, do: Stream.map(@facts, & &1)
end
```

### Running It

```elixir
iex> Generated.Parent.all()
[{"tom", "bob"}, {"bob", "jim"}, {"jim", "ann"}]

iex> Generated.Parent.member?({"tom", "bob"})
true

iex> Generated.Parent.member?({"tom", "ann"})
false

iex> Generated.Parent.stream() |> Enum.take(2)
[{"tom", "bob"}, {"bob", "jim"}]
```

The `stream/0` function wraps the fact list in a `Stream`, enabling lazy
evaluation when chaining with other `Stream` functions.

## Rules → Pattern-Matched `def`

Prolog rules become Elixir function clauses with pattern-matched heads.

### Source Prolog

```prolog
greet(hello, R) :- R is 1.
greet(world, R) :- R is 2.
```

### Compilation

```prolog
?- compile_rules_to_elixir(greet/2, [], Code), write(Code).
```

### Generated Elixir

```elixir
defmodule Generated.Greet do
  def greet("hello", arg2) do
    try do
      arg2 = 1
      {:ok, ["hello", arg2]}
    catch
      :fail -> :fail
    end
  end

  def greet("world", arg2) do
    try do
      arg2 = 2
      {:ok, ["world", arg2]}
    catch
      :fail -> :fail
    end
  end
end
```

### How Pattern Matching Works

Ground arguments in the Prolog clause head (like `hello`) become literal patterns
in the Elixir function head:

| Prolog Head | Elixir Pattern |
|-------------|----------------|
| `greet(hello, R)` | `def greet("hello", arg2)` |
| `greet(world, R)` | `def greet("world", arg2)` |
| `check(42, X)` | `def check(42, arg2)` |
| `process(X, Y)` | `def process(arg1, arg2)` |

Variable arguments become named parameters (`arg1`, `arg2`, ...) that are
available throughout the function body.

### Arithmetic in Bodies

The `is/2` predicate translates to Elixir assignment with arithmetic:

```prolog
add_nums(X, Y, R) :- R is X + Y.
```

→

```elixir
def add_nums(arg1, arg2, arg3) do
  try do
    arg3 = (arg1 + arg2)
    {:ok, [arg1, arg2, arg3]}
  catch
    :fail -> :fail
  end
end
```

```elixir
iex> Generated.AddNums.add_nums(10, 20, nil)
{:ok, [10, 20, 30]}
```

### Comparison Guards

Prolog comparison goals become `if not (...), do: throw(:fail)` checks:

```prolog
check_positive(X) :- X > 0.
```

→

```elixir
def check_positive(arg1) do
  try do
    if not (arg1 > 0), do: throw(:fail)
    {:ok, [arg1]}
  catch
    :fail -> :fail
  end
end
```

```elixir
iex> Generated.CheckPositive.check_positive(42)
{:ok, [42]}

iex> Generated.CheckPositive.check_positive(-1)
:fail
```

The `try/catch` pattern maps Prolog's failure semantics: when a goal fails, the
clause `:fail`s, just like Prolog backtracking would skip to the next clause.

---

**←** [Previous: Introduction](01_introduction.md) | **→** [Next: Recursion](03_recursion.md)
