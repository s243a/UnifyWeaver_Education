# Chapter 3: Recursion Patterns

The BEAM VM provides native tail-call optimization, making Elixir an excellent
target for recursive Prolog predicates. Unlike Java or Python, there's no need
to manually rewrite recursion into loops.

## Tail Recursion

Prolog tail recursion compiles to multi-clause `def` with an accumulator helper.

### Source Prolog

```prolog
sum_list([], Acc, Acc).
sum_list([H|T], Acc, Result) :- Acc1 is Acc + H, sum_list(T, Acc1, Result).
```

### Generated Elixir (via multifile dispatch)

```prolog
?- tail_recursion:compile_tail_pattern(elixir, sum_list, 3, ..., Code).
```

```elixir
defmodule Generated.SumList do
  def sum_list(arg1, arg2, arg3) do
    {arg1, arg2, arg3}
  end

  @doc "Tail-recursive helper with accumulator"
  def sum_list_acc([], acc), do: acc
  def sum_list_acc([item | rest], acc) do
    sum_list_acc(rest, acc + item)
  end
end
```

### Running It

```elixir
iex> Generated.SumList.sum_list_acc([10, 20, 30], 0)
60
```

BEAM compiles this to a tight loop internally — no stack growth regardless
of list size.

## Linear Recursion (Fibonacci)

Linear recursion produces simple multi-clause functions.

### Source Prolog

```prolog
fib(0, 0).
fib(1, 1).
fib(N, R) :- N > 1, N1 is N - 1, N2 is N - 2,
             fib(N1, R1), fib(N2, R2), R is R1 + R2.
```

### Generated Elixir

```prolog
?- linear_recursion:compile_linear_pattern(elixir, fib, 2, ..., Code).
```

```elixir
defmodule Generated.Fib do
  def fib(0), do: 0
  def fib(1), do: 1
  def fib(n) when n > 1 do
    fib(n - 1) + fib(n - 2)
  end
end
```

### Running It

```elixir
iex> Generated.Fib.fib(10)
55

iex> Generated.Fib.fib(20)
6765
```

### Idiomatic Improvement

For production use, you'd want memoization. In Elixir, this is typically done
with `Agent` or ETS:

```elixir
defmodule Fib.Memo do
  use Agent

  def start, do: Agent.start_link(fn -> %{0 => 0, 1 => 1} end, name: __MODULE__)

  def fib(n) do
    case Agent.get(__MODULE__, &Map.get(&1, n)) do
      nil ->
        result = fib(n - 1) + fib(n - 2)
        Agent.update(__MODULE__, &Map.put(&1, n, result))
        result
      val -> val
    end
  end
end
```

## Mutual Recursion

Mutually recursive predicates are grouped into a single `defmodule`:

### Source Prolog

```prolog
is_even(0).
is_even(N) :- N > 0, N1 is N - 1, is_odd(N1).
is_odd(1).
is_odd(N) :- N > 1, N1 is N - 1, is_even(N1).
```

### Generated Elixir

```prolog
?- compile_mutual_recursion_elixir([is_even/1, is_odd/1], [], Code).
```

```elixir
defmodule Generated.MutualGroup do
  def is_even(0) do
    try do
      {:ok, [0]}
    catch
      :fail -> :fail
    end
  end

  def is_odd(1) do
    try do
      {:ok, [1]}
    catch
      :fail -> :fail
    end
  end
end
```

### Running It

```elixir
iex> Generated.MutualGroup.is_even(0)
{:ok, [0]}

iex> Generated.MutualGroup.is_odd(1)
{:ok, [1]}
```

## Transitive Closure (BFS)

Transitive closure uses breadth-first search with `Map` and `MapSet`:

### Source Prolog

```prolog
parent(tom, bob).
parent(bob, jim).
parent(jim, ann).
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

### Generated Elixir

```prolog
?- recursive_compiler:compile_transitive_closure(elixir, ancestor, 2, parent, [], Code).
```

```elixir
defmodule Generated.AncestorQuery do
  def add_fact(rel, from, to) do
    Map.update(rel, from, [to], &([to | &1]))
  end

  def find_all(start, rel) do
    bfs([start], MapSet.new([start]), [], rel)
  end

  defp bfs([], _visited, results, _rel), do: Enum.reverse(results)
  defp bfs([current | queue], visited, results, rel) do
    nexts = Map.get(rel, current, [])
    new_nexts = Enum.reject(nexts, &MapSet.member?(visited, &1))
    new_visited = Enum.reduce(new_nexts, visited, &MapSet.put(&2, &1))
    bfs(queue ++ new_nexts, new_visited, Enum.reverse(new_nexts) ++ results, rel)
  end

  def check(start, target, rel) do
    target in find_all(start, rel)
  end
end
```

### Running It

```bash
$ printf "tom:bob\nbob:jim\njim:ann\n" | elixir ancestor_query.exs tom
tom:bob
tom:jim
tom:ann

$ printf "tom:bob\nbob:jim\njim:ann\n" | elixir ancestor_query.exs tom ann
tom:ann
```

The BFS approach guarantees all reachable nodes are found without infinite loops,
even in cyclic graphs.

## Multifile Dispatch Architecture

The Elixir target registers multifile clauses following the pattern established
by the R target:

```prolog
:- multifile tail_recursion:compile_tail_pattern/9.
:- multifile linear_recursion:compile_linear_pattern/8.
:- multifile mutual_recursion:compile_mutual_pattern/5.
```

This allows the recursion analysis modules in `core/advanced/` to dispatch to
target-specific code generators without coupling. When the analyzer detects
e.g. tail recursion and the target is `elixir`, it calls the Elixir-specific
clause automatically.

---

**←** [Previous: Facts and Rules](02_facts_and_rules.md) | **→** [Next: Pipelines](04_pipelines.md)

## Continuations And Failure In An Elixir-Shaped WAM

Elixir is useful because it makes control flow visible. A WAM runtime has to
answer a simple question after every goal: what happens on success, and what
happens on failure? In an Elixir-shaped lowering, that can be modeled with
small continuation functions or segments.

A two-goal rule such as:

```prolog
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

can be thought of as three pieces:

| Segment | Responsibility |
|---|---|
| Head segment | Read `X` and `Y` from argument registers. |
| First call continuation | Call `parent(X, Z)` and preserve `Z` for the next segment. |
| Tail continuation | Call `ancestor(Z, Y)` or fail into the next choice point. |

The target does not need to copy Go's explicit dispatch loop or Rust's state
layout. It still has to preserve the same WAM contract: bindings must be
trailed, alternatives must be reachable, and a failed branch must resume at the
right continuation or choice point.

This is also where BEAM-specific options become interesting. A future pure,
aggregate-shaped search can use BEAM processes for parallel alternatives, but
only when the calling context can safely merge all branch results. Direct
`next_solution`-style enumeration still needs sequential choice-point behavior
so it does not lose answers.

## Hybrid WAM Role

Elixir is a good teaching target for continuation-oriented WAM execution.
The BEAM encourages explicit process and continuation thinking, so Elixir
examples can explain how recursive logic, choice points, and failure paths
can be represented without pretending every target shares the same runtime
shape.

- Default generation path: target-ready WAM data should feed the Elixir
  emitter directly.
- Symbolic WAM text: useful as a readable listing, not as the normal internal
  bridge.
- Target-specific emphasis: CPS-style control flow, failure handling, and
  future strategy-menu choices such as BEAM-native parallel search.
