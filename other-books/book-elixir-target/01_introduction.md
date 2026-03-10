# Chapter 1: Introduction to the Elixir Target

## Why Elixir for Prolog?

Elixir runs on the BEAM VM, which was designed for Erlang — a language that shares
deep roots with Prolog. Both use pattern matching as a primary dispatch mechanism,
both have immutable data, and both excel at concurrent, fault-tolerant computing.

| Prolog | Elixir |
|--------|--------|
| Facts + Rules | Data + Functions |
| Clause selection | Multi-clause `def` with pattern matching |
| Backtracking | `Stream` / lazy enumeration |
| Unification | Pattern matching + `=` match operator |
| Tail recursion | Native BEAM TCO (no manual rewriting) |
| `assert`/`retract` | Agent / ETS (mutable state) |

## Architecture

```
┌───────────────────────────────────────┐
│        UnifyWeaver (Prolog)           │
│  compile_predicate_to_elixir/3        │
└─────────────────┬─────────────────────┘
                  │
                  ▼
           Generated.MyPred (Elixir)
                  │
                  ▼
         elixir generated.exs
         elixirc generated.ex
                  │
                  ▼
         BEAM bytecode / script
```

## Module Naming: `snake_to_camel/2`

Prolog predicate names are `snake_case` by convention. Elixir module names must
be `CamelCase` (they're actually Erlang atoms prefixed with `Elixir.`). UnifyWeaver
converts automatically:

```prolog
?- snake_to_camel(my_parent, X).
X = 'MyParent'.

?- snake_to_camel(elix_greet, X).
X = 'ElixGreet'.

?- snake_to_camel(ancestor, X).
X = 'Ancestor'.
```

The generated module is always namespaced under `Generated.`:

```elixir
defmodule Generated.MyParent do
  # ...
end
```

## Compilation Modes

The Elixir target supports five compilation modes, selected by options:

| Mode | Option | Use Case |
|------|--------|----------|
| **Simple** | *(default)* | Pattern-matched `def` clauses |
| **Facts** | `type(facts)` | Module attributes with `@facts` |
| **Pipeline** | `pipeline_input(true)` | JSONL stream processing |
| **Generator** | `generator_mode(true)` | Lazy `Stream.unfold` sequences |
| **Mutual** | `type(mutual_recursion)` | Grouped `defmodule` |

```prolog
% Simple mode (default)
?- compile_predicate_to_elixir(greet/2, [], Code).

% Facts mode
?- compile_predicate_to_elixir(parent/2, [type(facts)], Code).

% Pipeline mode
?- compile_predicate_to_elixir(process/2, [pipeline_input(true)], Code).
```

## Running Generated Code

Elixir scripts use the `.exs` extension (interpreted) or `.ex` (compiled to BEAM):

```bash
# Run as a script
elixir generated.exs

# Compile to BEAM bytecode
elixirc generated.ex
```

---

**→** [Next: Facts and Rules](02_facts_and_rules.md)
