# Elixir Target for UnifyWeaver

Compile Prolog predicates to idiomatic Elixir code that runs on the BEAM VM.

## Chapters

1. **[Introduction](01_introduction.md)** — Why Elixir, BEAM pattern matching, module naming
2. **[Facts and Rules](02_facts_and_rules.md)** — Facts → module attributes, rules → multi-clause `def`
3. **[Recursion](03_recursion.md)** — Tail, linear, mutual recursion, transitive closure BFS
4. **[Pipelines and Generators](04_pipelines.md)** — JSONL streaming, `Stream.unfold`
5. **[Bindings](05_bindings.md)** — Arithmetic, comparison, string, I/O mappings

## Prerequisites

- SWI-Prolog
- Elixir ≥ 1.14 / Erlang/OTP ≥ 25

```bash
# Ubuntu/Debian
sudo apt install elixir

# macOS
brew install elixir
```

## Quick Example

```prolog
?- compile_facts_to_elixir(parent, 2, Code), write(Code).
```

→

```elixir
defmodule Generated.Parent do
  @facts [
    {"tom", "bob"},
    {"bob", "jim"}
  ]

  def all, do: @facts
  def member?(args), do: args in @facts
  def stream, do: Stream.map(@facts, & &1)
end
```

## Generating Elixir Hybrid WAM Code

Use the WAM Elixir target when you want CPS-style WAM execution rather than the
plain Elixir target:

```prolog
?- use_module('src/unifyweaver/targets/wam_elixir_target').
?- write_wam_elixir_project([ancestor/2],
       [emit_mode(auto), module_name('AncestorWam')],
       'out/wam_elixir_ancestor').
```

`emit_mode(auto)` lets the target choose between interpreted WAM data and
lowered Elixir helpers where supported. Parser-dependent builtins currently
require capability checks; do not assume `runtime_parser(compiled)` is
available for Elixir.

## Hybrid WAM Role

Elixir is useful for explaining CPS-style WAM execution, continuation
boundaries, and future BEAM-native strategy choices. Book 17 covers the shared
Hybrid WAM model; this book shows where that model meets BEAM control flow.

## Why Elixir?

| Feature | Benefit |
|---------|---------|
| Pattern matching in function heads | Natural fit for Prolog clauses |
| Native tail-call optimization | BEAM handles TCO automatically — no manual loop rewriting |
| Immutability | Matches Prolog's logical variable semantics |
| `Stream` module | Lazy evaluation for backtracking-like patterns |
| OTP/Actors | Built-in concurrency for distributed Prolog execution |
| CamelCase modules | `snake_to_camel/2` converts predicate names idiomatically |
