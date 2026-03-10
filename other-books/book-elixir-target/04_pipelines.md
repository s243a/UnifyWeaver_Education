# Chapter 4: Pipelines and Generators

Elixir's `|>` pipe operator and `Stream` module make it a natural fit for
Unix-style JSONL stream processing.

## Pipeline Mode

Pipeline mode generates a module that reads JSONL from stdin, processes each
record, and writes results to stdout.

### Compilation

```prolog
?- compile_predicate_to_elixir(process_log/2, [pipeline_input(true)], Code).
```

### Generated Elixir

```elixir
defmodule Generated.ProcessLogPipeline do

  def process(record) do
    record
  end

  def run do
    IO.stream(:stdio, :line)
    |> Stream.reject(&(&1 == "\n"))
    |> Stream.map(&String.trim/1)
    |> Stream.map(&Jason.decode!/1)
    |> Stream.map(&process/1)
    |> Stream.reject(&is_nil/1)
    |> Enum.each(&(IO.puts(Jason.encode!(&1))))
  end
end

case System.argv() do
  [] -> :ok
  ["--run"|_] -> Generated.ProcessLogPipeline.run()
  _ -> :ok
end
```

### How It Works

The pipeline is a chain of lazy `Stream` transformations:

```
stdin (line by line)
  │
  ├── reject empty lines
  ├── trim whitespace
  ├── parse JSON (Jason.decode!)
  ├── apply process/1 function
  ├── reject nil results (failed clauses)
  └── encode and print to stdout
```

Each step is lazy — records are processed one at a time, not loaded into memory.

### Running It

```bash
echo '{"name": "alice", "score": 95}
{"name": "bob", "score": 42}' | elixir pipeline.exs --run
```

### Prerequisites

Pipeline mode requires the `Jason` library. In a Mix project:

```elixir
# mix.exs
defp deps do
  [{:jason, "~> 1.4"}]
end
```

For scripts, you can use `Mix.install`:

```elixir
Mix.install([{:jason, "~> 1.4"}])
```

## Generator Mode

Generator mode wraps a pipeline in `Stream.unfold/2` for lazy, potentially
infinite sequences.

### Compilation

```prolog
?- compile_predicate_to_elixir(evolve/2, [generator_mode(true)], Code).
```

### Generated Elixir

```elixir
# (Pipeline module is generated first, then the wrapper:)

defmodule Generated.EvolveGen do
  def generate(init_record) do
    Stream.unfold(init_record, fn record ->
      case Generated.EvolvePipeline.process(record) do
        nil -> nil
        next -> {next, next}
      end
    end)
  end
end
```

### How `Stream.unfold` Works

`Stream.unfold/2` takes an initial accumulator and a function that produces
`{element, next_accumulator}` or `nil` to stop:

```
init_record
    │
    ├── process(record) → next
    │   emit: next
    │   new acc: next
    │
    ├── process(next) → next2
    │   emit: next2
    │   new acc: next2
    │
    ├── process(next2) → nil
    │   stop
    └──
```

### Running It

```elixir
# Take the first 5 iterations of a state evolution
Generated.EvolveGen.generate(%{"state" => 0})
|> Enum.take(5)
|> Enum.each(&IO.inspect/1)
```

## Pipeline vs. Generator

| Feature | Pipeline | Generator |
|---------|----------|-----------|
| Input | stdin JSONL | Initial record |
| Output | stdout JSONL | Lazy `Stream` |
| Execution | `run/0` consumes all input | `generate/1` is lazy |
| Use case | Unix pipes, ETL | State machines, simulations |
| Termination | EOF on stdin | `process/1` returns `nil` |

## Combining with Facts

You can combine fact export with pipeline processing:

```prolog
% Export facts
?- compile_facts_to_elixir(allowed_users, 1, FactsCode).

% Generate pipeline that filters using facts
?- compile_predicate_to_elixir(filter_user/2, [pipeline_input(true)], PipeCode).
```

```elixir
# In the process/1 function, reference the facts module:
def process(record) do
  if Generated.AllowedUsers.member?({record["name"]}) do
    record
  else
    nil
  end
end
```

---

**←** [Previous: Recursion](03_recursion.md) | **→** [Next: Bindings](05_bindings.md)
