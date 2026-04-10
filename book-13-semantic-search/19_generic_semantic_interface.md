<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025-2026 John William Creighton (s243a)
-->

# Chapter 19: Generic Semantic Interface

*One Prolog spec, five target languages, hardware-aware code generation*

## Overview

Earlier chapters showed Python-specific semantic search via `PtSearcher` and the Python runtime. But what if you want the same Prolog predicate to compile to Go, Rust, C#, or Elixir — each using their idiomatic ML libraries and hardware acceleration?

The **generic semantic interface** (`semantic_compiler.pl`) provides exactly that. You declare a semantic predicate once, specify which providers to use per target, and the transpiler generates target-specific code with automatic device fallback.

## 19.1 Why a Generic Interface?

The ML ecosystem is fragmented across languages:

| Language | Idiomatic library | GPU support |
|----------|-------------------|-------------|
| Python | sentence-transformers / transformers | CUDA, MPS, CPU |
| Go | hugot | GPU, CPU |
| Rust | candle, ort | CUDA, CPU |
| C# | ONNX Runtime | DirectML, CPU |
| Elixir | Bumblebee + Nx + EXLA | CUDA, CPU |

Without a generic interface, each target needs its own predicate. With one, the same Prolog logic compiles to whichever target your deployment needs:

```prolog
% Declare once
:- semantic_provider(find_similar/3, [
    targets([
        target(go,      [provider(hugot),        model('all-MiniLM-L6-v2'), device(gpu)]),
        target(python,  [provider(transformers), model('all-MiniLM-L6-v2'), device(mps)]),
        target(rust,    [provider(candle),       model('all-MiniLM-L6-v2'), device(gpu)]),
        target(csharp,  [provider(onnx),         model('all-MiniLM-L6-v2'), device(gpu)]),
        target(elixir,  [provider(bumblebee),    model('all-MiniLM-L6-v2'), device(cpu)])
    ]),
    fallback([provider(onnx), model('all-MiniLM-L6-v2')])
]).

% Use anywhere
my_logic(Query, Top) :-
    find_similar(Query, 10, Results),
    member(Top, Results).
```

The transpiler picks the right code generator per target.

## 19.2 Architecture: Multifile Dispatch

The interface is built on Prolog's **multifile predicates**. A single hook is declared centrally:

```prolog
:- multifile semantic_dispatch/5.
% semantic_dispatch(+Target, +Goal, +ProviderInfo, +VarMap, -Code)
```

Each target file adds clauses for its own target name:

```prolog
% In python_target.pl
semantic_compiler:semantic_dispatch(python, Goal, Provider, VarMap, Code) :-
    Goal =.. [_, Query, TopK | _],
    option(provider(transformers), Provider),
    option(model(Model), Provider, 'all-MiniLM-L6-v2'),
    option(device(Device), Provider, auto),
    % Lookup variable names in VarMap
    (member(Query=QueryVar, VarMap) -> QueryExpr = QueryVar ; QueryExpr = Query),
    (member(TopK=TopKVar, VarMap) -> TopKExpr = TopKVar ; TopKExpr = TopK),
    % Device-aware initialization
    (Device == gpu -> DeviceStr = "cuda"
    ;Device == mps -> DeviceStr = "mps"
    ;Device == cpu -> DeviceStr = "cpu"
    ;DeviceStr = "auto"),
    format(string(Code), '
import torch
from sentence_transformers import SentenceTransformer
device = "~w"
if device == "cuda" and not torch.cuda.is_available():
    device = "mps" if torch.backends.mps.is_available() else "cpu"
elif device == "mps" and not torch.backends.mps.is_available():
    device = "cpu"
model = SentenceTransformer("~w", device=device)
query_emb = model.encode(~w, convert_to_numpy=True)
results = searcher.search(query_emb, top_k=~w)
', [DeviceStr, Model, QueryExpr, TopKExpr]).
```

The dispatch resolves at compile time. The user's code never has to know which target it's compiling for.

## 19.3 Hardware-Aware Code Generation

Each target generates initialization code with **automatic CPU fallback**. The user requests `device(gpu)`, but the generated code handles unavailable hardware gracefully.

### Python (transformers + PyTorch)

```python
device = "cuda"
if device == "cuda" and not torch.cuda.is_available():
    print("Warning: CUDA not available, trying MPS or CPU")
    device = "mps" if torch.backends.mps.is_available() else "cpu"
elif device == "mps" and not torch.backends.mps.is_available():
    device = "cpu"
model = SentenceTransformer("all-MiniLM-L6-v2", device=device)
```

### Go (hugot)

```go
emb, err := embedder.NewHugotEmbedder("models/all-MiniLM-L6-v2-onnx", "all-MiniLM-L6-v2", embedder.WithGPU())
if err != nil {
    log.Printf("Warning: GPU initialization failed, falling back to CPU: %v", err)
    emb, err = embedder.NewHugotEmbedder("models/all-MiniLM-L6-v2-onnx", "all-MiniLM-L6-v2", embedder.WithCPU())
    if err != nil { log.Fatal(err) }
}
```

### Rust (candle)

```rust
let device = candle_core::Device::new_cuda(0)
    .unwrap_or_else(|_| {
        eprintln!("Warning: CUDA not available, falling back to CPU");
        candle_core::Device::Cpu
    });
let searcher = PtSearcher::new("data.redb", "all-MiniLM-L6-v2", &device)?;
```

### C# (ONNX Runtime)

```csharp
var opts = new SessionOptions();
try {
    opts.AppendExecutionProvider_DML(); // DirectML for Windows GPU
} catch (Exception ex) {
    Console.WriteLine($"Warning: GPU initialization failed: {ex.Message}. Falling back to CPU.");
    opts.AppendExecutionProvider_CPU();
}
var searcher = new OnnxVectorSearch("data.db", "models/all-MiniLM-L6-v2-onnx", opts);
```

### Elixir (Bumblebee + EXLA)

```elixir
{:ok, model} = Bumblebee.load_model({:hf, "all-MiniLM-L6-v2"}, backend: {EXLA.Backend, client: :cuda})
{:ok, tokenizer} = Bumblebee.load_tokenizer({:hf, "all-MiniLM-L6-v2"})
serving = Bumblebee.Text.TextEmbedding.text_embedding(model, tokenizer,
    compile: [batch_size: 1],
    defn_options: [compiler: EXLA])
%{embedding: query_emb} = Nx.Serving.run(serving, query)
```

## 19.4 Inline Search Options

Beyond the per-target provider config, individual queries can override settings via `semantic_search/4`:

```prolog
% Use the default provider config
?- find_similar(query, 10, Results).

% Override specific options for this call
?- find_similar(query, 10, Results, [
    threshold(0.7),
    model('all-mpnet-base-v2'),
    index("custom_index.db")
]).
```

The `extract_search_options/2` and `merge_provider_options/3` predicates in `semantic_compiler.pl` handle the merge: inline options take precedence over provider defaults.

## 19.5 Vector Database Sources

Semantic search needs a place to store embeddings. The `input_source.pl` module supports a `vector_db` mode:

```prolog
?- compile_predicate_to_python(find_similar/3, [
    input(vector_db("embeddings.db"))
], Code).
```

This generates target-specific initialization:

| Target | Generated init |
|--------|---------------|
| Python | `_db = sqlite3.connect("embeddings.db")` |
| Go | `store, err := storage.NewStore("embeddings.db")` |
| Rust | `let store = Store::open("embeddings.db")?;` |
| C# | `using var store = new VectorStore("embeddings.db");` |
| Elixir | `{:ok, store} = VectorStore.open("embeddings.db")` |

## 19.6 Fuzzy Logic Integration

The fuzzy DSL (Chapter 17) is also wired through the generic interface. The `fuzzy_dispatch/3` multifile hook lets each target compile fuzzy operations to idiomatic code:

```prolog
:- multifile fuzzy_dispatch/3.
% fuzzy_dispatch(+Target, +Goal, -Code)
```

A single fuzzy expression compiles differently per target:

**Python (NumPy):**
```python
result = f_and([("bash", 0.9), ("shell", 0.5)], _term_scores)
```

**Go (inline math):**
```go
result := 1.0
result *= 0.9 * termScores["bash"]
result *= 0.5 * termScores["shell"]
```

**Rust (HashMap + iterators):**
```rust
let mut result: f64 = 1.0;
result *= 0.9 * term_scores.get("bash").copied().unwrap_or(0.5);
result *= 0.5 * term_scores.get("shell").copied().unwrap_or(0.5);
```

**C# (Dictionary + LINQ):**
```csharp
double result = 1.0;
result *= 0.9 * (termScores.TryGetValue("bash", out var s_bash) ? s_bash : 0.5);
result *= 0.5 * (termScores.TryGetValue("shell", out var s_shell) ? s_shell : 0.5);
```

**Elixir (pipe operator):**
```elixir
result = 1.0
  |> Kernel.*(0.9 * Map.get(term_scores, "bash", 0.5))
  |> Kernel.*(0.5 * Map.get(term_scores, "shell", 0.5))
```

All five targets support the same operations: `f_and`, `f_or`, `f_dist_or`, `f_union`, `f_not`, `blend_scores`, `top_k`. Python and Go also support batch variants for vectorized processing.

## 19.7 Cross-Runtime Pipelines

The generic interface integrates with `cross_runtime_pipeline.pl`, allowing semantic and fuzzy operations to appear in multi-language pipeline stages:

```prolog
?- compile_cross_runtime_pipeline([
    go:embed_query/2,
    rust:fuzzy_rank/3,
    python:visualize/1
], [pipeline_name(my_pipeline)], _).
```

Each stage gets compiled with its target's semantic/fuzzy dispatch. The pipeline orchestrator handles inter-stage communication via JSONL streams.

## 19.8 Adding a New Target

To add semantic search for a new target language:

1. **Add the dispatch hook** in your target file:
   ```prolog
   :- multifile semantic_compiler:semantic_dispatch/5.
   semantic_compiler:semantic_dispatch(my_target, Goal, Provider, VarMap, Code) :-
       % Generate target-specific code
   ```

2. **Add fuzzy dispatch** (optional):
   ```prolog
   :- multifile semantic_compiler:fuzzy_dispatch/3.
   semantic_compiler:fuzzy_dispatch(my_target, f_and(Terms, _), Code) :-
       % Generate target-specific fuzzy AND
   ```

3. **Add input_source support** (optional):
   ```prolog
   :- multifile input_source:vector_db_init_code/3.
   input_source:vector_db_init_code(my_target, vector_db(Path, _), Code) :-
       format(string(Code), 'open_db("~w")', [Path]).
   ```

4. **Register in the test suite** to verify dispatch works correctly.

That's it. No changes to `semantic_compiler.pl` itself — the multifile hooks make extension trivial.

## 19.9 Summary

The generic semantic interface decouples semantic search **logic** (declarative Prolog) from **execution** (target-specific generated code). This enables:

- **Polyglot deployments** — same predicate, different targets
- **Hardware portability** — automatic GPU/MPS/CPU fallback
- **Provider flexibility** — choose the best library per language
- **Query-level overrides** — inline options via `semantic_search/4`
- **Incremental adoption** — add targets without touching existing code

The interface is just one piece of the picture. For computing semantic *distances* over graph structures, UnifyWeaver provides a deeper optimization: the **recursive kernel system**, which can lower entire algorithm patterns to native target code. That's the subject of Chapter 20.
