<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025-2026 John William Creighton (s243a)
-->

# Chapter 20: Recursive Kernels and Foreign Lowering

*Pattern-matched Prolog clauses become native algorithms via the Rust hybrid WAM FFI*

## Overview

Chapter 19 covered code generation for semantic search *operations* (embedding lookup, similarity computation). This chapter covers something deeper: the **recursive kernel system**, which recognizes entire *algorithm patterns* in Prolog clauses and lowers them to native target implementations.

The motivating example: minimum semantic distance. A user writes a clean declarative Prolog spec:

```prolog
semantic_path_cost(X, Y, _Visited, W) :-
    edge_weight(X, Y, W).
semantic_path_cost(X, Y, Visited, Cost) :-
    edge_weight(X, Z, W),
    \+ member(Z, Visited),
    semantic_path_cost(Z, Y, [Z|Visited], RestCost),
    Cost is W + RestCost.

min_semantic_dist(Start, Target, MinDist) :-
    aggregate_all(min(Cost),
        semantic_path_cost(Start, Target, [Start], Cost),
        MinDist).
```

Pure Prolog enumerates *all* paths exhaustively — exponential time, unusable beyond ~200 edges. But the transpiler recognizes this clause structure as a **weighted shortest path** problem and lowers it to a native Dijkstra implementation in Rust. Same logic, O((V+E) log V) instead of O(paths).

## 20.1 The Three-Tier Lowering Strategy

When the Rust target compiles a predicate, it tries three strategies in order:

```
┌─────────────────────────────────────────────────────────┐
│  Tier 1: Native pattern (form, list, math)              │
│   ↓ if matched, emit hand-crafted Rust function         │
│   ↓ otherwise...                                        │
├─────────────────────────────────────────────────────────┤
│  Tier 2: Recursive kernel (FFI lowering)                │
│   ↓ if pattern matched, register native handler         │
│   ↓ via foreign function interface                      │
│   ↓ otherwise...                                        │
├─────────────────────────────────────────────────────────┤
│  Tier 3: WAM compilation                                │
│     Compile Prolog → WAM bytecode → Rust runtime        │
└─────────────────────────────────────────────────────────┘
```

The kernel system is **Tier 2**. It sits between hand-coded native patterns and the general-purpose WAM. When a clause structure matches a known algorithm pattern, the transpiler:

1. Extracts configuration data (edge weights, dimensions, etc.) from the Prolog facts
2. Registers a native Rust handler for the predicate
3. Sets up data structures (HashMaps, indexed adjacency lists) at startup
4. Compiles the Prolog call site into a foreign function call

The WAM runtime hands control to the native handler when the predicate is invoked, gets results back, and resumes Prolog execution.

## 20.2 The Kernel Catalog

Currently registered kernels:

| Kernel | Arity | Pattern | Native algorithm |
|--------|-------|---------|-----------------|
| `category_ancestor` | 4 | DFS with visited set + max depth | Bounded DFS |
| `countdown_sum2` | 2 | Arithmetic accumulation | Iterative loop |
| `list_suffix2` | 2 | List slicing | Slice operations |
| `transitive_closure2` | 2 | `Pred(X,Y) :- Edge(X,Y)` + recursive | DFS reachability |
| `transitive_distance3` | 3 | Hop-counting distance | DFS with depth |
| `transitive_parent_distance4` | 4 | Distance + parent tracking | DFS with parent map |
| `weighted_shortest_path3` | 3 | `Pred + edge weights, sum costs` | **Dijkstra (BinaryHeap)** |
| `astar_shortest_path4` | 4 | Weighted path + heuristic + dim | **A\* with f(n) = g^D + h^D** |

The two highlighted ones — `weighted_shortest_path3` and `astar_shortest_path4` — were added specifically for the semantic distance work. We'll trace through how they're built.

## 20.3 Anatomy of a Kernel: weighted_shortest_path3

A kernel has five components in `rust_target.pl`:

### 1. The detector (clause-structure matcher)

```prolog
rust_recursive_kernel_detector(weighted_shortest_path3, rust_recursive_kernel_weighted_shortest_path).

rust_foreign_lowerable_weighted_shortest_path(Pred, 3, Clauses, WeightPred/3, FactTriples) :-
    % Find a base clause: Pred(X, Y, W) :- WeightPred(X, Y, W).
    member(BaseHead-BaseBody, Clauses),
    BaseHead =.. [Pred, BaseStart, BaseTarget, BaseWeight],
    BaseBody =.. [WeightPred, BaseStart, BaseTarget, BaseWeight],

    % Find a recursive clause: Pred(X, Y, C) :- WeightPred(X, Z, W), Pred(Z, Y, RC), C is W+RC.
    member(RecHead-RecBody, Clauses),
    BaseHead \== RecHead,
    RecHead =.. [Pred, RecStart, RecTarget, RecCost],
    extract_weighted_rec_body(Pred, WeightPred, RecStart, RecTarget, RecCost, RecBody),

    % Extract all weighted edge facts from the database
    findall(Left-Right-Weight,
        ( functor(WHead, WeightPred, 3),
          user:clause(WHead, true),
          WHead =.. [WeightPred, Left, Right, Weight],
          atom(Left), atom(Right), number(Weight)
        ),
        FactTriples),
    FactTriples \= [].
```

The detector looks at the clause AST. It needs:
- A base case with the right shape: `Pred(X,Y,W) :- WeightPred(X,Y,W)`
- A recursive case with the right body structure (with optional `\+ member` for cycle detection)
- Concrete weighted edge facts in the database

If everything matches, it returns `(WeightPred, FactTriples)` — the predicate name and all the data needed.

### 2. The kernel record

```prolog
rust_recursive_kernel_weighted_shortest_path(Pred, Arity, Clauses,
        recursive_kernel(weighted_shortest_path3, Pred/Arity,
            [weight_pred(WeightPred/3), fact_triples(FactTriples)])) :-
    rust_foreign_lowerable_weighted_shortest_path(Pred, Arity, Clauses,
        WeightPred/3, FactTriples).
```

This wraps the detected pattern into a `recursive_kernel(...)` term that flows through the rest of the compilation pipeline.

### 3. Result layout & mode

```prolog
rust_recursive_kernel_native_kind(weighted_shortest_path3, weighted_shortest_path3).
rust_recursive_kernel_result_layout(weighted_shortest_path3, tuple(2)).
rust_recursive_kernel_result_mode(weighted_shortest_path3, stream).
```

This declares:
- The native handler key (`weighted_shortest_path3`)
- Result layout (`tuple(2)` = pairs of `(node, distance)`)
- Result mode (`stream` = nondeterministic, returns multiple solutions)

### 4. Setup operations (data registration)

```prolog
rust_recursive_kernel_config_ops(weighted_shortest_path3, PredIndicator,
        [weight_pred(WeightPred/3), fact_triples(FactTriples)],
        [ register_foreign_string_config(PredIndicator, weight_pred, WeightPred/3),
          register_indexed_weighted_edge(WeightPred/3, FactTriples)
        ]).
```

These setup ops generate Rust code in the WAM VM's `init` method:

```rust
vm.register_foreign_string_config("min_semantic_dist/3", "weight_pred", "edge_weight/3");
vm.register_indexed_weighted_edge_triples("edge_weight/3", &[
    ("ml", "ai", 0.12),
    ("ai", "cs", 0.18),
    ("cs", "science", 0.30),
    // ... all weighted edges as Rust literals
]);
```

The runtime stores this in:

```rust
indexed_weighted_edge: HashMap<String, HashMap<String, Vec<(String, f64)>>>
```

This is an adjacency list keyed by predicate name and source node.

### 5. The native handler in `wam_rust_target.pl`

```prolog
compile_collect_native_weighted_shortest_path_to_rust(Code) :-
    Code = '    /// Dijkstra shortest path with precomputed semantic edge weights.
    pub fn collect_native_weighted_shortest_path_results(
        &self,
        start: &str,
        weight_pred: &str,
        out: &mut Vec<(String, f64)>,
    ) {
        use std::collections::BinaryHeap;
        use std::cmp::Ordering;

        #[derive(PartialEq)]
        struct State { cost: f64, node: String }
        impl Eq for State {}
        impl PartialOrd for State {
            fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
                other.cost.partial_cmp(&self.cost) // reversed for min-heap
            }
        }
        impl Ord for State {
            fn cmp(&self, other: &Self) -> Ordering {
                self.partial_cmp(other).unwrap_or(Ordering::Equal)
            }
        }

        let mut dist: HashMap<String, f64> = HashMap::new();
        let mut heap = BinaryHeap::new();
        dist.insert(start.to_string(), 0.0);
        heap.push(State { cost: 0.0, node: start.to_string() });

        while let Some(State { cost, node }) = heap.pop() {
            if let Some(&best) = dist.get(&node) {
                if cost > best { continue; }
            }
            if let Some(edges) = self.indexed_weighted_edge.get(weight_pred)
                .and_then(|table| table.get(&node))
            {
                for (next, weight) in edges {
                    let next_cost = cost + weight;
                    if next_cost < *dist.get(next).unwrap_or(&f64::INFINITY) {
                        dist.insert(next.clone(), next_cost);
                        heap.push(State { cost: next_cost, node: next.clone() });
                    }
                }
            }
        }

        for (node, cost) in &dist {
            if node != start {
                out.push((node.clone(), *cost));
            }
        }
    }'.
```

This is a textbook Dijkstra. The Prolog spec said *what* to compute; the kernel says *how*.

## 20.4 The WAM Foreign Function Bridge

Once the data is registered and the native handler exists, the WAM dispatches calls through `execute_foreign_predicate`:

```rust
"weighted_shortest_path3" => {
    // Extract A1=start, A2=target, A3=distance from registers
    let start = match self.regs.get("A1").cloned().map(|v| self.deref_var(&v)) {
        Some(Value::Atom(start)) => start,
        _ => return false,
    };
    let target_reg = match self.regs.get("A2").cloned() { ... };
    let dist_reg = match self.regs.get("A3").cloned() { ... };

    // Look up the configured weight predicate
    let weight_pred = self.foreign_string_config(&pred_key, "weight_pred")?;

    // Call the native algorithm
    let mut results: Vec<(String, f64)> = Vec::new();
    self.collect_native_weighted_shortest_path_results(&start, &weight_pred, &mut results);

    // Filter if a specific target was given
    if let Some(target) = target_filter {
        results.retain(|(node, _)| *node == target);
    }

    // Pack results as (node, f64) tuples and unify with output registers
    let packed: Vec<Value> = results.into_iter().map(|(node, dist)| {
        Value::Str("__pair__".to_string(), vec![
            Value::Atom(node),
            Value::Float(dist),
        ])
    }).collect();
    self.finish_foreign_results(&pred_key, vec![target_reg, dist_reg], packed)
}
```

This is the **foreign function interface**: WAM-side register reads, native-side computation, then WAM-side unification of results. The Prolog code that called `min_semantic_dist(ml, science, D)` sees `D = 0.60` as if it were computed by ordinary Prolog backtracking.

## 20.5 Adding Heuristics: A*

The A* kernel (`astar_shortest_path4`) extends the Dijkstra pattern with a heuristic and a dimensionality parameter:

```
priority f(n) = g(n)^D + h(n)^D
```

where `g(n)` is cost-so-far and `h(n)` is direct semantic distance to the target. For details on why D=1 is the empirical default, see the dimensionality benchmark in `examples/benchmark/benchmark_astar_dimensionality.py`.

The kernel infrastructure is the same — detector, kernel record, setup ops, native handler. The only differences:

- Config has additional `direct_dist_pred` and `dimensionality` keys
- The native handler maintains both `g_cost` and `f_cost` in heap entries
- It uses `indexed_weighted_edge` twice: once for actual edges, once for direct distances

```rust
let f_cost = |g: f64, h: f64| -> f64 {
    g.powf(dim) + h.powf(dim)
};

let heuristic = |node: &str| -> f64 {
    self.indexed_weighted_edge.get(direct_pred)
        .and_then(|table| table.get(node))
        .and_then(|edges| edges.iter().find(|(t, _)| t == target))
        .map(|(_, w)| *w)
        .unwrap_or(1.0)
};
```

When the user calls `min_semantic_dist_astar(ml, science, 1, D)`, the WAM dispatches to this handler, which uses Dijkstra-style expansion guided by the heuristic, terminating early when it pops the target from the queue.

## 20.6 Effective Semantic Distance: Aggregation Over Paths

`effective_semantic_dist/3` doesn't have its own kernel. It uses the `aggregate_all(sum, ...)` pattern with `semantic_path_cost/4` underneath:

```prolog
effective_semantic_dist(Start, Target, N, Deff) :-
    NegN is -N,
    aggregate_all(sum(W),
        ( semantic_path_cost(Start, Target, [Start], Cost),
          W is Cost ** NegN ),
        WeightSum),
    WeightSum > 0,
    InvN is -1 / N,
    Deff is WeightSum ** InvN.
```

Currently this falls through to the WAM compiler (Tier 3). Adding a dedicated kernel for "all-paths sum aggregation with weighted edges" would let it run in native code as well — a natural extension for future work.

## 20.7 Why Rust First?

The kernel system currently exists only for the Rust target. The Python target has its own compilation modes (procedural and generator) but no WAM runtime and no foreign function interface yet.

This is a deliberate phasing:

| Target | Has WAM runtime | Has FFI lowering | Status |
|--------|----------------|------------------|--------|
| Rust | Yes (`wam_rust_target.pl`) | Yes (`rust_foreign_lowering_spec`) | **Full kernel support** |
| Python | No | No | Direct codegen via `python_target.pl` |
| Go | No | No | Direct codegen via `go_target.pl` |
| C# | No | No | Direct codegen via `csharp_target.pl` |
| Elixir | No | No | Direct codegen via `elixir_target.pl` |

Rust got the WAM treatment first because:

1. **Memory safety needs** — generated Rust must satisfy the borrow checker, which is much harder for arbitrary recursive Prolog. WAM bytecode is regular and easier to lower safely.
2. **Embedded deployment** — Rust targets are often constrained environments where dynamic structures matter. The WAM gives precise control over allocation.
3. **Hybrid execution** — patterns that lower to native handlers run at full speed; patterns that don't fall through to the WAM. You get the best of both.

The Python target instead generates idiomatic Python directly: streaming generators for pipelines, fixpoint loops for recursion, memoization for tree recursion. It's a different design philosophy — Python's runtime is forgiving enough that direct codegen works well.

If a future Python WAM is implemented, the kernel system would lift cleanly: the same `recursive_kernel(...)` records would dispatch through a Python-side multifile hook (`python_recursive_kernel_*`) and the native handlers would be Python functions instead of Rust ones. The `rust_target.pl` infrastructure is the blueprint.

## 20.8 Adding a New Kernel

To add a kernel for a new algorithm pattern, you need five things:

1. **A detector** — clause-shape matcher in `rust_target.pl`
2. **A native_kind / result_layout / result_mode** declaration
3. **A config_ops** clause that generates VM init code
4. **A WAM dispatch handler** in `wam_rust_target.pl` that reads registers and calls the native function
5. **A native function** that does the actual algorithm

The `weighted_shortest_path3` kernel was implemented in PR #1268 with about 200 lines of Prolog (detector + setup + dispatch) and ~50 lines of generated Rust (the Dijkstra function template). The A* kernel added another ~150 lines.

The pattern is the same for any algorithm: pattern-match a clause structure, extract its data, register it as a foreign predicate, generate the native handler. The difficulty is mostly in writing a robust pattern matcher that recognizes idiomatic Prolog without false positives.

## 20.9 Putting It All Together: A Real Pipeline

Here's how all the pieces fit:

```prolog
% 1. Declare the semantic interface (Chapter 19)
:- semantic_provider(embed_text/2, [
    targets([target(rust, [provider(candle), model('all-MiniLM-L6-v2')])])
]).

% 2. Specify the vector source
:- compile_predicate_to_rust(graph_search/3, [
    input(vector_db("embeddings.db"))
], _).

% 3. Define the search logic — the kernel detects this as weighted_shortest_path3
semantic_path_cost(X, Y, _, W) :- edge_weight(X, Y, W).
semantic_path_cost(X, Y, V, C) :-
    edge_weight(X, Z, W),
    \+ member(Z, V),
    semantic_path_cost(Z, Y, [Z|V], RC),
    C is W + RC.

graph_search(Start, Target, MinDist) :-
    aggregate_all(min(C),
        semantic_path_cost(Start, Target, [Start], C),
        MinDist).

% 4. Edge weights come from semantic_compiler:compile_edge_weights/4 at build time:
%    weight = 1 - cosine_similarity(embed("Machine learning"), embed("AI"))
edge_weight(machine_learning, artificial_intelligence, 0.12).
edge_weight(artificial_intelligence, computer_science, 0.18).
% ... etc
```

When this compiles to Rust:

- The kernel detector recognizes `semantic_path_cost/4` + `aggregate_all(min)` as the weighted shortest path pattern
- The edge weights get registered as native HashMap data
- The Dijkstra handler is wired into the WAM dispatcher
- The WAM compiles `graph_search/3` as a foreign call to the handler
- At runtime, Prolog logic calls into Rust, runs Dijkstra in native code, and returns the result

What the user wrote was a clean declarative specification. What runs is a hand-tuned native algorithm. That's the kernel system.

## 20.10 Further Reading

- **PR #1268** — `feat: min semantic distance kernel with Dijkstra shortest path`
- **PR #1274** — `feat: A* semantic distance kernel with dimensionality-aware heuristic`
- **PR #1277** — `feat: effective semantic distance — power-mean over weighted paths`
- `src/unifyweaver/targets/rust_target.pl` — kernel detectors and setup ops
- `src/unifyweaver/targets/wam_rust_target.pl` — WAM dispatch and native handlers
- `examples/benchmark/min_semantic_distance.pl` — Prolog spec for Dijkstra
- `examples/benchmark/benchmark_astar_dimensionality.py` — D parameter benchmark
