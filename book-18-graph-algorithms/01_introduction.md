<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 1: Introduction

## The naive premise

A graph is a set of nodes and edges. A graph query is a question about that set. Many such questions have a textbook algorithm — "find the shortest path", "find a common ancestor", "find everything reachable" — and the implementation is a few dozen lines of breadth-first or depth-first search with bookkeeping for visited nodes and termination conditions.

If you have written this code, you know that for small graphs and one-off queries, the story ends there. You wrote the traversal, it returned an answer, the work is done.

This book is about everything that happens after *and then we scaled it up*.

## The journey

Consider the Wikipedia category graph. It is a directed acyclic graph (mostly — there are stray cycles, which the ingest pipeline cleans up) with roughly four million nodes (`Category:Foo`) and ten million edges (`Category:Foo is a subcategory of Category:Bar`). It has interesting structure: roughly 2.26 million categories are reachable from `Category:Main_topic_classifications` along subcategory edges. The English version is roughly thirty times larger than the Simple English version.

A reasonable question: *given a category `v`, what is its "distance" from the root `r`?* For a tree, the question has an exact answer: count the edges from `v` to `r`. For a DAG, there is no single answer — `v` can have many ancestral paths to `r`, of many different lengths.

We can settle for the *minimum* number of edges (the shortest-path distance, computed by BFS). That is a number we can compute cheaply per query. But the minimum lies — it picks the single shortest path and ignores everything else. A category that is one edge from the root via an unusual high-shortcut edge, but mostly reached by long zig-zag paths through the typical structure, has minimum distance `1` by this metric. Whether that is the right answer depends on the question you wanted to ask.

Real questions about real graphs — *how central is `v`?*, *how broadly accessible is `v`?*, *how does information propagate from `r` to `v`?* — want a different kind of distance: one that averages over the available paths, weighted by some notion of how likely or how natural each path is. UnifyWeaver's tree-likeness work calls this a *directionally-weighted power-mean metric*, written `d_wPow`. The formal definition lives in `docs/design/TREE_LIKENESS_INDEX.md`. The headline is: it is a real number that captures more about the graph topology than minimum-edge distance, and computing it requires enumerating paths, not just finding the shortest one.

Enumerating paths is exponential in the worst case. For a tightly-budgeted enumeration on a real graph, it is tractable — but only because the budget cuts the search short. The choice of budget interacts with the choice of metric in non-obvious ways, and on real-world graphs the interaction is empirical, not analytical. Chapter 5 walks through the experiments we ran to map it out.

## Where the naive premise breaks

Once you commit to per-query graph search on a multi-million-node graph, the cost structure changes shape:

- **Per-query cost** is small per call (a few hundred milliseconds with a good bidirectional A* kernel) but pays no amortisation across calls. Cold-cache queries dominate.
- **Materialisation cost** (precompute the metric for every pair) is Ω(V²) at minimum and infeasible at V = 2.26 million. You cannot store the answers, let alone compute them.
- **Ingest cost** is bounded but real: building the LMDB representation of the English Wikipedia categorisation graph takes minutes, and that is the *fast* path. If you change the metric definition, you may need to ingest again.

None of this is news to a working graph engineer. What may be news is that the answer is not "pick one of the three; live with the trade-off." The answer is that for many graph problems, neither pure per-query search nor pure materialisation is the right shape. The right shape is *iterative refinement* — a procedural loop that approaches the answer over successive passes, with the option to stop early and the option to amortise work across queries.

The textbook name for the shape is *fixed-point computation*. The shape that lets you express it as a series of incremental updates over an evolving relation is a *difference equation*. The transition from "graph search as a primitive" to "difference equation as a primitive" is the empirical pivot at the centre of this book.

## What declarative compilation does about it

UnifyWeaver is a declarative-to-imperative compiler. The user writes a Prolog-style specification of *what* they want; the compiler chooses *how* to compute it. For the graph problems this book is about, the choice spans:

- A direct graph-search kernel (the per-query path).
- A precomputed materialisation (the static path, where it fits).
- A generated difference-equation loop (the iterative path).
- A hybrid — graph search to seed, iterative refinement to settle.

The current state of UnifyWeaver supports the first three with varying degrees of completeness. The user expresses preference for one over another via *constraint-hint predicates* — declarations that the compiler reads as sufficient conditions to emit a particular code shape. Chapter 9 walks through what those predicates are and what the compiler does with them.

The forward direction — and this book takes "forward direction" seriously, because the book itself is a forward-direction artifact — is the ability for the compiler to *detect* a graph-search formulation and *rewrite* it into difference-equation form by rule. The user writes a graph traversal; the compiler observes that the traversal corresponds to an iterative fixed-point update; the compiler emits the update. Chapter 10 explores what is prototyped, what is hypothesised, and what is open.

A philosophical question lurks: if the compiler chose a procedural per-iteration update under the hood, is the user's specification still declarative? This book argues yes. The argument is in chapter 8. It matters because the argument cashes out as: *the user retains the right to redirect the compiler's choice without rewriting the specification.* That is the property that distinguishes a declarative interface from a leaky abstraction.

## Why graph theory

You cannot make any of the above choices without knowing roughly how the graph is shaped. Specifically:

- The transitive closure of a sparse graph is O(V·E) — feasible. The transitive closure of a dense graph is O(V³) — usually not. Which regime you are in depends on the degree distribution.
- Real-world graphs are not random in the textbook sense. They are *scale-free*: the degree distribution has a heavy tail. The friendship paradox — your friends have more friends than you do, on average — has a non-trivial corollary for graph algorithms: the *effective* branching factor an algorithm sees during traversal is larger than the average degree, sometimes much larger.
- Iterative methods on graphs converge at rates governed by the spectral properties of the connection matrix. Whether your difference equation converges in five passes or five hundred is not a matter of how clever you were about the loop; it is a matter of `b_eff · D` and the convergence ratio `r`.

Chapter 3 introduces the theory needed to reason about these choices. It is self-contained — a reader with a CS-undergrad background in algorithms but no exposure to scale-free graphs or fixed-point semantics on graphs should leave chapter 3 ready to read the rest of the book.

## Why F# (and WAM)

Most of the code in this book is in F#. The choice is pragmatic:

- F# compiles to native-speed code via .NET's JIT and integrates with LightningDB (an LMDB binding) for cursor-based traversal over disk-resident graphs of arbitrary size.
- F#'s functional syntax — pattern matching, immutable records, pipeline operators — keeps the kernel templates short. The bidirectional ancestor kernel template in `templates/targets/fsharp_wam/kernel_bidirectional_ancestor.fs.mustache` is roughly eighty lines.
- F# is reasonably portable across .NET runtimes (Linux, macOS, Windows) and is an easier on-ramp than Haskell or Idris for readers approaching functional programming for the first time.

Some examples — particularly when illustrating *what* the compiler is doing rather than *how* the generated code runs — use the **symbolic WAM target**, which represents Prolog execution as an explicit instruction stream rather than as native code. The symbolic form makes the compiler's choices visible. See [Book 17: WAM Target](../book-17-wam-target/) for background on the symbolic instruction set; this book invokes it but does not re-derive it.

Other examples reach for Python (prototypes built before the F# kernels existed), C# (the fixed-point query runtime), and Prolog (the source language). Where a particular target illustrates a point better than F#, the book uses it.

## How to read this book

The book is structured in three parts:

- **Part I (chapters 2–4)** — the foundation: declarative philosophy, graph theory, what a graph metric is. Linear; chapters build on each other.
- **Part II (chapters 5–7)** — the empirical pivot: what graph-search-as-primitive looked like in practice, what the cost ledger reveals, and why difference equations enter the picture. Builds on Part I.
- **Part III (chapters 8–11)** — the bridge: how UnifyWeaver currently handles the trade-off, what is prototyped, what is open. Builds on Parts I and II.

A reader who wants only the philosophy can read chapters 1, 2, 8, and 11 and skip the rest. A reader who wants only the practical compilation pipeline can skim chapters 1–2, then read 5, 6, 9, and 10. A reader new to functional programming, scale-free graphs, or both is the intended audience for the linear path.

The book is also a forward-direction artifact for the project itself. Sections marked *(open)* or *(prototyped, not generalised)* are flags for future work, not gaps in the exposition.

## Next

Chapter 2: The declarative philosophy of UnifyWeaver *(planned)*.
