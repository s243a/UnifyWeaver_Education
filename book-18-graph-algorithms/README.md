<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Book 18: Graph Algorithms

**From Naive Search to Difference Equations**

*Part of the [UnifyWeaver Education Series](../README.md)*

**Status:** Initial — this book is a starting point. The architecture it describes is partly built (graph-search kernels, predicate-driven transpilation) and partly forward-looking (rule-based detection that rewrites graph-search formulations into difference equations). Chapters will be revised as the code direction settles.

## What this book is about

There is an obvious way to query a graph: write a traversal. BFS, DFS, A* — pick one, run it, return the answer. For small graphs and one-off queries, that is the whole story.

For everything else, it isn't. Materialising the transitive closure of a multi-million-node DAG is intractable in time and space. Running a per-query bidirectional walk is cheap per call but pays an ingest cost up front and rebuilds work on every cold query. Iterated fixed-point computation lies between them and is the *right* answer for many problems — but the textbook description of fixed-point semantics doesn't tell you when to reach for it.

This book is about how UnifyWeaver lets you write the *what* of a graph query — the declarative specification — and have the compiler choose the *how*. That choice spans the spectrum from generated graph-search code through to generated difference-equation iterations. It also forces a philosophical question that the book takes seriously: if the compiler chose a procedural per-iteration update under the hood, is your declaration still declarative? The book argues yes, but the argument matters.

## Why graph theory

You cannot reason about the choice between graph search, transitive closure, and difference equations without graph theory. Specifically:

- **Complexity classes** — transitive closure is O(V·E) on sparse graphs but O(V³) in the worst case. The gap predicts when materialisation is feasible.
- **Scale-free degree distributions** — real graphs (Wikipedia categories, citation networks, social graphs) have heavy-tailed degree distributions. Algorithms that work fine on uniform-degree graphs blow up on these.
- **Friendship paradox / Chung–Lu effective degree** — naive average degree is not the average degree the algorithm sees during traversal. The corrected `b_eff` governs convergence behaviour.
- **Fixed-point semantics** — when iterative refinement converges and what it converges to; the link between Tarski's fixed-point theorem and an "iterate until stable" loop.
- **Metric distortion** — when a tree distance, a shortest-path distance, and a weighted-power-mean distance agree, and when they don't (the tree-likeness index work).

Chapter 3 is a self-contained primer covering the theory the rest of the book needs. Readers familiar with this material can skim or skip it.

## Relationship to other books

| Book | Perspective |
|------|-------------|
| `other-books/book-semantic-geometry` | Hierarchy *emerges* from continuous geometry (learned embeddings, density manifolds, MST). Same Wikipedia data, complementary lens. |
| [Book 1: Foundations](../book-01-foundations/) | The compiler architecture this book builds on. Prerequisite if you are new to UnifyWeaver. |
| [Book 11: Prolog Target](../book-11-prolog-target/) | Fixed-point semantics in the Prolog target — relevant background for chapters 7–8. |
| [Book 17: WAM Target](../book-17-wam-target/) | The symbolic WAM target this book uses for some examples. |
| `other-books/book-fsharp-target` | The F# target this book uses as the primary example language. |

If you came from `book-semantic-geometry` looking for the "explicit DAG and graph-search" half of the story, this is that book. If you came here looking for the "learned-from-embeddings continuous geometry" half, go there.

## Example targets

Most code examples are in **F#**. F# is concise (the bidirectional kernel template fits in roughly eighty lines), efficient (LightningDB cursors and JIT-compiled inner loops scale to multi-million-node graphs), and a reasonable functional second language for readers comfortable with one of OCaml/Haskell/Scala.

Where the example benefits from showing the *symbolic* version of an algorithm rather than the native-lowered one, examples use the **symbolic WAM target**. Examples occasionally reach for Python (prototypes built before the F# kernels existed), C# (the fixed-point query runtime), or Prolog (the source language) where those targets illustrate a point better.

## Contents

### Part I — Philosophy and foundations

| # | Chapter | Topics |
|---|---------|--------|
| 1 | [Introduction](01_introduction.md) | The arc of the book; the naive premise; the journey |
| 2 | The declarative philosophy of UnifyWeaver *(planned)* | What declarative means here; vs Prolog, SQL, Datalog |
| 3 | Graph theory primer *(planned)* | DAGs, TC, BFS/bidirectional/A*, fixed-point, scale-free graphs, friendship paradox |
| 4 | What metrics on graphs mean *(planned)* | Depth, effective distance, weighted-power-mean, tree-likeness |

### Part II — The empirical pivot

| # | Chapter | Topics |
|---|---------|--------|
| 5 | Graph search as a primitive *(planned)* | F# bidirectional kernel; tree-likeness experiments; what we learned |
| 6 | The cost ledger *(planned)* | Ingest vs materialisation vs per-query; the regime where each wins |
| 7 | The difference-equation pivot *(planned)* | When TC blows up; iterative refinement; fixed-point semantics |

### Part III — How UnifyWeaver bridges the gap

| # | Chapter | Topics |
|---|---------|--------|
| 8 | Declarative vs procedural revisited *(planned)* | The reframing; what's recoverable as declarative |
| 9 | Constraint-hint predicates *(planned)* | Sufficient-condition declarations the compiler reads; concrete examples |
| 10 | Pattern detection by rule *(planned)* | Rules that rewrite graph-search into difference-equation form |
| 11 | Open problems *(planned)* | What's not built; what tree-likeness hinted at; how the book should evolve |

## Prerequisites

- Comfort reading code in at least one of F#, OCaml, Haskell, or Scala (chapter 5 onward assumes you can follow F#).
- Algorithms at the CS-undergrad level: BFS, DFS, asymptotic complexity, basic recurrences.
- No prior exposure to scale-free graphs, friendship-paradox effective degree, or fixed-point semantics on graphs — chapter 3 covers what you need.
- Familiarity with UnifyWeaver basics ([Book 1: Foundations](../book-01-foundations/)) is helpful but not strictly required for Part I.

## What's next

After completing Book 18:
- `other-books/book-semantic-geometry` — the complementary continuous-geometry perspective
- [Book 11: Prolog Target](../book-11-prolog-target/) — fixed-point semantics in depth
- [Book 14: AI Training](../book-14-ai-training/) — where graph structure feeds into learned models

## License

This educational content is licensed under CC BY 4.0.
Code examples are dual-licensed under MIT OR Apache-2.0.
