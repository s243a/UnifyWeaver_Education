<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 2: The Declarative Philosophy of UnifyWeaver

## What declarative means

A *declarative* specification states what an answer is, not how to compute it. A *procedural* specification states a sequence of steps that, when executed, produces an answer. The two are not opposites — every implementation is procedural at the bottom — but the *interface* the user writes against can be one or the other.

UnifyWeaver's surface language is Prolog. Prolog has declarative semantics: a clause `ancestor(X, Y) :- parent(X, Y).` is a logical statement (*for all X and Y, X is an ancestor of Y if X is a parent of Y*). It also has a procedural execution model (top-down, depth-first, with backtracking) that you can write against directly if you choose to. UnifyWeaver makes the same surface available but compiles it to many targets — Bash, Python, F#, Rust, C#, SQL, Prolog, the symbolic WAM, and others. The compiler's job is to translate a declarative specification into procedural code in the target language.

The choice of target is not the only choice the compiler makes. Within a target, multiple compilation strategies are possible:

- **Stream / principal** — generate a single forward pass that yields results as they are computed.
- **Fixed-point** — generate an iterative loop that approaches the answer over passes.
- **Query engine** — emit data into a structured runtime that answers ad-hoc queries.
- **Generator** — emit a Python-style generator that the caller drives lazily.

The user picks the surface specification. The compiler — guided by user-supplied hints — picks the target and the strategy.

## The two questions

Every compilation decision in UnifyWeaver answers two questions:

1. **What is being computed?** A Prolog program states this directly.
2. **How should it be computed?** A constraint hint or optimisation manifest states this.

The codebase splits these questions explicitly. The module `src/unifyweaver/core/algorithm_manifest.pl` separates them with two distinct user directives: `decl_algorithm/2` (the *what*) and `decl_algorithm_optimization/2` (the *how*). Quoting from its header:

> Algorithm here means the Datalog/SQL sense of "a query" — the declarative *what*, not the algorithms-textbook sense of "a step-by-step procedure". The optimization manifest *completes* the definition by saying how the algorithm should be compiled.

That distinction matters. In a pure functional language, the two are typically conflated: the program text is both the specification and the implementation, and reorganising one reorganises the other. In UnifyWeaver, the specification is stable and the implementation is a separate concern. The user can change the optimisation manifest without touching the algorithm definition, and the compiler regenerates implementation code accordingly.

## Comparing the neighbourhood

**SQL** is declarative for relational queries. The user writes `SELECT name FROM person WHERE age > 30` and the query planner picks indices, join orders, and execution strategies. The *what* is constrained to relational algebra; the *how* is the planner's job. Hints (`/*+ INDEX(...) */`) let the user nudge the planner.

**Datalog** is a syntactic subset of Prolog with guaranteed termination (no function symbols, no infinite domains). It admits bottom-up evaluation by default, which avoids Prolog's depth-first non-termination on left-recursive rules. The price is expressive restriction: arbitrary Prolog cannot always be Datalog.

**Prolog** has declarative semantics and a procedural execution model that the user often reasons about explicitly. *Cut*, mode declarations, and clause ordering all leak procedural concerns into the surface syntax. Most Prolog programs in the wild are read both ways at once.

**UnifyWeaver** sits between SQL and Prolog. The surface language is Prolog — flexible, expressive, easy to read declaratively — but the compiler treats it more as Datalog: it analyses the structure, classifies the pattern (linear recursion, tail recursion, mutual recursion, fixed point), and dispatches to a target-specific code generator. The user retains the Prolog expressiveness but pays Datalog-style attention to which patterns the compiler recognises.

## What "still declarative" means

If the compiler emits a procedural loop under the hood, is the user's specification still declarative? The book argues yes — provided two properties hold:

1. **Readable as a statement.** The user can read the surface specification and reason about what answer it defines, without simulating execution.
2. **Redirectable without rewrite.** The user can change the optimisation manifest, the target, or both, and the surface specification still produces the same answer (modulo target capabilities). The user did not have to rewrite *what they wanted* in order to change *how it is computed*.

The first property is a semantic property of the surface language. UnifyWeaver inherits it from Prolog.

The second property is an architectural property of the compiler. UnifyWeaver achieves it by splitting algorithm and optimisation into separate declarations. If a user originally compiled `ancestor/2` as a stream and later wants it as a fixed point, they edit the optimisation manifest, not the predicate. The compiler picks up the new manifest and regenerates the target code.

Both properties together define what the book calls *redirectable declarative*. It is a stronger property than "the surface language has a model-theoretic semantics" (which is true of any logic language) and a weaker property than "the compiled code is provably equivalent to the specification under all extensional refinements" (which is far harder to achieve and not the bar UnifyWeaver aims for).

## Why this framing matters for graph problems

Graph problems make the *what/how* split unavoidably visible. Consider the canonical Prolog ancestor query:

```prolog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

This is a complete, correct declarative specification of the ancestor relation. As Prolog code, executed top-down, it has known pathologies on large graphs: redundant subgoal evaluation, no memoisation, stack growth, non-termination on cycles. Vanilla Prolog will work on small graphs and fail on the Wikipedia category DAG.

A skilled Prolog programmer rewrites the predicate with tabling, with explicit memoisation, or with iterative deepening. Each rewrite changes the surface code — *what they wanted* now embeds choices about *how to compute it*. The specification has stopped being declarative in the redirectable sense.

UnifyWeaver lets the specification stay as written. The compilation strategy is selected by a separate declaration. A stream-based target lowers the predicate to a streaming traversal. A fixed-point target lowers it to a bottom-up iteration over the *parent* relation, computing the transitive closure incrementally. A query-engine target lowers it to indexed lookups over a precomputed materialisation. The user gets to pick, but does not have to embed the pick in the surface code.

## The trade-off

The user pays for redirectability with reduced direct control. In hand-written Prolog with explicit tabling, the programmer chooses exactly which subgoals are memoised and when. In UnifyWeaver, that choice is implicit in the optimisation manifest, and the user must trust the compiler to do something sensible.

This trade is the same one SQL users make against hand-written database access code. It is a good trade when the compiler is well-tuned and a bad one when it isn't. UnifyWeaver's bet is that for the graph problems this book is about — where the optimisation space is large enough that human optimisation is unreliable — the trade is worth it.

The rest of this book is about what makes that bet plausible.

## Next

Chapter 3: Graph theory primer.
