# Book 17 Hybrid WAM Design

## Purpose

Book 17 should evolve from a short classic WAM fallback book into the
primary conceptual guide to UnifyWeaver's WAM and Hybrid WAM model. The
book should teach the stable architecture and vocabulary without turning
into a target-by-target implementation manual.

The central editorial stance is:

- Symbolic WAM text is the readable teaching, debugging, testing, and
  interchange notation.
- The preferred compiler path for hybrid WAM targets is structured WAM
  items or another target-ready WAM representation, consumed directly by
  the target emitter.
- The symbolic-text path remains available, but it is opt-in and mostly
  useful when humans or external tools need to inspect the WAM program.

## Audience

The book should serve two audiences:

- Readers learning the Hybrid WAM concepts and why they exist.
- Contributors who need the conceptual contract before reading
  target-specific code or design documents.

It should not try to teach every target's concrete implementation. Those
details belong in target books, target design documents, or focused
appendices.

## Core Concepts

Book 17 should cover these concepts as target-neutral ideas:

- Classic WAM execution: registers, heap, trail, choice points, calls,
  unification, and backtracking.
- WAM items as the compiler-facing representation.
- Symbolic WAM as a human-readable view of WAM items.
- Lowered emitters as target-specific translators from WAM-shaped logic
  into host-language code or host-language runtime structures.
- Hybrid routing: native lowering where profitable, WAM-shaped fallback
  where semantics require it, and target-specific strategy menus rather
  than one universal execution model.
- Foreign function interface boundaries: where logic execution calls into
  host capabilities, libraries, kernels, or external systems.
- External fact sources, indexing, materialization, and cost-aware choice
  of data access strategy.
- Kernels as specialized accelerators for common relational or graph
  workloads.
- Cross-target parity as the semantic contract, even when targets use
  different internal representations.

## Pipeline Contract

The book should make the default and optional paths explicit.

Default hybrid path:

```text
Prolog source
  -> WAM items / target-ready WAM representation
  -> lowered emitter or target WAM emitter
  -> host-language runtime structures or host-language code
```

Optional symbolic path:

```text
Prolog source
  -> symbolic WAM text
  -> shared compile-time parser
  -> WAM items
  -> lowered emitter or target WAM emitter
```

The symbolic path should be described as useful for debugging, golden
tests, human-readable dumps, external interchange, and diagnosing target
drift. It should not be presented as the normal internal bridge for
hybrid targets.

## Symbolic WAM In The Book

Symbolic WAM should still appear frequently because it is the best
teaching notation. The book can use symbolic listings the way systems
books use assembly listings: compact, inspectable, and target-neutral.

Every symbolic example should be framed as a readable projection of the
WAM item stream, not as proof that target generation must print and
parse text.

Recommended wording pattern:

```text
Conceptual listing:

  get_variable X1 A1
  get_constant alice A2
  call parent/2 2
  proceed

Compiler reality:

  Hybrid targets normally consume structured WAM items directly. The
  listing above is useful for explanation, debugging, tests, and
  interchange, but the normal target path skips text parsing.
```

## Target Comparisons

The book should use narrow, rotating comparison tables instead of one
wide target matrix. Each table should include the symbolic WAM view and
only the target columns that best illuminate the current concept.

Recommended table forms:

```markdown
| Symbolic WAM | Concept | Go sketch | Rust sketch |
|---|---|---|---|
```

```markdown
| Symbolic WAM | Concept | C sketch | LLVM sketch |
|---|---|---|---|
```

```markdown
| Concept | Haskell shape | Elixir shape | F# shape |
|---|---|---|---|
```

Suggested target choices by topic:

- Registers, heap, trail, and memory layout: C, Rust, Go.
- Lowered deterministic predicates: Go, Rust, C++.
- Choice points and continuations: Haskell, Elixir, Python.
- Foreign calls: C, LLVM, Haskell, Rust.
- Fact sources and indexing: Go, Rust, C++, Elixir.
- Parser and symbolic debug path: Python, C++, F#, R.

Each table should include a short caveat when needed:

> These examples are illustrative. Hybrid WAM targets do not need to
> share the same concrete representation; they share the item contract
> and semantic obligations.

## Proposed Chapter Shape

1. Introduction: Why WAM became Hybrid WAM
2. The compiler pipeline: items first, symbolic text as a view
3. Registers, terms, heap, trail, and unification
4. Calls, choice points, backtracking, and cuts
5. Lowered emitters and target strategy menus
6. Foreign functions and host capability boundaries
7. External fact sources, indexing, and materialization
8. Kernels and specialized relational execution
9. Cross-target parity and testing obligations
10. Debugging with symbolic WAM
11. Hybrid WAM design patterns and anti-patterns
12. Where to read target-specific implementation details

The current chapters can be retained where useful, but their framing
should change:

- The introduction should stop presenting WAM primarily as a fallback
  hub into WAT/JVM.
- The instruction-set chapter should become a teaching chapter for WAM
  items and symbolic listings.
- The compilation chapter should distinguish direct item production from
  symbolic text generation.
- The fallback-hub chapter should become one part of the broader hybrid
  routing story, not the main thesis.

## Non-Goals

Book 17 should not:

- Show complete implementations for every target.
- Imply that symbolic WAM text is the preferred internal representation.
- Encode target maturity as a permanent architecture claim.
- Duplicate every WAM design document from the main repository.
- Replace target-specific books or target-specific implementation docs.

## Validation

The edited book should be checked for these issues:

- Does every symbolic WAM example avoid implying a required print/parse
  round trip?
- Are target examples explicitly illustrative rather than canonical?
- Does the pipeline contract match the WAM Items API design?
- Are implementation details linked outward instead of embedded deeply?
- Can the book be read on narrow screens without oversized tables?
