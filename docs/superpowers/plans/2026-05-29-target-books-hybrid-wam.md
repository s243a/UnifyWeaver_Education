# Target Books Hybrid WAM Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Update the target-specific education books so each one explains its relationship to Hybrid WAM without duplicating Book 17 or implying that symbolic WAM text is the default compiler transport.

**Architecture:** Book 17 remains the canonical conceptual source. Target books get short, target-appropriate sections that link back to Book 17 and describe the target's role: native-only, hybrid WAM runtime, lowered emitter, parser/debug path, FFI/kernel host, or future/adjacent target. Each edit is documentation-only and should preserve existing untracked work in `education/`.

**Tech Stack:** Markdown education books in the separate `education` git repository; validation with `git diff`, `grep`, and link/path inspection.

---

## File Structure

Modify these files in focused batches:

- `education/README.md`: add Book 17 to the series overview and add a Hybrid WAM learning path.
- `education/book-17-wam-target/README.md`: make Book 17 the conceptual hub that target books point to.
- `education/book-06-go-target/README.md`
- `education/book-06-go-target/07_recursive_queries.md`
- `education/book-09-rust-target/README.md`
- `education/book-09-rust-target/05_native_clause_lowering.md`
- `education/book-05-python-target/README.md`
- `education/book-05-python-target/06_native_clause_lowering.md`
- `education/book-03-csharp-target/README.md`
- `education/book-03-csharp-target/08_native_clause_lowering.md`
- `education/book-07-cross-target-glue/README.md`
- `education/book-07-cross-target-glue/17_llvm_ffi.md`
- `education/book-13-semantic-search/README.md`
- `education/book-13-semantic-search/20_recursive_kernels.md`
- `education/other-books/book-haskell-target/README.md`
- `education/other-books/book-haskell-target/04_performance.md`
- `education/other-books/book-elixir-target/README.md`
- `education/other-books/book-elixir-target/03_recursion.md`
- `education/other-books/book-c-target/README.md`
- `education/other-books/book-c-target/04_recursive_queries.md`
- `education/other-books/book-cpp-target/README.md`
- `education/other-books/book-cpp-target/04_recursive_queries.md`
- `education/other-books/book-llvm-target/README.md`
- `education/other-books/book-llvm-target/03_recursive_queries.md`
- `education/other-books/book-r-target/README.md`
- `education/other-books/book-r-target/06_advanced_features.md`
- `education/other-books/book-fsharp-target/README.md`
- `education/other-books/book-fsharp-target/03_recursive_queries.md`
- `education/other-books/book-clojure-target/README.md`
- `education/other-books/book-clojure-target/04_recursive_queries.md`
- `education/other-books/book-scala-target/README.md`
- `education/other-books/book-scala-target/04_recursive_queries.md`
- `education/other-books/book-wasm-target/README.md`
- `education/other-books/book-wasm-target/02_compilation.md`

Do not modify or stage these existing untracked files unless the user explicitly asks:

- `education/init.pl`
- `education/other-books/book-elixir-target/04_transitive_closure.md`
- `education/other-books/book-elixir-target/05_pipelines.md`

### Shared Editorial Rule

Every added target section must follow this contract using the exact
target-specific wording supplied in the task steps:

```markdown
## Hybrid WAM Role

Book 17 explains the shared Hybrid WAM concepts; this chapter only describes
how those concepts surface for this target.

- Default generation path: name the target's normal path without claiming
  symbolic WAM text is the default internal bridge.
- Symbolic WAM text: describe it as readable notation, debugging output,
  tests, parser input, or not used in that target book.
- Target-specific emphasis: name the one or two target-specific ideas the
  reader should remember.
```

Use exact role language from the relevant task below. Keep sections short: 120-220 words per target unless the file already has a deeper WAM section.

### Validation Commands

Run these after each task:

```bash
git -C education diff --check
grep -R "default.*symbolic WAM\|symbolic WAM.*default\|compile.*symbolic.*normal" -n education/book-* education/other-books/book-* || true
git -C education status --short
```

Expected:

- `git diff --check` prints nothing.
- The `grep` command has no lines that imply symbolic WAM text is the normal/default compiler path.
- `git status --short` shows only the files intentionally edited for the current task plus the three pre-existing untracked files listed above.

---

### Task 1: Update Series-Level Navigation

**Files:**
- Modify: `education/README.md`
- Modify: `education/book-17-wam-target/README.md`

- [ ] **Step 1: Update `education/README.md` book tables**

Add Book 17 to the main overview after Book 16:

```markdown
### Execution Architecture (Book 17)

| Book | Title | Focus |
|------|-------|-------|
| **17** | [WAM and Hybrid WAM](book-17-wam-target/README.md) | WAM items, symbolic WAM notation, lowered emitters, FFI, external fact sources, kernels, and cross-target parity |
```

Add Book 17 to Book Status:

```markdown
| 17 - WAM and Hybrid WAM | Planned refresh | `book-17-wam-target/` |
```

Add this learning path after the Systems Programmer path:

```markdown
### Path 7: Hybrid WAM Contributor
1. Book 1: Foundations
2. Book 17: WAM and Hybrid WAM
3. Book 6 or 9: Go or Rust Target
4. Book 7: Cross-Target Glue
5. Target-specific books for the runtime you plan to edit
```

- [ ] **Step 2: Rewrite `book-17-wam-target/README.md` as the conceptual hub**

Replace the opening description with:

```markdown
# Book 17: WAM and Hybrid WAM

The WAM (Warren Abstract Machine) material in UnifyWeaver has grown from a
fallback target into the shared conceptual layer behind many hybrid targets.
This book teaches that layer: WAM items, symbolic WAM notation, lowered
emitters, foreign calls, external fact sources, kernels, and cross-target
parity.

Symbolic WAM text is used throughout the book because it is readable. It is
the teaching, debugging, testing, and interchange notation. The preferred
compiler path for hybrid targets is structured WAM items or another
target-ready WAM representation consumed directly by the target emitter.
The symbolic-text path remains useful, but it is not the default internal
transport.
```

Add this section before the chapter list:

```markdown
## How Target Books Use This Book

Target-specific books should link here for the shared model and then explain
only the target's own role: native lowering, lowered WAM emission, runtime
interpretation, FFI, indexing, parser support, or kernel dispatch. No target
book should duplicate the full Hybrid WAM architecture.
```

- [ ] **Step 3: Run validation**

Run:

```bash
git -C education diff --check
grep -R "default.*symbolic WAM\|symbolic WAM.*default\|compile.*symbolic.*normal" -n education/book-* education/other-books/book-* || true
git -C education status --short
```

Expected: no whitespace errors; no misleading symbolic-WAM-default wording; only intended files plus the three pre-existing untracked files are shown.

- [ ] **Step 4: Commit Task 1**

Run:

```bash
git -C education add README.md book-17-wam-target/README.md
git -C education commit -m "docs(wam): orient education series around hybrid wam"
```

Expected: commit succeeds with only the two intended files.

---

### Task 2: Update Primary In-Tree Target Books

**Files:**
- Modify: `education/book-06-go-target/README.md`
- Modify: `education/book-06-go-target/07_recursive_queries.md`
- Modify: `education/book-09-rust-target/README.md`
- Modify: `education/book-09-rust-target/05_native_clause_lowering.md`
- Modify: `education/book-05-python-target/README.md`
- Modify: `education/book-05-python-target/06_native_clause_lowering.md`
- Modify: `education/book-03-csharp-target/README.md`
- Modify: `education/book-03-csharp-target/08_native_clause_lowering.md`

- [ ] **Step 1: Add Go Hybrid WAM role text**

In `book-06-go-target/README.md`, add a chapter bullet or note:

```markdown
- Hybrid WAM role: Go is a practical host for explicit WAM state, indexed fact dispatch, and selective lowered predicates. Book 17 covers the shared model; this book shows where Go's maps, slices, structs, and generated functions fit that model.
```

In `book-06-go-target/07_recursive_queries.md`, add:

```markdown
## Hybrid WAM Role

Go is a useful example of a target that can keep WAM state explicit while
still lowering selected predicates into ordinary host-language functions.
The shared Hybrid WAM concepts are covered in Book 17; the Go-specific point
is that registers, choice points, indexed fact access, and deterministic
predicate helpers can be represented with simple structs, slices, maps, and
methods.

- Default generation path: structured WAM items or target-ready WAM data
  should feed the Go emitter directly.
- Symbolic WAM text: useful as a readable debug listing, not as the normal
  internal bridge.
- Target-specific emphasis: explicit state, indexed dispatch, and selective
  lowered helpers for deterministic pieces of a larger WAM-shaped program.
```

- [ ] **Step 2: Add Rust Hybrid WAM role text**

In `book-09-rust-target/README.md`, add:

```markdown
- Hybrid WAM role: Rust is the main example for memory-safe WAM state, external fact sources, cost-aware materialization, and FFI/kernel dispatch.
```

In `book-09-rust-target/05_native_clause_lowering.md`, add:

```markdown
## Hybrid WAM Role

Rust should be read as the systems target where Hybrid WAM choices become
explicit: ownership, storage layout, external fact sources, materialization,
and native kernels all have to be represented safely. Book 17 covers the
shared architecture; this chapter focuses on why Rust-specific lowering is a
good fit for memory-safe WAM state and host-native acceleration.

- Default generation path: structured WAM items or target-ready WAM data
  should feed Rust code generation directly.
- Symbolic WAM text: useful for explanations, debug dumps, and tests, but not
  the preferred compiler transport.
- Target-specific emphasis: ownership-aware WAM state, LMDB or other external
  fact sources, indexed lookup, and FFI/kernel dispatch.
```

- [ ] **Step 3: Add Python Hybrid WAM role text**

In `book-05-python-target/README.md`, add:

```markdown
- Hybrid WAM role: Python remains primarily a high-level target in this book; where WAM appears, it should be framed as an optional runtime/parser/debug path rather than the default Python compilation model.
```

In `book-05-python-target/06_native_clause_lowering.md`, add:

```markdown
## Hybrid WAM Boundary

Python's native target material is best understood as the high-level side of
the Hybrid WAM boundary. Many examples can stay as procedural or generator
code. When a workload needs full WAM semantics or a runtime parser, Book 17's
model explains the alternate path.

- Default generation path in this book: Python-native lowering where the
  predicate shape is simple enough.
- Symbolic WAM text: useful for debugging and parser-related tests when using
  WAM-backed Python paths.
- Target-specific emphasis: readability, generator-style search, and clear
  separation between native Python lowering and WAM-backed execution.
```

- [ ] **Step 4: Add C# Hybrid WAM boundary text**

In `book-03-csharp-target/README.md`, add:

```markdown
- Hybrid WAM boundary: C# target material focuses on native query/runtime techniques. Book 17 should be consulted when those techniques are compared with WAM-style fallback, parity, or cross-target execution.
```

In `book-03-csharp-target/08_native_clause_lowering.md`, add:

```markdown
## Hybrid WAM Boundary

The C# target book teaches native lowering and query-runtime design rather
than a primary WAM runtime. That distinction matters: Hybrid WAM is not a
requirement that every target implement the same machine. It is a shared
semantic contract and a set of fallback or acceleration patterns that targets
adopt where they fit.

- Default generation path in this book: C#-native query/runtime lowering.
- Symbolic WAM text: useful as a comparison point when explaining what native
  lowering avoids or replaces.
- Target-specific emphasis: LINQ/query-runtime techniques, fixed-point
  execution, and when native lowering should be preferred over WAM-shaped
  execution.
```

- [ ] **Step 5: Run validation**

Run:

```bash
git -C education diff --check
grep -R "default.*symbolic WAM\|symbolic WAM.*default\|compile.*symbolic.*normal" -n education/book-* education/other-books/book-* || true
git -C education status --short
```

Expected: validation passes and status shows the eight intended files plus the three pre-existing untracked files.

- [ ] **Step 6: Commit Task 2**

Run:

```bash
git -C education add book-06-go-target/README.md book-06-go-target/07_recursive_queries.md book-09-rust-target/README.md book-09-rust-target/05_native_clause_lowering.md book-05-python-target/README.md book-05-python-target/06_native_clause_lowering.md book-03-csharp-target/README.md book-03-csharp-target/08_native_clause_lowering.md
git -C education commit -m "docs(targets): explain hybrid wam roles for primary targets"
```

Expected: commit succeeds with only the eight intended files.

---

### Task 3: Update Cross-Target, Semantic, and Haskell Books

**Files:**
- Modify: `education/book-07-cross-target-glue/README.md`
- Modify: `education/book-07-cross-target-glue/17_llvm_ffi.md`
- Modify: `education/book-13-semantic-search/README.md`
- Modify: `education/book-13-semantic-search/20_recursive_kernels.md`
- Modify: `education/other-books/book-haskell-target/README.md`
- Modify: `education/other-books/book-haskell-target/04_performance.md`

- [ ] **Step 1: Add Cross-Target Glue Hybrid WAM role text**

In `book-07-cross-target-glue/README.md`, add:

```markdown
- Hybrid WAM role: cross-target glue explains how WAM-shaped execution, FFI calls, kernels, and service boundaries compose across languages. Book 17 supplies the WAM concepts; this book supplies integration patterns.
```

In `book-07-cross-target-glue/17_llvm_ffi.md`, add:

```markdown
## Hybrid WAM And FFI

The foreign function interface is one of the main places where Hybrid WAM
stops being a pure interpreter story. A WAM-shaped predicate can call into a
host function, a compiled kernel, a native library, or an external service,
then unify the returned values back into logic variables. Book 17 explains
the shared boundary; this chapter focuses on LLVM-oriented integration.

- Default generation path: structured WAM items or target-ready lowering data
  should reach the LLVM-facing emitter directly.
- Symbolic WAM text: useful for showing the call site in a compact listing.
- Target-specific emphasis: ABI boundaries, value representation, and how
  native calls return into WAM unification.
```

- [ ] **Step 2: Refine Semantic Search Hybrid WAM wording**

In `book-13-semantic-search/README.md`, update existing Hybrid WAM bullets so they point to Book 17 and say:

```markdown
- Three-tier lowering: native pattern -> kernel FFI -> WAM-shaped fallback.
- Rust Hybrid WAM with foreign function interface; see Book 17 for the shared WAM concepts.
```

In `book-13-semantic-search/20_recursive_kernels.md`, add near the first kernel/WAM section:

```markdown
Book 17 explains the general Hybrid WAM architecture. This chapter uses the
Rust semantic-search kernels as a concrete example of one layer in that
architecture: a recognized logic pattern is routed to a native handler through
the WAM foreign-call boundary, then results are unified back into the logic
query.
```

Replace wording that says `Compile Prolog -> WAM bytecode -> Rust runtime`
with:

```markdown
Compile Prolog -> WAM items or target-ready WAM representation -> Rust runtime
```

- [ ] **Step 3: Add Haskell Hybrid WAM performance role text**

In `other-books/book-haskell-target/README.md`, add:

```markdown
- Hybrid WAM role: Haskell demonstrates how a target can keep WAM semantics while experimenting with purity-aware routing, parallel search, FFI, and different state representations.
```

In `other-books/book-haskell-target/04_performance.md`, add near the opening:

```markdown
Book 17 covers the shared Hybrid WAM concepts. This chapter is the Haskell
case study: it shows how the same semantic obligations can be met with
Haskell-specific choices around state, purity, profiling, FFI, and data
layout.
```

- [ ] **Step 4: Run validation**

Run:

```bash
git -C education diff --check
grep -R "Compile Prolog -> WAM bytecode -> Rust runtime" -n education/book-13-semantic-search education/other-books/book-haskell-target education/book-07-cross-target-glue || true
grep -R "default.*symbolic WAM\|symbolic WAM.*default\|compile.*symbolic.*normal" -n education/book-* education/other-books/book-* || true
git -C education status --short
```

Expected: no old bytecode wording in the checked files; no misleading symbolic-WAM-default wording; status shows six intended files plus the three pre-existing untracked files.

- [ ] **Step 5: Commit Task 3**

Run:

```bash
git -C education add book-07-cross-target-glue/README.md book-07-cross-target-glue/17_llvm_ffi.md book-13-semantic-search/README.md book-13-semantic-search/20_recursive_kernels.md other-books/book-haskell-target/README.md other-books/book-haskell-target/04_performance.md
git -C education commit -m "docs(targets): connect kernels ffi and haskell to hybrid wam"
```

Expected: commit succeeds with only the six intended files.

---

### Task 4: Update Supplementary WAM Runtime Target Books

**Files:**
- Modify: `education/other-books/book-elixir-target/README.md`
- Modify: `education/other-books/book-elixir-target/03_recursion.md`
- Modify: `education/other-books/book-c-target/README.md`
- Modify: `education/other-books/book-c-target/04_recursive_queries.md`
- Modify: `education/other-books/book-cpp-target/README.md`
- Modify: `education/other-books/book-cpp-target/04_recursive_queries.md`
- Modify: `education/other-books/book-llvm-target/README.md`
- Modify: `education/other-books/book-llvm-target/03_recursive_queries.md`

- [ ] **Step 1: Add Elixir role text without touching untracked Elixir chapters**

In `other-books/book-elixir-target/README.md`, add:

```markdown
- Hybrid WAM role: Elixir is useful for explaining CPS-style WAM execution, continuation boundaries, and future BEAM-native strategy choices.
```

In `other-books/book-elixir-target/03_recursion.md`, add:

```markdown
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
```

- [ ] **Step 2: Add C and C++ role text**

In `other-books/book-c-target/README.md`, add:

```markdown
- Hybrid WAM role: C is the low-level memory-layout reference point for WAM state, lifecycle, and foreign-call boundaries.
```

In `other-books/book-c-target/04_recursive_queries.md`, add:

```markdown
## Hybrid WAM Role

C is the low-level reference point for WAM memory and lifecycle choices. Book
17 explains the shared Hybrid WAM model; the C-specific value is seeing how
registers, stacks, trails, foreign calls, and explicit ownership can be made
visible without hiding behind a managed runtime.

- Default generation path: structured WAM items or target-ready WAM data
  should feed C emission directly.
- Symbolic WAM text: useful as a readable listing before discussing concrete
  memory layout.
- Target-specific emphasis: memory layout, lifecycle, explicit stacks, and
  foreign-call boundaries.
```

In `other-books/book-cpp-target/README.md`, add:

```markdown
- Hybrid WAM role: C++ is useful for explaining runtime containers, parser support, LMDB-style fact sources, and compiled/native parser tradeoffs.
```

In `other-books/book-cpp-target/04_recursive_queries.md`, add:

```markdown
## Hybrid WAM Role

C++ is useful for explaining the host-runtime side of Hybrid WAM: containers,
fact-source adapters, parser modes, and native library integration. Book 17
defines the shared concepts; this chapter should show why C++ can host both
compact runtime structures and pragmatic bridges to storage or parsing code.

- Default generation path: structured WAM items or target-ready WAM data
  should feed C++ generation directly.
- Symbolic WAM text: useful for debug listings and parser-related workflows,
  not as the preferred internal transport.
- Target-specific emphasis: runtime containers, parser modes, fact sources,
  and native/library integration.
```

- [ ] **Step 3: Add LLVM role text**

In `other-books/book-llvm-target/README.md`, add:

```markdown
- Hybrid WAM role: LLVM is the lowered-code perspective on WAM items, control flow, value representation, and FFI boundaries.
```

In `other-books/book-llvm-target/03_recursive_queries.md`, add:

```markdown
## Hybrid WAM Role

LLVM is the clearest target for explaining the lowered-emitter idea. The
shared Hybrid WAM contract is still semantic, but LLVM forces control flow,
register movement, value representation, and foreign-call boundaries into a
lower-level form.

- Default generation path: structured WAM items or target-ready lowering data
  should feed LLVM emission directly.
- Symbolic WAM text: useful as a compact listing before showing lower-level
  control-flow shapes.
- Target-specific emphasis: basic blocks, branches, value layout, and ABI
  calls.
```

- [ ] **Step 4: Run validation**

Run:

```bash
git -C education diff --check
grep -R "default.*symbolic WAM\|symbolic WAM.*default\|compile.*symbolic.*normal" -n education/book-* education/other-books/book-* || true
git -C education status --short
```

Expected: validation passes and status shows eight intended files plus the three pre-existing untracked files.

- [ ] **Step 5: Commit Task 4**

Run:

```bash
git -C education add other-books/book-elixir-target/README.md other-books/book-elixir-target/03_recursion.md other-books/book-c-target/README.md other-books/book-c-target/04_recursive_queries.md other-books/book-cpp-target/README.md other-books/book-cpp-target/04_recursive_queries.md other-books/book-llvm-target/README.md other-books/book-llvm-target/03_recursive_queries.md
git -C education commit -m "docs(targets): add hybrid wam notes to supplementary runtimes"
```

Expected: commit succeeds with only the eight intended files.

---

### Task 5: Update Parser And Additional JVM/Functional Target Books

**Files:**
- Modify: `education/other-books/book-r-target/README.md`
- Modify: `education/other-books/book-r-target/06_advanced_features.md`
- Modify: `education/other-books/book-fsharp-target/README.md`
- Modify: `education/other-books/book-fsharp-target/03_recursive_queries.md`
- Modify: `education/other-books/book-clojure-target/README.md`
- Modify: `education/other-books/book-clojure-target/04_recursive_queries.md`
- Modify: `education/other-books/book-scala-target/README.md`
- Modify: `education/other-books/book-scala-target/04_recursive_queries.md`
- Modify: `education/other-books/book-wasm-target/README.md`
- Modify: `education/other-books/book-wasm-target/02_compilation.md`

- [ ] **Step 1: Add parser-oriented R and F# role text**

In `other-books/book-r-target/README.md`, add:

```markdown
- Hybrid WAM role: R is useful for discussing native parser support and data-analysis-friendly WAM boundaries.
```

In `other-books/book-r-target/06_advanced_features.md`, add:

```markdown
## Hybrid WAM Role

R is useful for discussing parser and data-analysis boundaries rather than a
one-size-fits-all WAM implementation. Book 17 explains the shared Hybrid WAM
contract; this chapter should focus on where R-specific parsing, tabular data,
and analysis workflows meet WAM-shaped execution.

- Default generation path: R-native lowering should remain the first choice
  for analysis-shaped code, with WAM-backed paths used when full logic
  semantics or parser behavior is needed.
- Symbolic WAM text: useful for parser/debug paths and compact explanations.
- Target-specific emphasis: native parser support, data-frame boundaries, and
  analysis workflows.
```

In `other-books/book-fsharp-target/README.md`, add:

```markdown
- Hybrid WAM role: F# is useful for discussing compiled parser support, .NET-friendly WAM state, and functional update tradeoffs.
```

In `other-books/book-fsharp-target/03_recursive_queries.md`, add:

```markdown
## Hybrid WAM Role

F# is useful for explaining how WAM semantics fit a functional .NET target.
Book 17 covers the shared Hybrid WAM model; this chapter should show how
compiled parser support, .NET representation choices, and functional updates
shape the target-specific version of that model.

- Default generation path: structured WAM items or target-ready WAM data
  should feed the F# emitter directly.
- Symbolic WAM text: useful for readable listings and for explaining compiled
  parser support.
- Target-specific emphasis: compiled parser support, .NET representation
  choices, and functional state updates.
```

- [ ] **Step 2: Add JVM-family role text**

In `other-books/book-clojure-target/README.md`, add:

```markdown
- Hybrid WAM role: Clojure is useful for explaining persistent data structure tradeoffs and lowered WAM on a dynamic JVM language.
```

In `other-books/book-clojure-target/04_recursive_queries.md`, add:

```markdown
## Hybrid WAM Role

Clojure is useful for explaining Hybrid WAM on a dynamic JVM language. Book
17 defines the shared contract; this chapter should focus on how persistent
data structures, dynamic dispatch, and lowered helpers can represent WAM
semantics without copying another target's concrete runtime shape.

- Default generation path: structured WAM items or target-ready WAM data
  should feed Clojure generation directly.
- Symbolic WAM text: useful as a readable listing and debug artifact.
- Target-specific emphasis: persistent data structures, dynamic dispatch, and
  lowered helpers.
```

In `other-books/book-scala-target/README.md`, add:

```markdown
- Hybrid WAM role: Scala is useful for explaining JVM-hosted Hybrid WAM choices with typed functional and object-oriented representation options.
```

In `other-books/book-scala-target/04_recursive_queries.md`, add:

```markdown
## Hybrid WAM Role

Scala is useful for explaining JVM-hosted Hybrid WAM with typed functional and
object-oriented representation options. Book 17 explains the shared WAM item
and parity contract; this chapter should show where Scala can use typed data
models, functional collections, and JVM dispatch to host WAM-shaped logic.

- Default generation path: structured WAM items or target-ready WAM data
  should feed Scala generation directly.
- Symbolic WAM text: useful as readable notation and test/debug output.
- Target-specific emphasis: typed JVM representations, functional
  collections, and hybrid runtime dispatch.
```

- [ ] **Step 3: Add WASM role text**

In `other-books/book-wasm-target/README.md`, add:

```markdown
- Hybrid WAM role: WASM is a portable execution target for WAM-shaped programs, especially when browser or sandboxed deployment matters.
```

In `other-books/book-wasm-target/02_compilation.md`, add:

```markdown
## Hybrid WAM Role

WASM is best read as a portable execution environment for WAM-shaped code,
not as the only or default destination for WAM. Book 17 explains why Hybrid
WAM targets usually prefer structured items internally while still using
symbolic WAM text for readable listings and debugging.

- Default generation path: structured WAM items or target-ready WAM data
  should feed the WASM/WAT emitter directly.
- Symbolic WAM text: useful as a compact explanation before showing WAT.
- Target-specific emphasis: stack-machine lowering, linear memory, browser
  deployment, and sandboxed execution.
```

- [ ] **Step 4: Run validation**

Run:

```bash
git -C education diff --check
grep -R "default.*symbolic WAM\|symbolic WAM.*default\|compile.*symbolic.*normal" -n education/book-* education/other-books/book-* || true
git -C education status --short
```

Expected: validation passes and status shows ten intended files plus the three pre-existing untracked files.

- [ ] **Step 5: Commit Task 5**

Run:

```bash
git -C education add other-books/book-r-target/README.md other-books/book-r-target/06_advanced_features.md other-books/book-fsharp-target/README.md other-books/book-fsharp-target/03_recursive_queries.md other-books/book-clojure-target/README.md other-books/book-clojure-target/04_recursive_queries.md other-books/book-scala-target/README.md other-books/book-scala-target/04_recursive_queries.md other-books/book-wasm-target/README.md other-books/book-wasm-target/02_compilation.md
git -C education commit -m "docs(targets): document parser jvm and wasm hybrid wam roles"
```

Expected: commit succeeds with only the ten intended files.

---

### Task 6: Final Consistency Pass

**Files:**
- Modify only files already changed in Tasks 1-5 if wording cleanup is needed.

- [ ] **Step 1: Check cross-book references**

Run:

```bash
grep -R "Book 17" -n education/book-* education/other-books/book-* | head -120
grep -R "symbolic WAM" -n education/book-* education/other-books/book-* | head -120
```

Expected: Book 17 references appear in updated target books; symbolic WAM wording consistently frames it as readable/debug notation.

- [ ] **Step 2: Check for outdated fallback-only framing**

Run:

```bash
grep -R "universal fallback\|fallback hub\|WAM bytecode" -n education/book-* education/other-books/book-* || true
```

Expected: Any remaining lines are either in Book 17 historical context or are edited to avoid claiming fallback/interchange is the whole Hybrid WAM story.

- [ ] **Step 3: Run final validation**

Run:

```bash
git -C education diff --check
grep -R "default.*symbolic WAM\|symbolic WAM.*default\|compile.*symbolic.*normal" -n education/book-* education/other-books/book-* || true
git -C education status --short
```

Expected: validation passes and only intentional files plus the three pre-existing untracked files appear.

- [ ] **Step 4: Commit final cleanup if there are changes**

If Step 2 or Step 3 required cleanup, stage only the files reported by
`git -C education status --short` that were intentionally changed by Tasks
1-5. Do not stage `init.pl` or the untracked Elixir chapters. Use explicit
paths, for example:

```bash
git -C education add README.md book-17-wam-target/README.md book-06-go-target/README.md book-06-go-target/07_recursive_queries.md book-09-rust-target/README.md book-09-rust-target/05_native_clause_lowering.md book-05-python-target/README.md book-05-python-target/06_native_clause_lowering.md book-03-csharp-target/README.md book-03-csharp-target/08_native_clause_lowering.md book-07-cross-target-glue/README.md book-07-cross-target-glue/17_llvm_ffi.md book-13-semantic-search/README.md book-13-semantic-search/20_recursive_kernels.md other-books/book-haskell-target/README.md other-books/book-haskell-target/04_performance.md other-books/book-elixir-target/README.md other-books/book-elixir-target/03_recursion.md other-books/book-c-target/README.md other-books/book-c-target/04_recursive_queries.md other-books/book-cpp-target/README.md other-books/book-cpp-target/04_recursive_queries.md other-books/book-llvm-target/README.md other-books/book-llvm-target/03_recursive_queries.md other-books/book-r-target/README.md other-books/book-r-target/06_advanced_features.md other-books/book-fsharp-target/README.md other-books/book-fsharp-target/03_recursive_queries.md other-books/book-clojure-target/README.md other-books/book-clojure-target/04_recursive_queries.md other-books/book-scala-target/README.md other-books/book-scala-target/04_recursive_queries.md other-books/book-wasm-target/README.md other-books/book-wasm-target/02_compilation.md
git -C education commit -m "docs(targets): align hybrid wam terminology across books"
```

Expected: commit succeeds. If no cleanup was needed, skip this commit.
