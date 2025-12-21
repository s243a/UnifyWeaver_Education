<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Book 6: Go Target

**Cross-Platform Native Binaries**

*Part of the [UnifyWeaver Education Series](../README.md)*

This book covers how to use UnifyWeaver to compile Prolog predicates into standalone Go executables. The Go target enables high-performance, cross-platform record processing with no runtime dependencies.

## What You'll Learn

By completing this book, you will be able to:

- Compile Prolog predicates to standalone Go binaries with zero runtime dependencies
- Choose between **streaming mode** (simple transforms) and **generator mode** (full Datalog)
- Handle JSON input/output with schema validation
- Build semantic search and RDF crawling pipelines
- Implement recursive predicates with BFS, memoization, and tail-call optimization
- Deploy cross-platform executables (Linux, Windows, macOS)

## Prerequisites

**Required:**
- [Book 1: Foundations](../book-01-foundations/README.md)

**Recommended:**
- [Book 5: Python Target](../book-05-python-target/README.md) - first book in the Portable Targets section
- [Book 2: Bash Target](../book-02-bash-target/README.md) - for stream compilation concepts

**Technical:**
- Go 1.16+ installed (for compiling generated code)
- UnifyWeaver with Go target support (v0.5+)

## Learning Path

**1. Introduction** (`01_introduction.md`)
-   Why use the Go target?
-   Architecture overview
-   Comparison with other targets

**2. Basic Compilation** (`02_basic_compilation.md`)
-   Compiling facts to lookup tables
-   Compiling rules for stream processing
-   Running generated binaries

**3. Advanced Features** (`03_advanced_features.md`)
-   Regex matching with `match/2`
-   Capture groups and extraction
-   Constraints and aggregations

**4. JSON Processing** (`04_json_processing.md`)
-   JSON input/output
-   Schema validation
-   Nested field extraction

**5. Semantic Crawling** (`05_semantic_crawling.md`) *(Advanced)*
-   Semantic Search (`semantic_search/3`)
-   RDF/XML Crawling
-   Embeddings and Vector Storage
-   *Requires: ONNX Runtime, model files*

**6. Generator Mode** (`06_generator_mode.md`) **NEW**
-   Fixpoint Datalog evaluation
-   Indexed joins and stratified negation
-   Aggregation with HAVING clauses
-   Parallel execution with goroutines
-   Database persistence with BoltDB

**7. Recursive Queries** (`07_recursive_queries.md`)
-   BFS-based transitive closure
-   Tail recursion optimization
-   Linear recursion with memoization
-   Mutual recursion patterns

## Appendices

**A. API Reference** (`A1_api_reference.md`)
-   All compilation predicates
-   Options and parameters
-   API selection guide

**B. Complexity Guide** (`A2_complexity_guide.md`)
-   Big-O analysis by mode
-   Performance tips
-   Mode selection criteria

**C. Database Persistence** (`A3_database_persistence.md`)
-   BoltDB schema and storage format
-   Key strategies (single, composite, hash)
-   Querying outside generated programs

## Related Books

- [Book 13: Semantic Search](../book-13-semantic-search/README.md) - Deep dive into semantic features (Chapter 5)
- [Book 7: Cross-Target Glue](../book-07-cross-target-glue/README.md) - Orchestrating multiple targets together

## License

This educational content is licensed under CC BY 4.0.
Code examples are dual-licensed under MIT OR Apache-2.0.
