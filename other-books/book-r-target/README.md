<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->
# R Target for UnifyWeaver

Compile Prolog predicates to idiomatic R scripts with vectorized operations, data frames, and memoized recursion.

## Chapters

1. **[Introduction](01_introduction.md)** — Why R, architecture, compilation modes, running generated scripts
2. **[Facts and Rules](02_facts_and_rules.md)** — Facts to lists, rules to functions, data frame accessors
3. **[Data Manipulation](03_data_manipulation.md)** — filter, group_by, sort_by, pipeline operations
4. **[Recursion Patterns](04_recursion_patterns.md)** — Tail, linear, tree, multi-call, mutual recursion with memoization
5. **[Bindings](05_bindings.md)** — 70+ R bindings: math, string, type conversion, vector, file I/O
6. **[Advanced Features](06_advanced_features.md)** — Direct multi-call compilation, fixpoint pipelines, component registry

## Prerequisites

- SWI-Prolog
- R >= 4.0 / Rscript

```bash
# Ubuntu/Debian
sudo apt install r-base

# macOS
brew install r

# Termux (Android)
pkg install r-base
```

## Quick Example

```prolog
?- compile_facts_to_r(parent, 2, Code), write(Code).
```

```r
parent_data <- list(
  c("tom", "bob"),
  c("bob", "jim")
)

parent_get_all <- function() parent_data
parent_stream <- function() parent_data
parent_contains <- function(x, y) {
  any(sapply(parent_data, function(p) p[1] == x && p[2] == y))
}
```

## Why R?

| Feature | Benefit |
|---------|---------|
| Vectorized operations | Prolog list operations map to efficient R vectorization |
| `new.env(hash=TRUE)` | O(1) memoization using R's environment hash tables |
| `Reduce()` / `Recall()` | Natural fit for fold-based and recursive Prolog patterns |
| Data frames | First-class tabular data for filter/group/sort predicates |
| `Rscript` CLI | Generated scripts run directly from the command line |
| CRAN ecosystem | 20,000+ packages for statistics, ML, visualization |

## Comparison

| Feature | Bash | R | Python |
|---------|------|---|--------|
| **Recursion** | Loop rewriting | Memoized functions | Generator-based |
| **Data structures** | Associative arrays | Vectors, data frames, environments | Dicts, FrozenDicts |
| **Streaming** | Pipes (`\|`) | JSONL read/write | Generators |
| **Memoization** | `declare -gA` | `new.env(hash=TRUE)` | `@functools.cache` |
| **Best for** | System scripts | Data analysis, statistics | General purpose |

## License

This educational content is licensed under CC BY 4.0.
Code examples are dual-licensed under MIT OR Apache-2.0.
