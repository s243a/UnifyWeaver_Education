# Chapter 1: Introduction to the Kotlin Target

The Kotlin target generates modern Kotlin code with sequences, null safety, and lambda expressions.

## Features

| Feature | Status | Details |
|---------|--------|---------|
| Pipeline mode | ✅ | `.mapNotNull {}` for streaming |
| Generator mode | ✅ | `sequence { yield() }` |
| Recursive queries | ✅ | BFS transitive closure |
| Fact export | ✅ | `compile_facts_to_kotlin/3` |

## Why Kotlin?

- **Concise**: Less boilerplate than Java
- **Null safety**: `?.` and `?:` operators
- **Sequences**: Lazy evaluation built-in
- **Interop**: 100% Java compatible

## Architecture

```
┌─────────────────┐    ┌─────────────────┐    ┌──────────────────┐
│ Prolog Predicate│───▶│kotlin_target.pl │───▶│ Generated Kotlin │
└─────────────────┘    └─────────────────┘    └──────────────────┘
                                                      │
                                                      ▼
                                             ┌──────────────────┐
                                             │ kotlinc + Gradle │
                                             └──────────────────┘
```

## Quick Start

```prolog
% Pipeline mode
?- compile_predicate_to_kotlin(filter/2, [pipeline_input(true)], Code).

% Recursive query (transitive closure)
?- compile_recursive(ancestor/2, [target(kotlin)], Code).

% Export facts
?- compile_facts_to_kotlin(parent, 2, Code).
```

## Next Steps

- [Chapter 2: Pipeline Mode](02_pipeline_mode.md)
- [Chapter 4: Recursive Queries](04_recursive_queries.md)
