# Chapter 1: Introduction to the Kotlin Target

The Kotlin target generates modern Kotlin code with sequences, null safety, and lambda expressions.

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

## Modes

| Mode | Use Case | Pattern |
|------|----------|---------|
| Simple | Basic predicates | println() |
| Pipeline | Stream processing | .mapNotNull {} |
| Generator | Multiple outputs | sequence { yield() } |

## Quick Start

```prolog
?- use_module('src/unifyweaver/targets/kotlin_target').
?- compile_predicate_to_kotlin(filter/2, [pipeline_input(true)], Code).
```

## Next Steps

- [Chapter 2: Pipeline Mode](02_pipeline_mode.md)
