# Chapter 1: Introduction to the Scala Target

The Scala target generates functional Scala code with Option, pattern matching, and LazyList.

## Why Scala?

- **Functional**: First-class functions, immutability
- **Pattern matching**: Powerful type-safe destructuring
- **Option[T]**: Safe null handling
- **LazyList**: Infinite lazy sequences

## Architecture

```
┌─────────────────┐    ┌─────────────────┐    ┌──────────────────┐
│ Prolog Predicate│───▶│scala_target.pl  │───▶│ Generated Scala  │
└─────────────────┘    └─────────────────┘    └──────────────────┘
                                                      │
                                                      ▼
                                             ┌──────────────────┐
                                             │ scalac + SBT     │
                                             └──────────────────┘
```

## Modes

| Mode | Use Case | Pattern |
|------|----------|---------|
| Simple | Basic predicates | println() |
| Pipeline | Stream processing | Option[T].flatMap |
| Generator | Multiple outputs | LazyList #:: |

## Quick Start

```prolog
?- use_module('src/unifyweaver/targets/scala_target').
?- compile_predicate_to_scala(filter/2, [pipeline_input(true)], Code).
```

## Next Steps

- [Chapter 2: Pipeline Mode](02_pipeline_mode.md)
