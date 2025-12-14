# Chapter 1: Introduction to the Scala Target

The Scala target generates idiomatic Scala code with LazyList, pattern matching, and case classes.

## Features

| Feature | Status | Details |
|---------|--------|---------|
| Pipeline mode | ✅ | `Option[Record]` for filtering |
| Generator mode | ✅ | `LazyList` for lazy evaluation |
| Recursive queries | ✅ | BFS transitive closure |
| Fact export | ✅ | `compile_facts_to_scala/3` |

## Why Scala?

- **Functional**: First-class functions, immutable data
- **LazyList**: Memory-efficient stream processing
- **Pattern matching**: Expressive case analysis
- **JVM**: Access to Java libraries

## Architecture

```
┌─────────────────┐    ┌─────────────────┐    ┌──────────────────┐
│ Prolog Predicate│───▶│ scala_target.pl │───▶│ Generated Scala  │
└─────────────────┘    └─────────────────┘    └──────────────────┘
                                                      │
                                                      ▼
                                             ┌──────────────────┐
                                             │   scalac + sbt   │
                                             └──────────────────┘
```

## Quick Start

```prolog
% Pipeline mode
?- compile_predicate_to_scala(filter/2, [pipeline_input(true)], Code).

% Recursive query (transitive closure)
?- compile_recursive(ancestor/2, [target(scala)], Code).

% Export facts
?- compile_facts_to_scala(parent, 2, Code).
```

## Next Steps

- [Chapter 2: Pipeline Mode](02_pipeline_mode.md)
- [Chapter 4: Recursive Queries](04_recursive_queries.md)
