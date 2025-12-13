# Book: Kotlin Target

A guide to compiling Prolog predicates to Kotlin with sequences, lambdas, and null safety.

## Status: 🚧 Initial

## Contents

1. [Introduction](01_introduction.md) - Kotlin target basics
2. [Pipeline Mode](02_pipeline_mode.md) - Sequences and lambdas
3. [Generator Mode](03_generator_mode.md) - sequence { yield() }

## Prerequisites

- Kotlin 2.0+
- Gradle (recommended)

## Quick Start

```prolog
?- use_module('src/unifyweaver/targets/kotlin_target').
?- compile_predicate_to_kotlin(filter/2, [pipeline_input(true)], Code).
```

## Key Features

- **Kotlin sequences** for lazy evaluation
- **Null safety** with `?.` and `?:`
- **Lambda expressions** for functional style
- **sequence { yield() }** for generators
- **Gradle** build generation
- **44 bindings** (Stdlib, Collections, Strings, I/O, Sequences)

## See Also

- [JVM_TARGET.md](../../../docs/JVM_TARGET.md) - JVM family documentation
