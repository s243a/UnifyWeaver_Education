# Book: Scala Target

A guide to compiling Prolog predicates to Scala with Option, pattern matching, and LazyList.

## Status: 🚧 Initial

## Contents

1. [Introduction](01_introduction.md) - Scala target basics
2. [Pipeline Mode](02_pipeline_mode.md) - Option/flatMap filtering
3. [Generator Mode](03_generator_mode.md) - LazyList with #::

## Prerequisites

- Scala 2.13+ (or 3.x)
- SBT (recommended)

## Quick Start

```prolog
?- use_module('src/unifyweaver/targets/scala_target').
?- compile_predicate_to_scala(filter/2, [pipeline_input(true)], Code).
```

## Key Features

- **Option[T]** for filtering (Some/None)
- **Pattern matching** for type-safe processing
- **LazyList** with `#::` for generators
- **@tailrec** annotation for optimization
- **SBT** build generation
- **43 bindings** (Option, Either, Collections, LazyList, Pattern Matching)

## See Also

- [JVM_TARGET.md](../../../docs/JVM_TARGET.md) - JVM family documentation
