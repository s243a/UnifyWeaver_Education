# Book: Clojure Target

A guide to compiling Prolog predicates to Clojure with lazy sequences and threading macros.

## Status: 🚧 Initial

## Contents

1. [Introduction](01_introduction.md) - Clojure target basics
2. [Pipeline Mode](02_pipeline_mode.md) - keep/filter processing
3. [Generator Mode](03_generator_mode.md) - lazy-seq with cons

## Prerequisites

- Clojure CLI 1.11+
- deps.edn (recommended)

## Quick Start

```prolog
?- use_module('src/unifyweaver/targets/clojure_target').
?- compile_predicate_to_clojure(filter/2, [pipeline_input(true)], Code).
```

## Key Features

- **Lazy sequences** with `lazy-seq` and `cons`
- **Threading macros** (`->>`) for pipelines
- **loop/recur** for tail recursion
- **clojure.data.json** for JSON handling
- **deps.edn** build generation
- **64 bindings** (Core, Collections, Sequences, Strings, Threading Macros)

## See Also

- [JVM_TARGET.md](../../../docs/JVM_TARGET.md) - JVM family documentation
