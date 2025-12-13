# Chapter 1: Introduction to the Clojure Target

The Clojure target generates idiomatic Clojure code with lazy sequences, threading macros, and immutable data.

## Why Clojure?

- **Lisp**: Homoiconic, code as data
- **Lazy**: Infinite sequences by default
- **Immutable**: Persistent data structures
- **REPL**: Interactive development

## Architecture

```
┌─────────────────┐    ┌──────────────────┐    ┌───────────────────┐
│ Prolog Predicate│───▶│clojure_target.pl │───▶│ Generated Clojure │
└─────────────────┘    └──────────────────┘    └───────────────────┘
                                                       │
                                                       ▼
                                              ┌───────────────────┐
                                              │ clojure + deps.edn│
                                              └───────────────────┘
```

## Modes

| Mode | Use Case | Pattern |
|------|----------|---------|
| Simple | Basic predicates | (println) |
| Pipeline | Stream processing | (keep process) |
| Generator | Multiple outputs | (lazy-seq (cons)) |

## Quick Start

```prolog
?- use_module('src/unifyweaver/targets/clojure_target').
?- compile_predicate_to_clojure(filter/2, [pipeline_input(true)], Code).
```

## Next Steps

- [Chapter 2: Pipeline Mode](02_pipeline_mode.md)
