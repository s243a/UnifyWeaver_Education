# Chapter 1: Introduction to the Clojure Target

The Clojure target generates idiomatic Clojure code with lazy sequences, immutable data, and functional composition.

## Features

| Feature | Status | Details |
|---------|--------|---------|
| Pipeline mode | ✅ | `->>` threading macros |
| Generator mode | ✅ | `lazy-seq` for lazy evaluation |
| Recursive queries | ✅ | `loop/recur` BFS |
| Fact export | ✅ | `compile_facts_to_clojure/3` |

## Why Clojure?

- **Lisp**: Homoiconicity, macros, REPL
- **Lazy sequences**: Infinite and efficient
- **Immutable**: Persistent data structures
- **JVM**: Java interop

## Architecture

```
┌─────────────────┐    ┌──────────────────┐    ┌───────────────────┐
│ Prolog Predicate│───▶│ clojure_target.pl│───▶│ Generated Clojure │
└─────────────────┘    └──────────────────┘    └───────────────────┘
                                                       │
                                                       ▼
                                              ┌───────────────────┐
                                              │ clj + deps.edn    │
                                              └───────────────────┘
```

## Quick Start

```prolog
% Pipeline mode
?- compile_predicate_to_clojure(filter/2, [pipeline_input(true)], Code).

% Recursive query (transitive closure)
?- compile_recursive(ancestor/2, [target(clojure)], Code).

% Export facts
?- compile_facts_to_clojure(parent, 2, Code).
```

## Next Steps

- [Chapter 2: Pipeline Mode](02_pipeline_mode.md)
- [Chapter 4: Recursive Queries](04_recursive_queries.md)
