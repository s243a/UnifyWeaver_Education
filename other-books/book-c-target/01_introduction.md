# Chapter 1: Introduction to the C Target

The C target generates native C programs with cJSON for JSON handling and manual memory management.

## Features

| Feature | Status | Details |
|---------|--------|---------|
| Pipeline mode | ✅ | cJSON streaming |
| Generator mode | ✅ | Callback-based iteration |
| Recursive queries | ✅ | BFS with adjacency list |
| Fact export | ✅ | `compile_facts_to_c/3` |

## Why C?

- **Performance**: Native compilation, no runtime overhead
- **Portability**: Runs on any system with a C compiler
- **Minimal dependencies**: Only cJSON for JSON handling
- **Memory control**: Direct control over allocation

## Architecture

```
┌─────────────────┐    ┌──────────────┐    ┌─────────────────┐
│ Prolog Predicate│───▶│ c_target.pl  │───▶│ Generated C Code│
└─────────────────┘    └──────────────┘    └─────────────────┘
                                                    │
                                                    ▼
                                           ┌─────────────────┐
                                           │   gcc/clang     │
                                           └─────────────────┘
```

## Quick Start

```prolog
% Pipeline mode
?- compile_predicate_to_c(filter/2, [pipeline_input(true)], Code).

% Recursive query (transitive closure)
?- compile_recursive(ancestor/2, [target(c)], Code).

% Export facts
?- compile_facts_to_c(parent, 2, Code).
```

## Next Steps

- [Chapter 2: Pipeline Mode](02_pipeline_mode.md)
- [Chapter 4: Recursive Queries](04_recursive_queries.md)
