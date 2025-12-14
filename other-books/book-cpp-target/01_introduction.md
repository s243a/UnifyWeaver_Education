# Chapter 1: Introduction to the C++ Target

The C++ target generates modern C++ programs using nlohmann/json and STL containers.

## Features

| Feature | Status | Details |
|---------|--------|---------|
| Pipeline mode | ✅ | `std::optional<json>` |
| Generator mode | ✅ | `std::vector<json>` |
| Recursive queries | ✅ | BFS with `unordered_map` |
| Fact export | ✅ | `compile_facts_to_cpp/3` |

## Why C++?

- **Modern features**: std::optional, auto, lambdas
- **Type safety**: Strong typing with templates
- **Header-only JSON**: nlohmann/json requires no linking
- **STL integration**: Familiar containers and algorithms

## Architecture

```
┌─────────────────┐    ┌──────────────┐    ┌───────────────────┐
│ Prolog Predicate│───▶│cpp_target.pl │───▶│ Generated C++ Code│
└─────────────────┘    └──────────────┘    └───────────────────┘
                                                    │
                                                    ▼
                                           ┌───────────────────┐
                                           │ CMake + g++/clang │
                                           └───────────────────┘
```

## Quick Start

```prolog
% Pipeline mode
?- compile_predicate_to_cpp(filter/2, [pipeline_input(true)], Code).

% Recursive query (transitive closure)
?- compile_recursive(ancestor/2, [target(cpp)], Code).

% Export facts
?- compile_facts_to_cpp(parent, 2, Code).
```

## Next Steps

- [Chapter 2: Pipeline Mode](02_pipeline_mode.md)
- [Chapter 4: Recursive Queries](04_recursive_queries.md)
