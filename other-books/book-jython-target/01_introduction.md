# Chapter 1: Introduction to the Jython Target

The Jython target generates Python 2.7-compatible code that runs on the JVM with Java interop.

## Features

| Feature | Status | Details |
|---------|--------|---------|
| Pipeline mode | ✅ | Generator functions |
| Generator mode | ✅ | `yield` statements |
| Recursive queries | ✅ | BFS with `deque` |
| Fact export | ✅ | `compile_facts_to_jython/3` |

## Why Jython?

- **Python syntax**: Familiar to many developers
- **JVM integration**: Access Java libraries
- **Generators**: Memory-efficient iteration
- **Scripting**: Easy prototyping

## Architecture

```
┌─────────────────┐    ┌──────────────────┐    ┌──────────────────┐
│ Prolog Predicate│───▶│ jython_target.pl │───▶│ Generated Python │
└─────────────────┘    └──────────────────┘    └──────────────────┘
                                                       │
                                                       ▼
                                              ┌──────────────────┐
                                              │     jython       │
                                              └──────────────────┘
```

## Quick Start

```prolog
% Pipeline mode
?- compile_predicate_to_jython(filter/2, [pipeline_input(true)], Code).

% Recursive query (transitive closure)
?- compile_recursive(ancestor/2, [target(jython)], Code).

% Export facts
?- compile_facts_to_jython(parent, 2, Code).
```

## Next Steps

- [Chapter 2: Pipeline Mode](02_pipeline_mode.md)
- [Chapter 4: Recursive Queries](04_recursive_queries.md)
