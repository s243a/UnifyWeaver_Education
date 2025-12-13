# Chapter 1: Introduction to the Jython Target

The Jython target generates Python code that runs on the JVM, combining Python's simplicity with Java's libraries.

## Why Jython?

- **Python syntax**: Familiar, readable code
- **Java interop**: Access to all Java libraries
- **Generators**: Native `yield` support
- **Scripting**: Easy to modify and extend

## Architecture

```
┌─────────────────┐    ┌─────────────────┐    ┌──────────────────┐
│ Prolog Predicate│───▶│jython_target.pl │───▶│ Generated Jython │
└─────────────────┘    └─────────────────┘    └──────────────────┘
                                                      │
                                                      ▼
                                             ┌──────────────────┐
                                             │ Jython Runtime   │
                                             └──────────────────┘
```

## Modes

| Mode | Use Case | Pattern |
|------|----------|---------|
| Simple | Basic predicates | print() |
| Pipeline | Stream processing | for/yield |
| Generator | Multiple outputs | def process(): yield |

## Quick Start

```prolog
?- use_module('src/unifyweaver/targets/jython_target').
?- compile_predicate_to_jython(filter/2, [pipeline_input(true)], Code).
```

## Next Steps

- [Chapter 2: Pipeline Mode](02_pipeline_mode.md)
