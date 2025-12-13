# Chapter 1: Introduction to the Java Target

The Java target generates Java programs that process JSONL data using Gson for JSON handling and the Stream API for pipeline operations.

## Why Java?

- **Enterprise ready**: Widely used in production environments
- **Stream API**: Functional-style data processing
- **Strong typing**: Compile-time type checking
- **Ecosystem**: Extensive library support

## Architecture

```
┌─────────────────┐    ┌───────────────┐    ┌──────────────────┐
│ Prolog Predicate│───▶│java_target.pl │───▶│ Generated Java   │
└─────────────────┘    └───────────────┘    └──────────────────┘
                                                    │
                                                    ▼
                                           ┌──────────────────┐
                                           │ javac + Gradle   │
                                           └──────────────────┘
```

## Modes

| Mode | Use Case | Return Type |
|------|----------|-------------|
| Simple | Basic predicates | void |
| Pipeline | Stream processing | Map<String, Object> |
| Generator | Multiple outputs | Stream<Map<String, Object>> |

## Quick Start

```prolog
?- use_module('src/unifyweaver/targets/java_target').
?- compile_predicate_to_java(filter/2, [pipeline_input(true)], Code).
```

## Next Steps

- [Chapter 2: Pipeline Mode](02_pipeline_mode.md)
