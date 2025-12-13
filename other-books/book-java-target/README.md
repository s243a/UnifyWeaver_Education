# Book: Java Target

A guide to compiling Prolog predicates to Java programs with Gson JSON handling and Stream API support.

## Status: 🚧 Initial

## Contents

1. [Introduction](01_introduction.md) - Java target basics
2. [Pipeline Mode](02_pipeline_mode.md) - Streaming JSONL with Gson
3. [Generator Mode](03_generator_mode.md) - Stream.flatMap iteration

## Prerequisites

- JDK 11+
- Gson library
- Gradle (optional)

## Quick Start

```prolog
?- use_module('src/unifyweaver/targets/java_target').
?- compile_predicate_to_java(filter/2, [pipeline_input(true)], Code).
```

## Key Features

- **Gson** for JSON parsing
- **Stream API** for pipeline processing
- **Stream.flatMap** for generator mode
- **Gradle** build generation
- **42 bindings** (String, Math, Collections, I/O, Stream API)

## See Also

- [JVM_TARGET.md](../../../docs/JVM_TARGET.md) - JVM family documentation
