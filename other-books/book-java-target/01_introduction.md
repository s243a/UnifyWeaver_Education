# Chapter 1: Introduction to the Java Target

The Java target generates Java programs for processing data, supporting both JSONL pipeline operations and classic recursive queries like `ancestor`.

## What's Implemented

| Feature | Status | Notes |
|---------|--------|-------|
| Pipeline mode | ✅ | Streaming JSONL with Gson |
| Generator mode | ✅ | Stream.flatMap for multiple outputs |
| Simple mode | ✅ | Basic predicate structure |
| Gradle build | ✅ | Build file generation |
| Tail recursion | ✅ | While loop optimization |
| **Recursive query** | ✅ | **Transitive closure (ancestor)** |
| **Fact export** | ✅ | **compile_facts_to_java/3** |

## The Classic Example: Family Tree

Just like the Bash target in Book 2, you can compile the family tree predicates:

```prolog
% Load family tree
?- ['education/book-02-bash-target/examples/family_tree'].

% Load recursive compiler with Java target
?- use_module('src/unifyweaver/core/recursive_compiler').

% Compile ancestor to Java
?- compile_recursive(ancestor/2, [target(java)], Code).
```

This generates a complete Java class with BFS-based transitive closure!

## Compiling Facts

Export facts as static Java data:

```prolog
?- use_module('src/unifyweaver/targets/java_target').
?- compile_facts_to_java(parent, 2, Code).
```

Generates a `PARENT` class with all facts as a `List<String[]>`.

## Architecture

```
┌─────────────────┐    ┌────────────────────┐    ┌──────────────────┐
│ Prolog Source   │    │ recursive_compiler │    │ Generated Java   │
│                 │───▶│ + java_target      │───▶│                  │
│ ancestor(X,Y) :-│    │                    │    │ Ancestor.java    │
│   parent(X,Y).  │    │ target(java)       │    │ (BFS fixpoint)   │
└─────────────────┘    └────────────────────┘    └──────────────────┘
```

## Comparison with Bash Target

| Capability | Bash Target | Java Target |
|------------|-------------|-------------|
| `compile_recursive/3` | ✅ Ancestor queries | ✅ BFS implementation |
| `compile_facts` | ✅ Bash arrays | ✅ Java List/Stream |
| Pipeline mode | via jq | ✅ Gson + Stream API |
| Generator mode | pipes | ✅ Stream.flatMap |
| Performance | Good | Better (JVM optimized) |

## Quick Start

### Pipeline Mode (JSONL Processing)
```prolog
?- use_module('src/unifyweaver/targets/java_target').
?- compile_predicate_to_java(filter/2, [pipeline_input(true)], Code).
```

### Recursive Queries (Ancestor)
```prolog
?- use_module('src/unifyweaver/core/recursive_compiler').
?- compile_recursive(ancestor/2, [target(java)], Code).
```

### Fact Export
```prolog
?- compile_facts_to_java(parent, 2, Code).
```

## Next Steps

- [Chapter 2: Pipeline Mode](02_pipeline_mode.md) - JSONL processing
- [Chapter 3: Generator Mode](03_generator_mode.md) - Multiple outputs
- [Chapter 4: Recursive Queries](04_recursive_queries.md) - Ancestor example
