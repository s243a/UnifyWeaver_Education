# Book: C Target

A guide to compiling Prolog predicates to C programs for high-performance JSONL pipeline processing.

## Status: 🚧 Initial

## Contents

1. [Introduction](01_introduction.md) - C target basics
2. [Pipeline Mode](02_pipeline_mode.md) - Streaming JSONL with cJSON
3. [Generator Mode](03_generator_mode.md) - Callback-based iteration
4. [Build Systems](04_build_systems.md) - Makefile and CMake generation

## Prerequisites

- GCC or Clang compiler
- cJSON library
- Basic understanding of C programming

## Quick Start

```prolog
?- use_module('src/unifyweaver/targets/c_target').
?- compile_predicate_to_c(filter/2, [pipeline_input(true)], Code).
```

## Key Features

- **cJSON** for JSON parsing (MIT license)
- **Tail recursion** → while loop optimization  
- **General recursion** → explicit stack pattern
- **Makefile/CMake** generation
- **41 bindings** (stdlib, I/O, strings, cJSON)

## Generating C Hybrid WAM Code

Use the WAM C target for explicit C runtime generation:

```prolog
?- use_module('src/unifyweaver/targets/wam_c_target').
?- write_wam_c_project([ancestor/2],
       [reverse_index(auto)],
       'out/wam_c_ancestor').
```

The C target emits runtime state, instruction data, and setup code such as
reverse-index artifacts when those options are enabled. Use this path when the
point is WAM state, explicit memory, or C FFI boundaries.

## Hybrid WAM Role

C is the low-level memory-layout reference point for WAM state, lifecycle, and
foreign-call boundaries. Book 17 covers the shared concepts; this book shows
where those concepts become explicit data structures and ownership rules.

## See Also

- [NATIVE_TARGET.md](../../../docs/NATIVE_TARGET.md) - Reference documentation
- [book-cpp-target](../book-cpp-target/) - C++ target book
