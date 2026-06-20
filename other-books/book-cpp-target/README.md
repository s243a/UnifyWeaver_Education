# Book: C++ Target

A guide to compiling Prolog predicates to modern C++ programs using nlohmann/json and STL containers.

## Status: 🚧 Initial

## Contents

1. [Introduction](01_introduction.md) - Modern C++ target basics
2. [Pipeline Mode](02_pipeline_mode.md) - std::optional filtering
3. [Generator Mode](03_generator_mode.md) - Iterator class pattern
4. [CMake Integration](04_cmake_integration.md) - FetchContent for dependencies

## Prerequisites

- C++17 compatible compiler (g++ 9+, clang++ 10+)
- CMake 3.14+
- Basic understanding of modern C++

## Quick Start

```prolog
?- use_module('src/unifyweaver/targets/cpp_target').
?- compile_predicate_to_cpp(filter/2, [pipeline_input(true)], Code).
```

## Key Features

- **nlohmann/json** for JSON (header-only, MIT license)
- **std::optional** for filtering semantics
- **Iterator classes** for generator mode
- **Modern C++17** features (auto, constexpr)
- **CMake FetchContent** for automatic dependency download
- **45 bindings** (STL, iostream, algorithms, nlohmann/json)

## Generating C++ Hybrid WAM Code

Use the WAM C++ target for the hybrid runtime with optional lowered functions:

```prolog
?- use_module('src/unifyweaver/targets/wam_cpp_target').
?- write_wam_cpp_project([ancestor/2],
       [emit_mode(functions), runtime_parser(native)],
       'out/wam_cpp_ancestor').
```

`emit_mode(interpreter)` keeps everything in the instruction-array runtime.
`emit_mode(functions)` emits direct C++ functions for lowerable predicates.
`runtime_parser(native)` is useful for canonical term parsing; use
`runtime_parser(compiled)` only when operator-aware Prolog source parsing is
needed and the compile-time cost is acceptable.

## Hybrid WAM Role

C++ is useful for explaining runtime containers, parser support, LMDB-style
fact sources, and compiled/native parser tradeoffs. Book 17 covers the shared
Hybrid WAM architecture; this book should focus on C++ runtime choices.

## See Also

- [NATIVE_TARGET.md](../../../docs/NATIVE_TARGET.md) - Reference documentation
- [book-c-target](../book-c-target/) - C target book
