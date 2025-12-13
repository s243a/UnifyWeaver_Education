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

## See Also

- [NATIVE_TARGET.md](../../../docs/NATIVE_TARGET.md) - Reference documentation
- [book-c-target](../book-c-target/) - C target book
