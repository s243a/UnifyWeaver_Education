<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book C++ Target - Implementation Documentation

Technical deep-dive documentation for C++ code generation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 2: Pipeline Mode | [02_pipeline_mode_impl.md](./02_pipeline_mode_impl.md) | [02_pipeline_mode_questions.md](./02_pipeline_mode_questions.md) |

## Question Count

- Chapter 2: 10 questions

## Topics Covered

- `compile_predicate_to_cpp/3` API
- std::optional for success/failure
- nlohmann/json library
- Safe field access with contains()
- JSON exception handling
- CMake integration with FetchContent
- C++17 requirements

## Source Files

- `src/unifyweaver/targets/cpp_target.pl`
