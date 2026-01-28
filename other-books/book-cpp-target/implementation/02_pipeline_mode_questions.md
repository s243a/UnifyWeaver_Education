<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Pipeline Mode - Questions

Q&A companion for C++ pipeline code generation.

## Question Index

1. [What is the API for compiling predicates to C++?](#bcpp02-q-api)
2. [How does std::optional work for filtering?](#bcpp02-q-optional)
3. [What JSON library is used?](#bcpp02-q-json)
4. [How are Prolog predicates translated to C++?](#bcpp02-q-translation)
5. [How do I check for optional values?](#bcpp02-q-check)
6. [How is safe field access implemented?](#bcpp02-q-safe-access)
7. [How are JSON parse errors handled?](#bcpp02-q-errors)
8. [What CMake configuration is needed?](#bcpp02-q-cmake)
9. [What C++ standard is required?](#bcpp02-q-standard)
10. [How can I optimize for large records?](#bcpp02-q-performance)

---

<a id="bcpp02-q-api"></a>
## Q1: What is the API for compiling predicates to C++?

**Question:** What is the main predicate for compiling Prolog to C++ pipeline code?

**Answer:** Use `compile_predicate_to_cpp/3`:

```prolog
?- compile_predicate_to_cpp(filter/2, [pipeline_input(true)], Code).
```

Parameters:
- `filter/2`: Predicate to compile
- `[pipeline_input(true)]`: Enable stdin JSONL processing
- `Code`: Output variable for generated C++

**See:** [02_pipeline_mode_impl.md#compile_predicate_to_cpp3](02_pipeline_mode_impl.md#compile_predicate_to_cpp3)

---

<a id="bcpp02-q-optional"></a>
## Q2: How does std::optional work for filtering?

**Question:** How does the C++ target use std::optional for filtering?

**Answer:** `std::optional<T>` represents success or failure:

```cpp
std::optional<json> process(const json& record) {
    if (record["value"] > 50) {
        return record;        // Success: has value
    }
    return std::nullopt;      // Failure: no value
}
```

The caller checks:
```cpp
auto result = process(record);
if (result) {
    std::cout << result->dump() << std::endl;
}
```

**See:** [02_pipeline_mode_impl.md#stdoptional-for-filtering](02_pipeline_mode_impl.md#stdoptional-for-filtering)

---

<a id="bcpp02-q-json"></a>
## Q3: What JSON library is used?

**Question:** What JSON library does the C++ target use?

**Answer:** The nlohmann/json library (JSON for Modern C++):

```cpp
#include <nlohmann/json.hpp>
using json = nlohmann::json;

json record = json::parse(line);
std::cout << record.dump() << std::endl;
```

It provides a modern, header-only interface with excellent ergonomics.

**See:** [02_pipeline_mode_impl.md#nlohmannjson-library](02_pipeline_mode_impl.md#nlohmannjson-library)

---

<a id="bcpp02-q-translation"></a>
## Q4: How are Prolog predicates translated to C++?

**Question:** How does the compiler translate Prolog patterns to C++?

**Answer:** Key translation rules:

| Prolog Pattern | C++ Output |
|----------------|------------|
| `get_field(R, "value", V)` | `record["value"]` |
| `V > 50` | `V > 50` |
| `Output = Input` | `return record;` |
| Failure | `return std::nullopt;` |

Example:
```cpp
std::optional<json> process(const json& record) {
    if (record.contains("value") && record["value"] > 50) {
        return record;
    }
    return std::nullopt;
}
```

**See:** [02_pipeline_mode_impl.md#filter-compilation](02_pipeline_mode_impl.md#filter-compilation)

---

<a id="bcpp02-q-check"></a>
## Q5: How do I check for optional values?

**Question:** What operations are available for std::optional?

**Answer:** Common operations:

| Operation | Syntax | Description |
|-----------|--------|-------------|
| Check | `if (result)` | True if has value |
| Dereference | `*result` | Get value |
| Arrow | `result->field` | Access member |
| Value or | `result.value_or(def)` | Get value or default |

Example:
```cpp
auto result = process(record);
if (result) {
    std::cout << result->dump() << std::endl;
}
```

**See:** [02_pipeline_mode_impl.md#optional-operations](02_pipeline_mode_impl.md#optional-operations)

---

<a id="bcpp02-q-safe-access"></a>
## Q6: How is safe field access implemented?

**Question:** How do I safely access JSON fields that might not exist?

**Answer:** Two approaches:

```cpp
// 1. Check with contains
if (record.contains("value") && record["value"] > 50) {
    // Safe
}

// 2. Use value() with default
int value = record.value("value", 0);  // 0 if missing
```

The `contains()` check prevents exceptions from missing keys.

**See:** [02_pipeline_mode_impl.md#safe-field-access](02_pipeline_mode_impl.md#safe-field-access)

---

<a id="bcpp02-q-errors"></a>
## Q7: How are JSON parse errors handled?

**Question:** How does the pipeline handle invalid JSON input?

**Answer:** With try-catch blocks:

```cpp
try {
    json record = json::parse(line);
    // Process...
} catch (const json::exception& e) {
    std::cerr << "JSON parse error: " << e.what() << std::endl;
}
```

Exception types:
- `json::parse_error`: Invalid syntax
- `json::type_error`: Wrong type access
- `json::out_of_range`: Missing field with `at()`

**See:** [02_pipeline_mode_impl.md#error-handling](02_pipeline_mode_impl.md#error-handling)

---

<a id="bcpp02-q-cmake"></a>
## Q8: What CMake configuration is needed?

**Question:** How do I set up CMake for the generated code?

**Answer:** Use FetchContent to download nlohmann/json:

```cmake
cmake_minimum_required(VERSION 3.14)
project(Pipeline CXX)

set(CMAKE_CXX_STANDARD 17)

include(FetchContent)
FetchContent_Declare(json URL https://github.com/nlohmann/json/...)
FetchContent_MakeAvailable(json)

add_executable(pipeline pipeline.cpp)
target_link_libraries(pipeline PRIVATE nlohmann_json::nlohmann_json)
```

**See:** [02_pipeline_mode_impl.md#cmake-integration](02_pipeline_mode_impl.md#cmake-integration)

---

<a id="bcpp02-q-standard"></a>
## Q9: What C++ standard is required?

**Question:** What C++ version is needed for the generated code?

**Answer:** C++17 or later is required for `std::optional`:

```cmake
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
```

Key C++17 features used:
- `std::optional<T>`
- `std::nullopt`
- Structured bindings (optional)

**See:** [02_pipeline_mode_impl.md#c17-optional-type](02_pipeline_mode_impl.md#c17-optional-type)

---

<a id="bcpp02-q-performance"></a>
## Q10: How can I optimize for large records?

**Question:** How can I improve performance when processing large JSON records?

**Answer:** Use move semantics:

```cpp
std::optional<json> process(json record) {  // Take by value
    if (record["value"] > 50) {
        return std::move(record);  // Move into optional
    }
    return std::nullopt;
}
```

Also pre-allocate strings:
```cpp
std::string line;
line.reserve(4096);  // Pre-allocate buffer
```

**See:** [02_pipeline_mode_impl.md#performance-considerations](02_pipeline_mode_impl.md#performance-considerations)
