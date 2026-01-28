<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Pipeline Mode - Implementation Details

Technical deep-dive for C++ pipeline code generation using std::optional.

## compile_predicate_to_cpp/3

### Signature

```prolog
compile_predicate_to_cpp(+PredicateSpec, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `PredicateSpec` | `Name/Arity` | Predicate to compile (e.g., `filter/2`) |
| `Options` | `list` | Generation options |
| `Code` | `string` | Generated C++ code |

### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `pipeline_input(Bool)` | `boolean` | `false` | Enable stdin JSONL processing |
| `cpp_standard(N)` | `integer` | `17` | C++ standard version |

### Return Value

Returns the complete C++ code as a string, ready for file output.

### Algorithm

1. **Includes**: Generate `#include` directives for iostream, optional, json
2. **Process Function**: Compile predicate to `std::optional<json>` function
3. **Pipeline Runner**: Generate `runPipeline` with getline loop
4. **Main Entry**: Add `main()` function

### Generated Code Structure

```cpp
#include <iostream>
#include <string>
#include <optional>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

std::optional<json> process(const json& record) {
    if (record.contains("value") && record["value"] > 50) {
        return record;
    }
    return std::nullopt;
}

void runPipeline() {
    std::string line;
    while (std::getline(std::cin, line)) {
        if (line.empty()) continue;
        try {
            json record = json::parse(line);
            auto result = process(record);
            if (result) {
                std::cout << result->dump() << std::endl;
            }
        } catch (const json::exception& e) {
            std::cerr << "JSON parse error: " << e.what() << std::endl;
        }
    }
}

int main() {
    runPipeline();
    return 0;
}
```

## std::optional for Filtering

### C++17 Optional Type

`std::optional<T>` represents a value that may or may not be present:

```cpp
std::optional<json> process(const json& record) {
    if (/* condition */) {
        return record;        // Has value
    }
    return std::nullopt;      // No value (filtered)
}
```

### Optional Operations

| Operation | Syntax | Description |
|-----------|--------|-------------|
| Check | `if (result)` | True if has value |
| Access | `*result` | Get value (undefined if empty) |
| Arrow | `result->field` | Access member |
| Value or | `result.value_or(def)` | Get value or default |

### Prolog to C++ Mapping

| Prolog | C++ |
|--------|-----|
| Success | `return record;` |
| Failure | `return std::nullopt;` |

## nlohmann/json Library

### Basic Operations

```cpp
#include <nlohmann/json.hpp>
using json = nlohmann::json;

// Parse JSON string
json record = json::parse(line);

// Check field existence
if (record.contains("value")) { ... }

// Access field
int value = record["value"];

// Output JSON
std::cout << record.dump() << std::endl;
```

### Safe Field Access

```cpp
// With contains check
if (record.contains("value") && record["value"] > 50) {
    return record;
}

// With value() and default
int value = record.value("value", 0);  // 0 if missing
```

## Filter Compilation

### Prolog Source

```prolog
filter(Input, Output) :-
    get_field(Input, "value", Value),
    Value > 50,
    Output = Input.
```

### Compilation Rules

| Prolog Pattern | C++ Output |
|----------------|------------|
| `get_field(R, F, V)` | `record["F"]` |
| `V > N` | `V > N` |
| `Output = Input` | `return record;` |
| Failure | `return std::nullopt;` |

### Generated Process Function

```cpp
std::optional<json> process(const json& record) {
    if (record.contains("value") && record["value"] > 50) {
        return record;
    }
    return std::nullopt;  // Filter out
}
```

## Error Handling

### JSON Parse Errors

```cpp
try {
    json record = json::parse(line);
    // Process...
} catch (const json::exception& e) {
    std::cerr << "JSON parse error: " << e.what() << std::endl;
}
```

### Exception Types

| Exception | Cause |
|-----------|-------|
| `json::parse_error` | Invalid JSON syntax |
| `json::type_error` | Wrong type access |
| `json::out_of_range` | Missing field with at() |

## CMake Integration

### CMakeLists.txt

```cmake
cmake_minimum_required(VERSION 3.14)
project(Pipeline CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)
FetchContent_Declare(
    json
    URL https://github.com/nlohmann/json/releases/download/v3.11.3/json.tar.xz
)
FetchContent_MakeAvailable(json)

add_executable(pipeline pipeline.cpp)
target_link_libraries(pipeline PRIVATE nlohmann_json::nlohmann_json)
```

### Build Commands

```bash
mkdir build && cd build
cmake ..
make
./pipeline < input.jsonl > output.jsonl
```

## Performance Considerations

### Move Semantics

For large records, consider move semantics:

```cpp
std::optional<json> process(json record) {  // Take by value
    if (record["value"] > 50) {
        return std::move(record);  // Move into optional
    }
    return std::nullopt;
}
```

### Reserve for Output

```cpp
void runPipeline() {
    std::string line;
    line.reserve(4096);  // Pre-allocate for typical line size
    // ...
}
```

## Source Files

- `src/unifyweaver/targets/cpp_target.pl`
