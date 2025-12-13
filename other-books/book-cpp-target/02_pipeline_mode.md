# Chapter 2: Pipeline Mode

Pipeline mode generates modern C++ code using `std::optional` and nlohmann/json.

## Generated Code Structure

```cpp
#include <iostream>
#include <string>
#include <optional>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

std::optional<json> process(const json& record) {
    // Return result or std::nullopt to filter
    return record;
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

| Prolog | C++ |
|--------|-----|
| Success | `return record;` (implicitly Some) |
| Failure | `return std::nullopt;` |
| Check | `if (result)` |
| Access | `result->field` or `*result` |

## Example Filter

```cpp
std::optional<json> process(const json& record) {
    if (record.contains("value") && record["value"] > 50) {
        return record;
    }
    return std::nullopt;  // Filter out
}
```

## Generating Pipeline Code

```prolog
?- compile_predicate_to_cpp(filter/2, [pipeline_input(true)], Code).
```

## CMake Integration

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

## Next Steps

- [Chapter 3: Generator Mode](03_generator_mode.md) - Iterator class pattern
