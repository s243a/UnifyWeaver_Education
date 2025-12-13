# Chapter 3: Generator Mode

Generator mode uses an iterator class pattern returning `std::vector<json>`.

## Generated Code Structure

```cpp
#include <iostream>
#include <vector>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

class TESTGenerator {
public:
    std::vector<json> process(const json& record) {
        std::vector<json> results;
        // Add multiple results
        results.push_back(record);
        return results;
    }
    
    template<typename Callback>
    void processAll(std::istream& input, Callback callback) {
        std::string line;
        while (std::getline(input, line)) {
            if (line.empty()) continue;
            try {
                json record = json::parse(line);
                for (const auto& result : process(record)) {
                    callback(result);
                }
            } catch (const json::exception& e) {
                std::cerr << "JSON parse error: " << e.what() << std::endl;
            }
        }
    }
};

int main() {
    TESTGenerator generator;
    generator.processAll(std::cin, [](const json& result) {
        std::cout << result.dump() << std::endl;
    });
    return 0;
}
```

## Iterator Class Pattern

Since C++ doesn't have built-in generators like Python, we use a class:

```cpp
class Generator {
public:
    std::vector<json> process(const json& record);
    
    template<typename Callback>
    void processAll(std::istream& input, Callback callback);
};
```

## Example: Expanding Arrays

```cpp
std::vector<json> process(const json& record) {
    std::vector<json> results;
    
    if (record.contains("items") && record["items"].is_array()) {
        for (const auto& item : record["items"]) {
            json result = record;
            result.erase("items");
            result["item"] = item;
            results.push_back(result);
        }
    }
    
    return results;
}
```

Input:
```json
{"id": 1, "items": ["a", "b", "c"]}
```

Output:
```json
{"id": 1, "item": "a"}
{"id": 1, "item": "b"}
{"id": 1, "item": "c"}
```

## Recursive Generators

```cpp
std::vector<json> process(const json& record) {
    std::vector<json> results;
    json current = record;
    constexpr int MAX_ITERATIONS = 10000;
    
    for (int i = 0; i < MAX_ITERATIONS; ++i) {
        results.push_back(current);
        
        if (isBaseCase(current)) break;
        current = transform(current);
    }
    
    return results;
}
```

## Generating Generator Code

```prolog
?- compile_predicate_to_cpp(expand/2, [generator_mode(true)], Code).
```
