# Chapter 4: Recursive Queries

This chapter covers compiling recursive predicates to C++.

## Compiling to C++

```prolog
?- compile_recursive(ancestor/2, [target(cpp)], Code).
```

## Generated C++ Code

The C++ target generates a class with STL containers:

```cpp
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <queue>

class ANCESTORQuery {
private:
    std::unordered_map<std::string, std::vector<std::string>> baseRelation;

public:
    void addFact(const std::string& from, const std::string& to) {
        baseRelation[from].push_back(to);
    }

    std::vector<std::string> findAll(const std::string& start) {
        std::vector<std::string> results;
        std::unordered_set<std::string> visited;
        std::queue<std::string> queue;

        queue.push(start);
        visited.insert(start);

        while (!queue.empty()) {
            std::string current = queue.front();
            queue.pop();

            auto it = baseRelation.find(current);
            if (it != baseRelation.end()) {
                for (const auto& next : it->second) {
                    if (visited.find(next) == visited.end()) {
                        visited.insert(next);
                        queue.push(next);
                        results.push_back(next);
                    }
                }
            }
        }
        return results;
    }

    bool check(const std::string& start, const std::string& target) {
        // BFS with early exit
    }
};
```

## Running

```bash
g++ -std=c++17 -o ancestor ancestor.cpp
echo 'abraham:isaac' | ./ancestor abraham
```

## C++-Specific Features

- `unordered_map` for O(1) adjacency lookup
- `unordered_set` for O(1) visited check
- `std::queue` for BFS
- Class encapsulation for clean API

## Advanced Recursion Patterns

The C++ target also supports tail and linear recursion via multifile dispatch:

```prolog
?- compile_tail_recursion(test_sum/3, [target(cpp)], Code).
?- compile_linear_recursion(factorial/2, [target(cpp)], Code).
```

| Pattern | Multifile Predicate | C++ Idiom |
|---------|-------------------|-----------|
| Tail Recursion | `tail_recursion:compile_tail_pattern/9` | Range-based `for` with accumulator |
| Linear Recursion | `linear_recursion:compile_linear_pattern/8` | `std::unordered_map<int, int>` memoization |

### Tail Recursion Example

```cpp
int test_sum(const std::vector<int>& items) {
    int acc = 0;
    for (int item : items) {
        acc = acc + item;
    }
    return acc;
}
```

### Linear Recursion Example

```cpp
static std::unordered_map<int, int> memo;

int factorial(int n) {
    auto it = memo.find(n);
    if (it != memo.end()) return it->second;
    if (n == 0) return 1;
    int result = 1;
    for (int current = n; current >= 1; current--) {
        result = current * result;
    }
    memo[n] = result;
    return result;
}
```
