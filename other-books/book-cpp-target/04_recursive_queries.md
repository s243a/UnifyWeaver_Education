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

## Fact Sources And Parser Modes In C++

C++ is a natural target for explaining two practical Hybrid WAM boundaries:
where facts live and how source terms enter the runtime.

For facts, the target can choose between generated containers and external
sources. A small predicate might become a `std::vector<Fact>`. A large
predicate can be backed by an indexed store such as LMDB. The WAM call path is
the same from the reader's point of view: bound arguments become lookup keys,
candidate rows become terms, and unification decides whether each row is an
answer.

```text
edge(a, B)
  -> A1 is bound to a
  -> C++ fact source looks up rows by first argument
  -> each candidate B value is unified with A2
  -> remaining candidates become choice-point work
```

Parser support is a separate boundary. A native parser is fast and compact
when the input is canonical. A compiled parser is heavier but can support a
fuller Prolog surface. Neither parser mode changes the default Hybrid WAM
generation path; parser modes matter only when the generated program needs to
read source text at runtime.

## Hybrid WAM Role

C++ is useful for explaining the host-runtime side of Hybrid WAM: containers,
fact-source adapters, parser modes, and native library integration. Book 17
defines the shared concepts; this chapter should show why C++ can host both
compact runtime structures and pragmatic bridges to storage or parsing code.

- Default generation path: structured WAM items or target-ready WAM data
  should feed C++ generation directly.
- Symbolic WAM text: useful for debug listings and parser-related workflows,
  not as the preferred internal transport.
- Target-specific emphasis: runtime containers, parser modes, fact sources,
  and native/library integration.
