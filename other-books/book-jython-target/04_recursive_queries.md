# Chapter 4: Recursive Queries

This chapter covers compiling recursive predicates to Jython.

## Compiling to Jython

```prolog
?- compile_recursive(ancestor/2, [target(jython)], Code).
```

## Generated Jython Code

```python
from collections import deque
import sys

class ancestorQuery:
    def __init__(self):
        self.base_relation = {}

    def add_fact(self, from_node, to_node):
        if from_node not in self.base_relation:
            self.base_relation[from_node] = set()
        self.base_relation[from_node].add(to_node)

    def find_all(self, start):
        visited = set([start])
        queue = deque([start])
        results = []

        while queue:
            current = queue.popleft()
            for next_node in self.base_relation.get(current, set()):
                if next_node not in visited:
                    visited.add(next_node)
                    queue.append(next_node)
                    results.append(next_node)
        return results

    def check(self, start, target):
        if start == target:
            return False
        visited = set([start])
        queue = deque([start])
        while queue:
            current = queue.popleft()
            for next_node in self.base_relation.get(current, set()):
                if next_node == target:
                    return True
                if next_node not in visited:
                    visited.add(next_node)
                    queue.append(next_node)
        return False
```

## Running

```bash
jython ancestor_query.py abraham < facts.txt
```

## Jython-Specific Features

- `collections.deque` for BFS queue
- Python dict with set values
- Java interop available
- Same API as CPython
