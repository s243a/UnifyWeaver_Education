# Chapter 3: Generator Mode

Generator mode uses Python's native `yield` keyword for lazy evaluation.

## Generated Code Structure

```python
#!/usr/bin/env jython
from __future__ import print_function
import json

def process(record):
    """Generator: yields zero or more results per input."""
    # Yield multiple results
    yield record

def process_all(records):
    """Flatten all generator results."""
    for record in records:
        for result in process(record):
            yield result

def run_pipeline():
    for result in process_all(read_jsonl()):
        print(json.dumps(result))
```

## Python Generators

Python generators are perfect for Prolog's non-determinism:

| Prolog | Python |
|--------|--------|
| Multiple solutions | `yield` multiple times |
| Backtracking | Generator iteration |
| Lazy evaluation | Generator is lazy |

## Example: Expanding Lists

```python
def process(record):
    """Expand items list into separate records."""
    items = record.get('items', [])
    for item in items:
        result = dict(record)
        result['item'] = item
        del result['items']
        yield result
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

Tail recursion is optimized to `while` loops:

```python
def process(record):
    current = dict(record)
    max_iterations = 10000
    
    for _ in xrange(max_iterations):
        yield dict(current)
        
        # Check base case
        if is_base_case(current):
            return
        
        # Transform for next iteration
        current = transform(current)
```

## Generating Generator Code

```prolog
?- compile_predicate_to_jython(expand/2, [generator_mode(true)], Code).
```
