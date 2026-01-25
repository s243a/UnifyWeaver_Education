# Chapter 18: Fuzzy Logic Python Code Generation

This chapter covers compiling fuzzy logic expressions from Prolog to Python with NumPy-based implementations for efficient batch processing.

## Overview

UnifyWeaver can compile fuzzy logic DSL expressions to Python code, enabling:
- High-performance NumPy vectorized operations
- Integration with Python ML/AI ecosystems
- Batch processing of thousands of items efficiently

## Code Generation Architecture

```
Prolog DSL                    Python Target
─────────────────────────────────────────────────
f_and([w(bash, 0.9)], R)  →  R = f_and([("bash", 0.9)], _term_scores)
f_or([w(a, 0.8)], R)      →  R = f_or([("a", 0.8)], _term_scores)
f_dist_or(0.7, Terms, R)  →  R = f_dist_or(0.7, terms, _term_scores)
apply_filter(I, F, R)     →  R = apply_filter(items, lambda_filter)
```

## Using the Code Generator

### From Prolog

```prolog
:- use_module('src/unifyweaver/targets/python_fuzzy_target').

% Generate Python code for a fuzzy query
generate_code :-
    compile_fuzzy_to_python(my_query/2, [], Code),
    write(Code).
```

### Generated Python

The code generator produces Python that imports the fuzzy logic runtime:

```python
from unifyweaver.targets.python_runtime.fuzzy_logic import (
    f_and, f_or, f_dist_or, f_union, f_not,
    f_and_batch, f_or_batch, f_dist_or_batch,
    multiply_scores, blend_scores, top_k, apply_filter, apply_boost
)
import numpy as np
```

## Python Runtime API

### Core Operations (Scalar)

```python
from fuzzy_logic import f_and, f_or, f_dist_or, f_union, f_not

# Term scores from semantic search
term_scores = {'bash': 0.8, 'shell': 0.6, 'linux': 0.7}

# Weighted terms
terms = [('bash', 0.9), ('shell', 0.5)]

# Fuzzy AND: 0.9*0.8 * 0.5*0.6 = 0.216
result = f_and(terms, term_scores)

# Fuzzy OR: 1 - (1-0.72)(1-0.3) = 0.804
result = f_or(terms, term_scores)

# Distributed OR with base score
result = f_dist_or(0.7, terms, term_scores)  # 0.60816

# Non-distributed OR (union)
result = f_union(0.7, terms, term_scores)    # 0.5628

# Fuzzy NOT
result = f_not(0.3)  # 0.7
```

### Batch Operations (Vectorized)

For processing many items efficiently:

```python
from fuzzy_logic import f_and_batch, f_or_batch, f_dist_or_batch
import numpy as np

# Batch term scores (one array per term, one value per item)
term_scores_batch = {
    'bash': np.array([0.8, 0.5, 0.9, 0.3]),   # 4 items
    'shell': np.array([0.6, 0.7, 0.4, 0.8])
}

terms = [('bash', 0.9), ('shell', 0.5)]

# Process all 4 items at once
results = f_and_batch(terms, term_scores_batch)
# Returns: np.array([...]) with 4 values

# Distributed OR with per-item base scores
base_scores = np.array([0.7, 0.8, 0.6, 0.9])
results = f_dist_or_batch(base_scores, terms, term_scores_batch)
```

### Score Combination

```python
from fuzzy_logic import multiply_scores, blend_scores, top_k

# Element-wise multiplication
combined = multiply_scores(scores1, scores2)

# Blend: alpha*scores1 + (1-alpha)*scores2
blended = blend_scores(0.6, semantic_scores, fuzzy_scores)

# Get top K results
items = ['a', 'b', 'c', 'd']
scores = np.array([0.2, 0.8, 0.5, 0.9])
top = top_k(items, scores, 2)  # [('d', 0.9), ('b', 0.8)]
```

### Filtering and Boosting

```python
from fuzzy_logic import apply_filter, apply_boost

items = [
    {'id': 1, 'path': '/Unix/BASH', 'type': 'tree'},
    {'id': 2, 'path': '/Programming/Python', 'type': 'pearl'},
]
scores = np.array([0.8, 0.7])

# Filter to trees only
filtered_items, filtered_scores = apply_filter(
    items, scores,
    lambda item: item['type'] == 'tree'
)

# Boost by path match
boosted = apply_boost(
    items, scores,
    lambda item: 1.5 if 'Unix' in item['path'] else 1.0
)
```

## Filter Translation

The code generator translates Prolog filter predicates to Python lambdas:

| Prolog | Python Lambda |
|--------|---------------|
| `is_type(tree)` | `lambda item: item.get('type') == "tree"` |
| `in_subtree("Unix")` | `lambda item: "Unix" in item.get('path', '')` |
| `has_depth(3)` | `lambda item: item.get('depth') == 3` |
| `depth_between(1, 5)` | `lambda item: 1 <= item.get('depth', 0) <= 5` |
| `child_of("BASH")` | `lambda item: item.get('parent', '').endswith("BASH")` |
| `not(Filter)` | `lambda item: not (filter_fn)(item)` |

## Bookmark Filing Example

Complete example integrating semantic search with fuzzy boosting:

```python
from fuzzy_logic import f_dist_or_batch, apply_filter, top_k
import numpy as np

def file_bookmark(query: str, items: list, k: int = 10):
    """File a bookmark using semantic search + fuzzy boosting."""

    # Step 1: Get semantic search scores
    base_scores = semantic_search(query, items)  # Your search function

    # Step 2: Compute per-item term relevance
    term_scores_batch = {
        'bash': compute_term_relevance(items, 'bash'),
        'shell': compute_term_relevance(items, 'shell'),
    }

    # Step 3: Apply fuzzy boost with distributed OR
    boosted = f_dist_or_batch(
        base_scores,
        [('bash', 0.9), ('shell', 0.5)],
        term_scores_batch
    )

    # Step 4: Filter to relevant subtree
    filtered_items, filtered_scores = apply_filter(
        items, boosted,
        lambda item: 'Unix' in item.get('path', '')
    )

    # Step 5: Get top K results
    return top_k(filtered_items, filtered_scores, k)
```

## Performance Considerations

### Vectorization Benefits

NumPy batch operations are significantly faster than scalar loops:

```python
# Slow: scalar loop
results = []
for item in items:
    score = f_and(terms, get_scores(item))
    results.append(score)

# Fast: vectorized batch
results = f_and_batch(terms, batch_term_scores)
```

For 10,000 items, batch operations are typically 50-100x faster.

### Memory Layout

The batch functions expect term scores as a dictionary of arrays:

```python
# Good: contiguous arrays
term_scores_batch = {
    'bash': np.array([...]),   # Shape: (n_items,)
    'shell': np.array([...]),
}

# Avoid: list of dicts (slow)
per_item_scores = [{'bash': 0.8, 'shell': 0.6}, ...]
```

## Module Structure

```
src/unifyweaver/targets/
  python_fuzzy_target.pl       # Prolog code generator
  python_runtime/
    fuzzy_logic.py             # NumPy runtime (scalar + batch)
    test_fuzzy_logic.py        # 17 unit tests
```

## Test Coverage

The runtime includes comprehensive tests:

```bash
cd src/unifyweaver/targets/python_runtime
python3 -m unittest test_fuzzy_logic -v
```

Tests cover:
- Core operations (f_and, f_or, f_dist_or, f_union, f_not)
- Batch vectorized operations
- Score combination utilities
- Filter and boost operations
- Bookmark filing integration scenario

## Next Steps

- Chapter 19 will cover SQL target generation for database-side fuzzy queries
- Chapter 20 will cover distributed fuzzy queries across federated nodes
