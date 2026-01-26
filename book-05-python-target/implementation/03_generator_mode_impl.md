<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3 Implementation: Generator Mode - Fixpoint Evaluation

**Detailed function documentation for RAG systems**

This document provides implementation details for Python's generator mode which uses semi-naive fixpoint evaluation.

---

## Table of Contents

1. [Semi-Naive Evaluation Algorithm](#semi-naive-evaluation-algorithm)
2. [FrozenDict Data Structure](#frozendict-data-structure)
3. [process_stream_generator()](#process_stream_generator)
4. [Rule Application](#rule-application)
5. [Negation Support](#negation-support)
6. [Performance Characteristics](#performance-characteristics)

---

## Semi-Naive Evaluation Algorithm

### Overview

Generator mode computes recursive queries by iterating to a fixpoint (no new facts derived).

**Key sets**:
- `total` - All facts discovered so far
- `delta` - Facts discovered in current iteration (new facts)

### Algorithm Steps

```
1. Initialize total, delta with input facts
2. WHILE delta is not empty:
   a. new_delta = empty set
   b. FOR each fact in delta:
      - Apply all rules
      - IF derived fact not in total:
        - Add to total
        - Add to new_delta
        - Yield (output) fact
   c. delta = new_delta
3. Fixpoint reached - done
```

### Why Semi-Naive?

**Naive approach**: Apply rules to ALL facts in `total` each iteration.
- Problem: O(n²) redundant work

**Semi-naive approach**: Only consider facts in `delta` (new ones).
- Optimization: Avoid recomputing known derivations
- Same result, much faster

### Example Trace

For transitive closure with edges `a→b→c→d`:

| Iteration | delta | total | Derived |
|-----------|-------|-------|---------|
| 0 | {a→b, b→c, c→d} | {a→b, b→c, c→d} | (input) |
| 1 | {a→c, b→d} | +{a→c, b→d} | a→c, b→d |
| 2 | {a→d} | +{a→d} | a→d |
| 3 | {} | (no change) | (fixpoint) |

---

## FrozenDict Data Structure

Python's `dict` is mutable and unhashable, so can't be stored in sets. `FrozenDict` solves this.

### Implementation

```python
class FrozenDict:
    """Immutable, hashable dictionary for use in sets."""

    def __init__(self, items: Tuple[Tuple[str, Any], ...]):
        self._items = items
        self._hash = hash(items)

    @staticmethod
    def from_dict(d: Dict) -> 'FrozenDict':
        """Convert dict to FrozenDict."""
        return FrozenDict(tuple(sorted(d.items())))

    def to_dict(self) -> Dict:
        """Convert back to regular dict."""
        return dict(self._items)

    def get(self, key: str, default=None):
        """Get value by key."""
        for k, v in self._items:
            if k == key:
                return v
        return default

    def __hash__(self):
        return self._hash

    def __eq__(self, other):
        return self._items == other._items
```

### Key Properties

| Property | Purpose |
|----------|---------|
| Immutable | Cannot be modified after creation |
| Hashable | Can be used as set elements |
| Sorted items | Ensures equal dicts hash equally |
| O(1) hash | Precomputed for performance |

### Usage

```python
# Create
fd = FrozenDict.from_dict({'arg0': 'a', 'arg1': 'b'})

# Use in set
facts = set()
facts.add(fd)

# Check membership (O(1) average)
fd2 = FrozenDict.from_dict({'arg0': 'a', 'arg1': 'b'})
print(fd2 in facts)  # True

# Convert back
d = fd.to_dict()  # {'arg0': 'a', 'arg1': 'b'}
```

---

## process_stream_generator()

```python
def process_stream_generator(records: Iterator[Dict]) -> Iterator[Dict]
```

**Purpose**: Main entry point for semi-naive fixpoint evaluation.

### Implementation

```python
def process_stream_generator(records: Iterator[Dict]) -> Iterator[Dict]:
    total: Set[FrozenDict] = set()
    delta: Set[FrozenDict] = set()

    # Initialize with input facts
    for record in records:
        frozen = FrozenDict.from_dict(record)
        if frozen not in total:
            total.add(frozen)
            delta.add(frozen)
            yield record  # Output immediately

    # Fixpoint iteration
    while delta:
        new_delta: Set[FrozenDict] = set()

        for fact in delta:
            # Apply each rule
            for new_fact in _apply_rules(fact, total):
                if new_fact not in total:
                    total.add(new_fact)
                    new_delta.add(new_fact)
                    yield new_fact.to_dict()

        delta = new_delta
```

### Key Design Decisions

1. **Immediate output**: Input facts yielded during initialization
2. **Streaming**: Derived facts yielded as discovered
3. **Deduplication**: `total` set prevents duplicates
4. **Termination**: Empty `delta` = fixpoint reached

---

## Rule Application

### Copy Rules (Fact Projection)

```prolog
path(X, Y) :- edge(X, Y).
```

Input facts matching `edge(X, Y)` are copied to `path(X, Y)`:

```python
def _apply_copy_rule(fact: FrozenDict) -> Iterator[FrozenDict]:
    # Just yield the fact with potentially renamed relation
    yield FrozenDict.from_dict({
        'arg0': fact.get('arg0'),
        'arg1': fact.get('arg1')
    })
```

### Binary Joins

```prolog
path(X, Z) :- edge(X, Y), path(Y, Z).
```

Joins `edge` and `path` on shared variable `Y`:

```python
def _apply_join_rule(fact: FrozenDict, total: Set[FrozenDict]) -> Iterator[FrozenDict]:
    """path(X, Z) :- edge(X, Y), path(Y, Z)."""
    x = fact.get('arg0')
    y = fact.get('arg1')

    if x is None or y is None:
        return

    # Look for path(Y, Z) in total
    for other in total:
        if other.get('arg0') == y:  # Join on Y
            z = other.get('arg1')
            new_fact = FrozenDict.from_dict({'arg0': x, 'arg1': z})
            yield new_fact
```

### N-Way Joins

```prolog
triple_path(X, W) :- edge(X, Y), edge(Y, Z), edge(Z, W).
```

Multiple goals joined sequentially:

```python
def _apply_nway_join(fact: FrozenDict, total: Set[FrozenDict]) -> Iterator[FrozenDict]:
    x = fact.get('arg0')
    y = fact.get('arg1')

    for edge2 in total:
        if edge2.get('arg0') == y:
            z = edge2.get('arg1')
            for edge3 in total:
                if edge3.get('arg0') == z:
                    w = edge3.get('arg1')
                    yield FrozenDict.from_dict({'arg0': x, 'arg1': w})
```

---

## Negation Support

### Stratified Negation-as-Failure

```prolog
blocked(b, c).
safe_path(X, Y) :- edge(X, Y), \+ blocked(X, Y).
```

**Stratification requirement**: Negated predicates must not depend on the predicate being defined.

### Implementation

```python
def _apply_rule_with_negation(fact: FrozenDict, total: Set[FrozenDict]) -> Iterator[FrozenDict]:
    x = fact.get('arg0')
    y = fact.get('arg1')

    # Check negation: \+ blocked(X, Y)
    blocked_fact = FrozenDict.from_dict({
        'relation': 'blocked',
        'arg0': x,
        'arg1': y
    })

    if blocked_fact in total:
        return  # Negation fails - blocked fact exists

    # Negation succeeds - continue with rule
    yield FrozenDict.from_dict({'arg0': x, 'arg1': y})
```

### Stratification Validation

The compiler validates stratification at compile time:

```prolog
% VALID: blocked does not depend on safe_path
safe_path(X, Y) :- edge(X, Y), \+ blocked(X, Y).

% INVALID: negated predicate depends on itself
bad(X) :- foo(X), \+ bad(X).  % ERROR: Unstratified negation
```

---

## Performance Characteristics

### Time Complexity

| Operation | Complexity | Notes |
|-----------|------------|-------|
| Set membership | O(1) average | Hash-based lookup |
| Binary join | O(n × m) | n delta facts × m total facts |
| N-way join | O(n^k) | k = number of joins |
| Overall fixpoint | O(n²) worst | For binary joins |

### Memory Complexity

| Component | Memory | Notes |
|-----------|--------|-------|
| `total` set | O(facts) | All discovered facts |
| `delta` set | O(facts/iteration) | New facts per iteration |
| FrozenDict | O(fields) | Per fact overhead |

### Optimization Tips

1. **Minimize fact size**: Smaller facts = faster hashing
   ```python
   # Prefer
   {"id": 123}
   # Over
   {"id": 123, "metadata": {...}}
   ```

2. **Filter early**: Pre-filter input before fixpoint
   ```bash
   jq 'select(.weight > 0)' | python3 path.py
   ```

3. **Use NUL-delimited format for large inputs**:
   ```prolog
   compile_predicate_to_python(path/2, [mode(generator), record_format(nul_json)], Code).
   ```

---

## Comparison: Python Generator vs Bash BFS

| Aspect | Python Generator | Bash BFS |
|--------|------------------|----------|
| Algorithm | Semi-naive fixpoint | BFS queue |
| Recursion | No limit (iteration) | No limit (loop) |
| Memory | O(all facts) | O(frontier) |
| Speed | Good | Good |
| Ecosystem | Python tools | Unix pipelines |

**Choose Python Generator when**:
- Need Python ecosystem integration
- Want readable generated code
- Complex negation patterns

**Choose Bash BFS when**:
- Unix pipeline integration
- Shell scripting environment
- Simpler deployment

---

## Debugging

### Print Iteration Progress

```python
iteration = 0
while delta:
    iteration += 1
    print(f"Iteration {iteration}: {len(delta)} new facts", file=sys.stderr)
    # ... rest of loop
```

### Inspect Total Set

```python
print(f"Total facts: {len(total)}", file=sys.stderr)
for fact in total:
    print(f"  {fact.to_dict()}", file=sys.stderr)
```

### Trace Rule Application

```python
def _apply_rule(fact, total):
    print(f"Applying rule to {fact.to_dict()}", file=sys.stderr)
    # ... rule logic
```

---

## Source Files

- `src/unifyweaver/targets/python_target.pl`
- `src/unifyweaver/targets/python_runtime/` (runtime support)

## See Also

- Chapter 3: Generator Mode (tutorial)
- Book 3: C# Target (similar semi-naive approach)
- Chapter 4: Recursion Patterns
