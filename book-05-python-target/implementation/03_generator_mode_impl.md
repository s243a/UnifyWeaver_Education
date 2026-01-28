<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: Generator Mode - Implementation Details

This document provides function-level documentation for Python generator mode compilation.

**Source**: `src/unifyweaver/targets/python_target.pl`

---

## Overview: Semi-Naive Fixpoint Evaluation

Generator mode implements the semi-naive algorithm from Datalog:

| Concept | Description |
|---------|-------------|
| **total** | All facts discovered so far |
| **delta** | Facts discovered in current iteration (new facts only) |
| **fixpoint** | When delta is empty, no new facts can be derived |
| **semi-naive** | Only apply rules to delta, not all of total |

---

## compile_predicate_to_python/3

Compiles a Prolog predicate to Python with generator mode.

### Signature

```prolog
compile_predicate_to_python(+Predicate/Arity, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicate/Arity` | `atom/integer` | The predicate to compile |
| `Options` | `list` | Must include `mode(generator)` |
| `Code` | `string` | Generated Python source |

### Options

| Option | Values | Default | Description |
|--------|--------|---------|-------------|
| `mode(M)` | `procedural`, `generator` | `procedural` | Evaluation mode |
| `record_format(F)` | `json`, `nul_json` | `json` | Input format |

### Example

```prolog
?- compile_predicate_to_python(path/2, [mode(generator)], Code).
```

---

## FrozenDict Class

Immutable, hashable dictionary for use in Python sets.

### Purpose

Python's `dict` is mutable and unhashable, so it cannot be stored in a `set`. FrozenDict provides:
- **Immutability** - Cannot be modified after creation
- **Hashability** - Can be used as set elements
- **Dictionary interface** - `get()` method for value access

### Implementation

```python
class FrozenDict:
    def __init__(self, items: Tuple[Tuple[str, Any], ...]):
        self._items = items
        self._hash = hash(items)

    @staticmethod
    def from_dict(d: Dict) -> 'FrozenDict':
        return FrozenDict(tuple(sorted(d.items())))

    def to_dict(self) -> Dict:
        return dict(self._items)

    def get(self, key: str, default=None):
        for k, v in self._items:
            if k == key:
                return v
        return default

    def __hash__(self):
        return self._hash

    def __eq__(self, other):
        return self._items == other._items
```

### Usage

```python
# Create from dict
fd = FrozenDict.from_dict({'arg0': 'alice', 'arg1': 'bob'})

# Use in set
facts = set()
facts.add(fd)

# Check membership (O(1) average)
fd2 = FrozenDict.from_dict({'arg0': 'alice', 'arg1': 'bob'})
print(fd2 in facts)  # True

# Access values
print(fd.get('arg0'))  # 'alice'

# Convert back to dict
d = fd.to_dict()  # {'arg0': 'alice', 'arg1': 'bob'}
```

---

## process_stream_generator Function

Main entry point for fixpoint evaluation.

### Signature

```python
def process_stream_generator(records: Iterator[Dict]) -> Iterator[Dict]
```

### Algorithm

```
1. Initialize total = {}, delta = {}
2. For each input record:
   a. Convert to FrozenDict
   b. Add to total and delta if new
   c. Yield the record
3. While delta is not empty:
   a. new_delta = {}
   b. For each fact in delta:
      - Apply all rules
      - Add new facts to new_delta and total
      - Yield new facts
   c. delta = new_delta
4. Done (fixpoint reached)
```

### Generated Code Structure

```python
def process_stream_generator(records: Iterator[Dict]) -> Iterator[Dict]:
    total: Set[FrozenDict] = set()
    delta: Set[FrozenDict] = set()

    # Phase 1: Initialize with input
    for record in records:
        frozen = FrozenDict.from_dict(record)
        if frozen not in total:
            total.add(frozen)
            delta.add(frozen)
            yield record

    # Phase 2: Fixpoint iteration
    while delta:
        new_delta: Set[FrozenDict] = set()

        for fact in delta:
            for new_fact in _apply_rules(fact, total):
                if new_fact not in total:
                    total.add(new_fact)
                    new_delta.add(new_fact)
                    yield new_fact.to_dict()

        delta = new_delta
```

---

## Rule Application

### Copy Rules

```prolog
path(X, Y) :- edge(X, Y).
```

Facts matching `edge(X, Y)` are copied to `path(X, Y)`. In generator mode, input facts are already in `total`, so copy rules are implicit.

### Binary Joins

```prolog
path(X, Z) :- edge(X, Y), path(Y, Z).
```

Generated Python:

```python
def _apply_rule_join(fact: FrozenDict, total: Set[FrozenDict]) -> Iterator[FrozenDict]:
    """path(X, Z) :- edge(X, Y), path(Y, Z)."""
    x = fact.get('arg0')
    y = fact.get('arg1')
    if x is not None and y is not None:
        for other in total:
            if other.get('arg0') == y:  # Join on Y
                z = other.get('arg1')
                yield FrozenDict.from_dict({'arg0': x, 'arg1': z})
```

### Negation (Stratified)

```prolog
safe_path(X, Y) :- edge(X, Y), \+ blocked(X, Y).
```

Generated Python:

```python
def _apply_rule_negation(fact: FrozenDict, total: Set[FrozenDict]) -> Iterator[FrozenDict]:
    x = fact.get('arg0')
    y = fact.get('arg1')

    # Check negation
    blocked_fact = FrozenDict.from_dict({'relation': 'blocked', 'arg0': x, 'arg1': y})
    if blocked_fact in total:
        return  # Negation fails

    yield FrozenDict.from_dict({'arg0': x, 'arg1': y})
```

**Stratification**: Negated predicates must not depend on the predicate being defined.

---

## Supported Patterns

### Transitive Closure

```prolog
path(X, Y) :- edge(X, Y).
path(X, Z) :- edge(X, Y), path(Y, Z).
```

Computes all reachable pairs in a graph.

### N-Way Joins

```prolog
triple_path(X, W) :- edge(X, Y), edge(Y, Z), edge(Z, W).
```

Multiple goals joined sequentially.

### Disjunction

```prolog
connected(X, Y) :- edge(X, Y).
connected(X, Y) :- edge(Y, X).  % Symmetric
```

Both clauses applied during fixpoint.

---

## Input/Output Format

### Input (JSONL)

```json
{"arg0": "a", "arg1": "b"}
{"arg0": "b", "arg1": "c"}
{"arg0": "c", "arg1": "d"}
```

### Command

```bash
cat edges.jsonl | python3 path.py
```

### Output

```json
{"arg0": "a", "arg1": "b"}
{"arg0": "b", "arg1": "c"}
{"arg0": "c", "arg1": "d"}
{"arg0": "a", "arg1": "c"}
{"arg0": "b", "arg1": "d"}
{"arg0": "a", "arg1": "d"}
```

The last three lines are derived facts from transitive closure.

---

## Performance Characteristics

### Time Complexity

| Operation | Complexity |
|-----------|------------|
| FrozenDict hash | O(n) where n = number of fields |
| Set membership | O(1) average |
| Binary join | O(|delta| × |total|) per iteration |
| Fixpoint | O(iterations × join cost) |

### Space Complexity

- `total` set: O(number of facts)
- `delta` set: O(new facts per iteration)
- FrozenDict overhead: ~2x raw dict size

### Optimization Tips

1. **Minimize fact size** - Fewer fields = faster hashing
2. **Filter early** - Pre-filter input with `jq` or similar
3. **Use NUL-delimited JSON** - `record_format(nul_json)` for large inputs

---

## Why Semi-Naive?

### Naive Approach

Apply rules to ALL facts in `total` each iteration:
```python
for fact in total:  # All facts
    apply_rules(fact, total)
```

**Problem**: Redundant computation. Facts processed multiple times.

### Semi-Naive Approach

Apply rules only to NEW facts (delta):
```python
for fact in delta:  # Only new facts
    apply_rules(fact, total)
```

**Benefit**: Each fact processed at most once per rule.

---

## Comparison with Procedural Mode

| Feature | Procedural | Generator |
|---------|------------|-----------|
| Recursion | Python call stack | Iteration |
| Depth limit | ~1000 (Python default) | Unlimited |
| Memory | Stack frames | Sets |
| Deduplication | Manual | Automatic |
| Use case | Simple transforms | Recursive queries |

---

## Debugging

### Print Iteration Progress

```python
iteration = 0
while delta:
    iteration += 1
    print(f"Iteration {iteration}: {len(delta)} new facts", file=sys.stderr)
    # ...
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
    # ...
```

---

## Related Documentation

- [Book 5 Chapter 2: Procedural Mode](../02_procedural_mode.md)
- [Book 5 Chapter 4: Recursion Patterns](../04_recursion_patterns.md)
- [Python Target Source](../../../../src/unifyweaver/targets/python_target.pl)
