<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: Generator Mode - Questions

Q&A companion for [03_generator_mode_impl.md](./03_generator_mode_impl.md).

---

## Question Index

1. [What is semi-naive fixpoint evaluation?](#b05c03-q-semi-naive)
2. [What is the difference between total and delta sets?](#b05c03-q-total-delta)
3. [What does compile_predicate_to_python do in generator mode?](#b05c03-q-compile-predicate)
4. [What is FrozenDict and why is it needed?](#b05c03-q-frozendict)
5. [How does process_stream_generator work?](#b05c03-q-process-stream)
6. [How are binary joins implemented?](#b05c03-q-binary-joins)
7. [How is negation handled in generator mode?](#b05c03-q-negation)
8. [What is stratification?](#b05c03-q-stratification)
9. [What is the time complexity of fixpoint evaluation?](#b05c03-q-complexity)
10. [Why is semi-naive better than naive evaluation?](#b05c03-q-why-semi-naive)
11. [When should I use generator mode vs procedural mode?](#b05c03-q-when-generator)
12. [How do I debug generator mode programs?](#b05c03-q-debugging)
13. [What input/output format does generator mode use?](#b05c03-q-io-format)
14. [How can I optimize generator mode performance?](#b05c03-q-optimization)

---

## Questions and Answers

### <a id="b05c03-q-semi-naive"></a>Q1: What is semi-naive fixpoint evaluation?

**Answer**: Semi-naive fixpoint evaluation is a Datalog technique that:
1. Maintains two sets: `total` (all facts) and `delta` (new facts)
2. Applies rules only to facts in `delta` (not all of `total`)
3. Iterates until `delta` is empty (fixpoint reached)

This avoids redundant computation by only processing new facts each iteration.

**See**: [Overview: Semi-Naive Fixpoint Evaluation](./03_generator_mode_impl.md#overview-semi-naive-fixpoint-evaluation)

---

### <a id="b05c03-q-total-delta"></a>Q2: What is the difference between total and delta sets?

**Answer**:

| Set | Contains | Purpose |
|-----|----------|---------|
| `total` | All facts discovered so far | Membership checking, join lookups |
| `delta` | Facts discovered in current iteration | Input for rule application |

At each iteration:
1. Rules are applied to facts in `delta`
2. New facts are added to `new_delta`
3. `new_delta` becomes the next `delta`
4. When `delta` is empty, fixpoint is reached

**See**: [process_stream_generator Function](./03_generator_mode_impl.md#process_stream_generator-function)

---

### <a id="b05c03-q-compile-predicate"></a>Q3: What does compile_predicate_to_python do in generator mode?

**Answer**: `compile_predicate_to_python/3` with `mode(generator)` generates Python code implementing semi-naive fixpoint evaluation:

```prolog
compile_predicate_to_python(path/2, [mode(generator)], Code).
```

The generated code includes:
- `FrozenDict` class for hashable facts
- `process_stream_generator` function for fixpoint loop
- `_apply_rule_*` functions for each clause

**See**: [compile_predicate_to_python/3](./03_generator_mode_impl.md#compile_predicate_to_python3)

---

### <a id="b05c03-q-frozendict"></a>Q4: What is FrozenDict and why is it needed?

**Answer**: FrozenDict is an immutable, hashable dictionary wrapper. It's needed because:
- Python's `dict` is mutable and unhashable
- We need to store facts in a `set` for O(1) membership testing
- FrozenDict provides dict-like access while being hashable

```python
fd = FrozenDict.from_dict({'arg0': 'alice', 'arg1': 'bob'})
facts = set()
facts.add(fd)  # Works because FrozenDict is hashable
print(fd.get('arg0'))  # 'alice'
```

**See**: [FrozenDict Class](./03_generator_mode_impl.md#frozendict-class)

---

### <a id="b05c03-q-process-stream"></a>Q5: How does process_stream_generator work?

**Answer**: The function has two phases:

**Phase 1: Initialization**
```python
for record in records:
    frozen = FrozenDict.from_dict(record)
    if frozen not in total:
        total.add(frozen)
        delta.add(frozen)
        yield record
```

**Phase 2: Fixpoint Iteration**
```python
while delta:
    new_delta = set()
    for fact in delta:
        for new_fact in _apply_rules(fact, total):
            if new_fact not in total:
                total.add(new_fact)
                new_delta.add(new_fact)
                yield new_fact.to_dict()
    delta = new_delta
```

**See**: [process_stream_generator Function](./03_generator_mode_impl.md#process_stream_generator-function)

---

### <a id="b05c03-q-binary-joins"></a>Q6: How are binary joins implemented?

**Answer**: For a rule like `path(X, Z) :- edge(X, Y), path(Y, Z)`:

```python
def _apply_rule_join(fact, total):
    x = fact.get('arg0')
    y = fact.get('arg1')
    for other in total:
        if other.get('arg0') == y:  # Join on Y
            z = other.get('arg1')
            yield FrozenDict.from_dict({'arg0': x, 'arg1': z})
```

The join iterates over `total` looking for facts where `arg0` matches the `arg1` of the current fact.

**See**: [Binary Joins](./03_generator_mode_impl.md#binary-joins)

---

### <a id="b05c03-q-negation"></a>Q7: How is negation handled in generator mode?

**Answer**: Negation-as-failure checks if a fact exists in `total`:

```prolog
safe_path(X, Y) :- edge(X, Y), \+ blocked(X, Y).
```

```python
def _apply_rule_negation(fact, total):
    blocked_fact = FrozenDict.from_dict({'relation': 'blocked', ...})
    if blocked_fact in total:
        return  # Negation fails
    yield new_fact
```

**See**: [Negation (Stratified)](./03_generator_mode_impl.md#negation-stratified)

---

### <a id="b05c03-q-stratification"></a>Q8: What is stratification?

**Answer**: Stratification is a restriction on negation: negated predicates must not depend (directly or indirectly) on the predicate being defined.

**Valid** (stratified):
```prolog
safe_path(X, Y) :- path(X, Y), \+ blocked(X, Y).
% blocked is independent of safe_path
```

**Invalid** (non-stratified):
```prolog
bad(X) :- good(X), \+ bad(X).
% bad depends on negation of itself
```

The compiler validates stratification at compile time.

**See**: [Negation (Stratified)](./03_generator_mode_impl.md#negation-stratified)

---

### <a id="b05c03-q-complexity"></a>Q9: What is the time complexity of fixpoint evaluation?

**Answer**:

| Operation | Complexity |
|-----------|------------|
| FrozenDict hash | O(n) where n = fields |
| Set membership | O(1) average |
| Binary join | O(|delta| × |total|) per iteration |
| Total fixpoint | O(iterations × join cost) |

For worst case with O(n²) total facts, this can be O(n⁴).

**See**: [Performance Characteristics](./03_generator_mode_impl.md#performance-characteristics)

---

### <a id="b05c03-q-why-semi-naive"></a>Q10: Why is semi-naive better than naive evaluation?

**Answer**:

**Naive**: Applies rules to ALL facts each iteration
```python
for fact in total:  # Redundant work
    apply_rules(fact, total)
```

**Semi-naive**: Applies rules only to NEW facts
```python
for fact in delta:  # Only new facts
    apply_rules(fact, total)
```

Semi-naive processes each fact at most once per rule, dramatically reducing redundant computation.

**See**: [Why Semi-Naive?](./03_generator_mode_impl.md#why-semi-naive)

---

### <a id="b05c03-q-when-generator"></a>Q11: When should I use generator mode vs procedural mode?

**Answer**:

| Use Generator When | Use Procedural When |
|--------------------|---------------------|
| Transitive closure | Simple transforms |
| Deep recursion (>1000) | Shallow recursion |
| Graph algorithms | Linear processing |
| Need automatic dedup | Manual control needed |

Generator mode has no recursion limit because it uses iteration instead of Python's call stack.

**See**: [Comparison with Procedural Mode](./03_generator_mode_impl.md#comparison-with-procedural-mode)

---

### <a id="b05c03-q-debugging"></a>Q12: How do I debug generator mode programs?

**Answer**: Add logging to the generated code:

```python
# Track iterations
iteration = 0
while delta:
    iteration += 1
    print(f"Iteration {iteration}: {len(delta)} new", file=sys.stderr)

# Inspect total set
for fact in total:
    print(f"  {fact.to_dict()}", file=sys.stderr)

# Trace rule application
def _apply_rule(fact, total):
    print(f"Applying to {fact.to_dict()}", file=sys.stderr)
```

**See**: [Debugging](./03_generator_mode_impl.md#debugging)

---

### <a id="b05c03-q-io-format"></a>Q13: What input/output format does generator mode use?

**Answer**: JSONL (JSON Lines) format:

**Input**:
```json
{"arg0": "a", "arg1": "b"}
{"arg0": "b", "arg1": "c"}
```

**Command**:
```bash
cat edges.jsonl | python3 path.py
```

**Output** (includes derived facts):
```json
{"arg0": "a", "arg1": "b"}
{"arg0": "b", "arg1": "c"}
{"arg0": "a", "arg1": "c"}
```

**See**: [Input/Output Format](./03_generator_mode_impl.md#inputoutput-format)

---

### <a id="b05c03-q-optimization"></a>Q14: How can I optimize generator mode performance?

**Answer**: Three main strategies:

1. **Minimize fact size** - Fewer fields = faster hashing
   ```python
   {"id": 123}  # Better
   {"id": 123, "meta": {...}}  # Slower
   ```

2. **Filter early** - Pre-filter before fixpoint
   ```bash
   jq 'select(.weight > 0)' | python3 path.py
   ```

3. **Use NUL-delimited JSON** - For large inputs
   ```prolog
   compile_predicate_to_python(path/2, [mode(generator), record_format(nul_json)], Code).
   ```

**See**: [Performance Characteristics](./03_generator_mode_impl.md#performance-characteristics)

---

## Summary

Generator mode provides:
- Semi-naive fixpoint evaluation
- FrozenDict for hashable facts
- No recursion depth limit
- Automatic deduplication
- Stratified negation support
