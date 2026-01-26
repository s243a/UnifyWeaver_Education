<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: Generator Mode - Questions

**Q&A companion to [03_generator_mode_impl.md](./03_generator_mode_impl.md)**

---

<a id="b05c03-q-semi-naive"></a>
## Q: What is semi-naive evaluation?

An algorithm that computes recursive queries by iterating to a fixpoint:

1. Initialize `total` and `delta` with input facts
2. Apply rules only to facts in `delta` (new facts)
3. Add derived facts to `new_delta`
4. Repeat until `delta` is empty (fixpoint)

**Why "semi-naive"?** Naive approach applies rules to ALL facts. Semi-naive only processes new facts, avoiding redundant computation.

**Reference**: [Semi-Naive Evaluation Algorithm](./03_generator_mode_impl.md#semi-naive-evaluation-algorithm)

---

<a id="b05c03-q-total-vs-delta"></a>
## Q: What's the difference between `total` and `delta` sets?

| Set | Contains | Purpose |
|-----|----------|---------|
| `total` | All facts discovered | Deduplication check |
| `delta` | Facts from current iteration | Work queue |

Only facts in `delta` are processed each iteration.

**Reference**: [Semi-Naive Evaluation Algorithm](./03_generator_mode_impl.md#semi-naive-evaluation-algorithm)

---

<a id="b05c03-q-what-is-fixpoint"></a>
## Q: What is a fixpoint?

The state where no new facts can be derived. When `delta` becomes empty, iteration stops.

**Guaranteed termination** for Datalog queries (finite number of possible facts).

**Reference**: [Semi-Naive Evaluation Algorithm](./03_generator_mode_impl.md#semi-naive-evaluation-algorithm)

---

<a id="b05c03-q-why-frozendict"></a>
## Q: Why use FrozenDict instead of regular dict?

Python's `dict` is mutable and unhashable, so can't be stored in sets.

`FrozenDict` provides:
- Immutability (can't be modified)
- Hashability (works in sets)
- O(1) membership testing

**Reference**: [FrozenDict Data Structure](./03_generator_mode_impl.md#frozendict-data-structure)

---

<a id="b05c03-q-frozendict-usage"></a>
## Q: How do I use FrozenDict?

```python
# Create from dict
fd = FrozenDict.from_dict({'arg0': 'a', 'arg1': 'b'})

# Use in set
facts.add(fd)

# Check membership (O(1))
print(fd2 in facts)

# Convert back to dict
d = fd.to_dict()
```

**Reference**: [FrozenDict Data Structure](./03_generator_mode_impl.md#frozendict-data-structure)

---

<a id="b05c03-q-copy-rule"></a>
## Q: What is a copy rule?

A rule that copies facts from one predicate to another:

```prolog
path(X, Y) :- edge(X, Y).
```

Input facts matching `edge(X, Y)` are copied to `path(X, Y)`.

**Reference**: [Copy Rules](./03_generator_mode_impl.md#copy-rules-fact-projection)

---

<a id="b05c03-q-binary-join"></a>
## Q: How do binary joins work in generator mode?

```prolog
path(X, Z) :- edge(X, Y), path(Y, Z).
```

For each fact in `delta`, look for matching facts in `total`:

```python
for other in total:
    if other.get('arg0') == y:  # Join on Y
        yield new_fact
```

**Reference**: [Binary Joins](./03_generator_mode_impl.md#binary-joins)

---

<a id="b05c03-q-negation"></a>
## Q: How does negation work in generator mode?

Stratified negation-as-failure: Check if negated fact exists in `total`.

```python
blocked_fact = FrozenDict.from_dict({'relation': 'blocked', 'arg0': x, 'arg1': y})
if blocked_fact in total:
    return  # Negation fails
```

**Requirement**: Negated predicates must not depend on the predicate being defined.

**Reference**: [Negation Support](./03_generator_mode_impl.md#negation-support)

---

<a id="b05c03-q-stratification"></a>
## Q: What is stratification for negation?

A requirement that negated predicates are fully computed before the negating predicate.

```prolog
% VALID: blocked doesn't depend on safe_path
safe_path(X, Y) :- edge(X, Y), \+ blocked(X, Y).

% INVALID: self-referential negation
bad(X) :- foo(X), \+ bad(X).  % ERROR
```

The compiler validates this at compile time.

**Reference**: [Stratification Validation](./03_generator_mode_impl.md#stratification-validation)

---

<a id="b05c03-q-performance"></a>
## Q: What's the time complexity of generator mode?

| Operation | Complexity |
|-----------|------------|
| Set membership | O(1) average |
| Binary join | O(n × m) |
| N-way join | O(n^k) |
| Overall fixpoint | O(n²) worst case |

**Reference**: [Performance Characteristics](./03_generator_mode_impl.md#performance-characteristics)

---

<a id="b05c03-q-when-generator"></a>
## Q: When should I use generator mode?

- **Transitive closure** (all paths in graph)
- **Deep recursion** (avoid Python's ~1000 recursion limit)
- **Complex negation patterns**
- **Python ecosystem integration**

**Reference**: [Chapter 3: Generator Mode](../03_generator_mode.md#when-to-use-generator-mode)

---

<a id="b05c03-q-vs-bash"></a>
## Q: How does Python generator compare to Bash BFS?

| Aspect | Python Generator | Bash BFS |
|--------|------------------|----------|
| Algorithm | Semi-naive fixpoint | BFS queue |
| Memory | O(all facts) | O(frontier) |
| Ecosystem | Python tools | Unix pipelines |

Choose based on deployment environment.

**Reference**: [Comparison: Python Generator vs Bash BFS](./03_generator_mode_impl.md#comparison-python-generator-vs-bash-bfs)

---

<a id="b05c03-q-debug-iteration"></a>
## Q: How do I debug generator mode iteration?

```python
iteration = 0
while delta:
    iteration += 1
    print(f"Iteration {iteration}: {len(delta)} new facts", file=sys.stderr)
```

**Reference**: [Debugging](./03_generator_mode_impl.md#debugging)

---

<a id="b05c03-q-optimize-input"></a>
## Q: How can I optimize generator mode performance?

1. **Minimize fact size** (smaller = faster hashing)
2. **Filter early** (pre-filter before fixpoint)
3. **Use NUL-delimited format** for large inputs

```bash
jq 'select(.weight > 0)' | python3 path.py
```

**Reference**: [Optimization Tips](./03_generator_mode_impl.md#optimization-tips)

---

## Question Index

| ID | Topic |
|----|-------|
| [b05c03-q-semi-naive](#b05c03-q-semi-naive) | Semi-naive evaluation |
| [b05c03-q-total-vs-delta](#b05c03-q-total-vs-delta) | total vs delta |
| [b05c03-q-what-is-fixpoint](#b05c03-q-what-is-fixpoint) | Fixpoint definition |
| [b05c03-q-why-frozendict](#b05c03-q-why-frozendict) | Why FrozenDict |
| [b05c03-q-frozendict-usage](#b05c03-q-frozendict-usage) | FrozenDict usage |
| [b05c03-q-copy-rule](#b05c03-q-copy-rule) | Copy rules |
| [b05c03-q-binary-join](#b05c03-q-binary-join) | Binary joins |
| [b05c03-q-negation](#b05c03-q-negation) | Negation support |
| [b05c03-q-stratification](#b05c03-q-stratification) | Stratification |
| [b05c03-q-performance](#b05c03-q-performance) | Performance |
| [b05c03-q-when-generator](#b05c03-q-when-generator) | When to use |
| [b05c03-q-vs-bash](#b05c03-q-vs-bash) | vs Bash BFS |
| [b05c03-q-debug-iteration](#b05c03-q-debug-iteration) | Debugging |
| [b05c03-q-optimize-input](#b05c03-q-optimize-input) | Optimization |
