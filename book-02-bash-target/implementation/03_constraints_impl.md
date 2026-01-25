<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3 Implementation: The Constraint System

**Detailed function documentation for RAG systems**

This document provides implementation details for the constraint system that controls deduplication and ordering in generated Bash code.

---

## Table of Contents

1. [Constraint Overview](#constraint-overview)
2. [Constraint Pragma](#constraint-pragma)
3. [Deduplication Strategies](#deduplication-strategies)
4. [Runtime Overrides](#runtime-overrides)
5. [Constraint-Aware Optimization](#constraint-aware-optimization)

---

## Constraint Overview

Two orthogonal constraint dimensions:

| Constraint | Values | Default | Effect |
|------------|--------|---------|--------|
| `unique` | `true`, `false` | `true` | Deduplicate results |
| `unordered` | `true`, `false` | `true` | Allow reordering |

**Shorthand forms**:
- `unique` → `unique(true)`
- `unordered` → `unordered(true)`
- `ordered` → `unordered(false)`

### Constraint Combinations

| unique | unordered | Use Case | Bash Strategy |
|--------|-----------|----------|---------------|
| `true` | `true` | Set queries (ancestors) | `sort -u` |
| `true` | `false` | Ordered timeline | Hash dedup |
| `false` | `true` | Aggregation | No dedup |
| `false` | `false` | Raw logs | Direct output |

---

## Constraint Pragma

### Syntax

```prolog
:- constraint(Predicate/Arity, [ConstraintList]).
```

### Examples

```prolog
% Explicit defaults
:- constraint(grandparent/2, [unique, unordered]).

% Preserve order, deduplicate
:- constraint(event_log/3, [unique, ordered]).

% Allow duplicates
:- constraint(all_mentions/2, [unique(false), unordered]).

% Raw output (no processing)
:- constraint(raw_log/3, [unique(false), ordered]).
```

### Implementation

The pragma is processed via `term_expansion/2`:

```prolog
user:term_expansion(
    (:- constraint(Pred, Constraints)),
    (:- initialization(constraint_analyzer:declare_constraint(Pred, Constraints)))
).
```

When the file loads, `declare_constraint/2` is called to register the constraints.

---

## Deduplication Strategies

### Strategy: sort_u

**When**: `unique(true), unordered(true)` (default)

**Generated Code**:
```bash
predicate() {
    predicate_all "$@" | sort -u
}
```

**Characteristics**:
- Efficient: `sort -u` is highly optimized
- Changes order: Results are sorted
- Buffers data: Must read all input before outputting

### Strategy: hash_dedup

**When**: `unique(true), unordered(false)`

**Generated Code**:
```bash
predicate() {
    declare -A seen
    predicate_all "$@" | while IFS= read -r line; do
        if [[ -z "${seen[$line]}" ]]; then
            seen[$line]=1
            echo "$line"
        fi
    done
}
```

**Characteristics**:
- Preserves order: First occurrence wins
- Streaming: Outputs as it reads
- Memory: O(unique results) for hashtable

### Strategy: no_dedup

**When**: `unique(false)`

**Generated Code**:
```bash
predicate() {
    predicate_all "$@"
}
```

**Characteristics**:
- Fastest: No processing overhead
- May have duplicates
- True streaming

---

## Runtime Overrides

Override declared constraints at compile time:

```prolog
% Declared constraint
:- constraint(user_action/3, [unique, ordered]).

% Override to get duplicates
?- compile_predicate(user_action/3, [unique(false)], Code).

% Override to allow reordering
?- compile_predicate(user_action/3, [unordered(true)], Code).
```

**Merge behavior**: Runtime options override declared constraints, which override defaults.

---

## Constraint-Aware Optimization

### Early Exit Optimization

For `unique(true)` with single-result predicates:

**Without optimization**:
```bash
count_items() {
    # ... loop logic ...
    echo "$final_value"
}
```

**With optimization** (knowing unique result):
```bash
count_items() {
    # ... loop logic ...
    echo "$final_value"
    exit 0
}
```

The `exit 0` terminates immediately, preventing downstream work.

### Pipeline Short-Circuit

When checking a specific relationship with `unique(true)`:

```bash
ancestor_check() {
    local start="$1"
    local target="$2"
    # With unique(true), can exit on first match
    ancestor_all "$start" | grep -q "^$start:$target$"
}
```

The `grep -q` exits on first match, SIGPIPE terminates `ancestor_all`.

---

## Constraint Query API

### get_constraints/2

```prolog
get_constraints(+Pred, -Constraints)
```

Get effective constraints for a predicate:

```prolog
?- get_constraints(ancestor/2, C).
C = [unique(true), unordered(true)].
```

### get_dedup_strategy/2

```prolog
get_dedup_strategy(+Constraints, -Strategy)
```

Get deduplication strategy name:

```prolog
?- get_dedup_strategy([unique(true), unordered(true)], S).
S = sort_u.

?- get_dedup_strategy([unique(true), unordered(false)], S).
S = hash_dedup.

?- get_dedup_strategy([unique(false)], S).
S = no_dedup.
```

### constraint_implies_sort_u/1

```prolog
constraint_implies_sort_u(+Constraints)
```

Test if constraints allow `sort -u`:

```prolog
?- constraint_implies_sort_u([unique(true), unordered(true)]).
true.

?- constraint_implies_sort_u([unique(true), unordered(false)]).
false.
```

### constraint_implies_hash/1

```prolog
constraint_implies_hash(+Constraints)
```

Test if constraints require hash deduplication:

```prolog
?- constraint_implies_hash([unique(true), unordered(false)]).
true.
```

---

## Use Cases

### Set Queries (Default)

Finding all ancestors, dependencies, etc.

```prolog
:- constraint(ancestor/2, [unique, unordered]).
```

Order doesn't matter, duplicates are noise.

### Ordered Timeline

First occurrence of each unique event.

```prolog
:- constraint(event_timeline/3, [unique, ordered]).
```

Order preserved, duplicates removed.

### Counting/Aggregation

Need to count duplicates.

```prolog
:- constraint(word_occurrences/2, [unique(false)]).
```

Duplicates needed for counting.

### Raw Log Processing

Every line matters, including duplicates and order.

```prolog
:- constraint(raw_access_log/3, [unique(false), ordered]).
```

No processing, direct passthrough.

---

## Performance Comparison

| Strategy | Time Complexity | Memory | Order Preserved |
|----------|----------------|--------|-----------------|
| `sort_u` | O(n log n) | O(n) | No |
| `hash_dedup` | O(n) | O(unique) | Yes |
| `no_dedup` | O(n) | O(1) | Yes |

**Recommendation**: Use defaults unless you have specific requirements.

---

## Source Files

- `src/unifyweaver/core/constraint_analyzer.pl`
- `src/unifyweaver/core/template_system.pl` (dedup templates)

## See Also

- Chapter 3: Advanced Constraints (tutorial)
- Book 1, Chapter 3: Architecture (constraint analyzer details)
- Chapter 6: Advanced Recursion (constraint-aware optimization)
