<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: Constraint System - Questions

**Q&A companion to [03_constraints_impl.md](./03_constraints_impl.md)**

---

<a id="b02c03-q-constraint-dimensions"></a>
## Q: What are the two constraint dimensions?

| Constraint | Values | Default | Effect |
|------------|--------|---------|--------|
| `unique` | `true`, `false` | `true` | Deduplicate results |
| `unordered` | `true`, `false` | `true` | Allow reordering |

They are orthogonal (independent).

**Reference**: [Constraint Overview](./03_constraints_impl.md#constraint-overview)

---

<a id="b02c03-q-constraint-shorthand"></a>
## Q: What are the constraint shorthands?

- `unique` → `unique(true)`
- `unordered` → `unordered(true)`
- `ordered` → `unordered(false)`

**Reference**: [Constraint Overview](./03_constraints_impl.md#constraint-overview)

---

<a id="b02c03-q-declare-constraint"></a>
## Q: How do I declare constraints for a predicate?

```prolog
:- constraint(grandparent/2, [unique, unordered]).
:- constraint(event_log/3, [unique, ordered]).
:- constraint(raw_log/3, [unique(false), ordered]).
```

**Reference**: [Constraint Pragma](./03_constraints_impl.md#constraint-pragma)

---

<a id="b02c03-q-sort-u-strategy"></a>
## Q: When is sort -u used for deduplication?

When `unique(true)` AND `unordered(true)` (the default):

```bash
predicate() {
    predicate_all "$@" | sort -u
}
```

Efficient but changes order.

**Reference**: [Strategy: sort_u](./03_constraints_impl.md#strategy-sort_u)

---

<a id="b02c03-q-hash-dedup-strategy"></a>
## Q: When is hash deduplication used?

When `unique(true)` AND `unordered(false)`:

```bash
declare -A seen
predicate_all "$@" | while IFS= read -r line; do
    if [[ -z "${seen[$line]}" ]]; then
        seen[$line]=1
        echo "$line"
    fi
done
```

Preserves order, streams output.

**Reference**: [Strategy: hash_dedup](./03_constraints_impl.md#strategy-hash_dedup)

---

<a id="b02c03-q-no-dedup"></a>
## Q: How do I disable deduplication?

Set `unique(false)`:

```prolog
:- constraint(word_occurrences/2, [unique(false)]).
```

Generated code has no deduplication overhead.

**Reference**: [Strategy: no_dedup](./03_constraints_impl.md#strategy-no_dedup)

---

<a id="b02c03-q-runtime-override"></a>
## Q: Can I override constraints at compile time?

Yes, pass options to compile_predicate:

```prolog
% Override declared constraint
?- compile_predicate(user_action/3, [unique(false)], Code).
```

Runtime options override declared constraints.

**Reference**: [Runtime Overrides](./03_constraints_impl.md#runtime-overrides)

---

<a id="b02c03-q-early-exit"></a>
## Q: What is early exit optimization?

For `unique(true)` with single-result predicates:

```bash
count_items() {
    echo "$final_value"
    exit 0  # Exit immediately after single result
}
```

Prevents unnecessary downstream work.

**Reference**: [Constraint-Aware Optimization](./03_constraints_impl.md#constraint-aware-optimization)

---

<a id="b02c03-q-use-case-set"></a>
## Q: When should I use [unique, unordered]?

For set-like queries where order doesn't matter:
- Finding all ancestors
- Finding all dependencies
- Graph reachability

This is the default.

**Reference**: [Use Cases](./03_constraints_impl.md#use-cases)

---

<a id="b02c03-q-use-case-timeline"></a>
## Q: When should I use [unique, ordered]?

For ordered timelines with first-occurrence deduplication:
- Event sequences
- Log processing (unique events)
- Chronological data

**Reference**: [Use Cases](./03_constraints_impl.md#use-cases)

---

<a id="b02c03-q-use-case-counting"></a>
## Q: When should I use [unique(false)]?

For counting or aggregation where duplicates matter:
- Word frequency counting
- Multiple occurrences of events
- Aggregation queries

**Reference**: [Use Cases](./03_constraints_impl.md#use-cases)

---

<a id="b02c03-q-performance-comparison"></a>
## Q: How do the dedup strategies compare?

| Strategy | Time | Memory | Order |
|----------|------|--------|-------|
| sort_u | O(n log n) | O(n) | Changed |
| hash_dedup | O(n) | O(unique) | Preserved |
| no_dedup | O(n) | O(1) | Preserved |

**Reference**: [Performance Comparison](./03_constraints_impl.md#performance-comparison)

---

## Question Index

| ID | Topic |
|----|-------|
| [b02c03-q-constraint-dimensions](#b02c03-q-constraint-dimensions) | Constraint dimensions |
| [b02c03-q-constraint-shorthand](#b02c03-q-constraint-shorthand) | Shorthand forms |
| [b02c03-q-declare-constraint](#b02c03-q-declare-constraint) | Declaring constraints |
| [b02c03-q-sort-u-strategy](#b02c03-q-sort-u-strategy) | sort -u strategy |
| [b02c03-q-hash-dedup-strategy](#b02c03-q-hash-dedup-strategy) | Hash dedup strategy |
| [b02c03-q-no-dedup](#b02c03-q-no-dedup) | Disabling dedup |
| [b02c03-q-runtime-override](#b02c03-q-runtime-override) | Runtime overrides |
| [b02c03-q-early-exit](#b02c03-q-early-exit) | Early exit optimization |
| [b02c03-q-use-case-set](#b02c03-q-use-case-set) | Set queries |
| [b02c03-q-use-case-timeline](#b02c03-q-use-case-timeline) | Ordered timeline |
| [b02c03-q-use-case-counting](#b02c03-q-use-case-counting) | Counting/aggregation |
| [b02c03-q-performance-comparison](#b02c03-q-performance-comparison) | Performance comparison |
