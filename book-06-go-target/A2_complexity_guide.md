<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Appendix B: Complexity & Performance Guide

This appendix documents the time and space complexity of Go target compilation modes and provides guidance on choosing the right approach.

## Compilation Mode Complexity

### Streaming Mode

| Operation | Time | Space | Notes |
|-----------|------|-------|-------|
| Simple transform | O(n) | O(1) | Single-pass stdin→stdout |
| Regex matching | O(n × m) | O(1) | n = input size, m = pattern size |
| JSON parsing | O(n) | O(d) | d = max nesting depth |
| Field extraction | O(1) | O(1) | Direct map access |

**Best for:** ETL pipelines, log processing, simple record transformations.

### Generator Mode (Fixpoint Datalog)

| Operation | Time | Space | Notes |
|-----------|------|-------|-------|
| Fixpoint iteration | O(n × k) | O(n) | n = facts, k = iterations to fixpoint |
| Indexed join | O(1) per lookup | O(n) | Hash-based index |
| Unindexed join | O(n²) | O(n) | Avoid if possible |
| Aggregation | O(n) | O(g) | g = number of groups |
| Stratified negation | O(n) | O(n) | Per stratum |

**Best for:** Recursive queries, complex joins, aggregation, negation.

### Recursion Patterns

| Pattern | Time | Space | Generated Code |
|---------|------|-------|----------------|
| BFS transitive closure | O(V + E) | O(V) | Queue + visited set |
| Tail recursion | O(n) | O(1) | Iterative for-loop |
| Linear recursion (memoized) | O(n) | O(n) | Map-based cache |
| Mutual recursion | O(n) | O(n) | Shared memo map |

## Database Query Complexity

### BoltDB Key Strategies

| Strategy | Lookup | Range Scan | Full Scan |
|----------|--------|------------|-----------|
| Single field key | O(log n) | N/A | O(n) |
| Composite key | O(log n) | O(k log n) | O(n) |
| Hash key | O(log n) | N/A | O(n) |

Where n = total records, k = records matching prefix.

### Query Optimization

The compiler automatically selects the best strategy:

| Query Pattern | Strategy | Complexity |
|---------------|----------|------------|
| `key = value` | Direct lookup | O(log n) |
| `key_prefix = value` | Prefix scan | O(k log n) |
| `field > value` (non-key) | Full scan + filter | O(n) |
| No constraints | Full scan | O(n) |

## Choosing the Right Mode

### Use Streaming Mode When:

- ✅ Single-pass processing is sufficient
- ✅ No recursion needed
- ✅ Simple field transformations
- ✅ Memory is constrained (O(1) space)
- ✅ High throughput is priority

### Use Generator Mode When:

- ✅ Recursive queries (transitive closure)
- ✅ Multiple relations need joining
- ✅ Aggregation (count, sum, avg, min, max)
- ✅ Negation-as-failure
- ✅ Incremental computation with persistence

### Use BoltDB Persistence When:

- ✅ Data persists across runs
- ✅ Incremental updates to facts
- ✅ Query optimization on key fields
- ✅ Data too large for memory

## Parallel Execution

Generator mode supports parallel execution via `workers(N)`:

| Workers | Speedup | Overhead | Best For |
|---------|---------|----------|----------|
| 1 | 1x | None | Small datasets |
| 2-4 | 1.5-3x | Channel sync | Medium datasets |
| N (CPU cores) | ~Nx | Goroutine scheduling | Large datasets |

**Tip:** Diminishing returns beyond CPU core count. Profile to find optimal N.

## Memory Considerations

### Streaming Mode
- O(1) working memory
- Buffers: `bufio.Scanner` default 64KB

### Generator Mode
- O(n) for fact storage in `map[string]Fact`
- O(n) for indexes
- O(g) for aggregation groups

### BoltDB
- Memory-mapped file access
- Page cache managed by OS
- Explicit `db.Close()` required

## Performance Tips

1. **Index key fields**: Use `db_key_field` or `db_key_strategy` for query predicates
2. **Composite keys**: Order fields by selectivity (most selective first)
3. **Avoid full scans**: Ensure WHERE clauses match key structure
4. **Batch writes**: Use `db.Update()` batching for bulk inserts
5. **Parallel workers**: Match to CPU cores for fixpoint computation
6. **Memoization**: Use `compile_linear_recursion_go/3` for overlapping subproblems

## Complexity Summary Table

| Mode | Time | Space | Use Case |
|------|------|-------|----------|
| Streaming | O(n) | O(1) | Simple ETL |
| Generator (no recursion) | O(n × j) | O(n) | Joins, aggregation |
| Generator (recursive) | O(n × k) | O(n) | Transitive closure |
| BFS recursive | O(V + E) | O(V) | Graph queries |
| Tail recursive | O(n) | O(1) | Accumulator patterns |
| Memoized recursive | O(n) | O(n) | Fibonacci-like |

Where:
- n = number of input records/facts
- j = number of join operations
- k = iterations to fixpoint
- V = vertices, E = edges in graph

---

## Navigation

**←** [Appendix A: API Reference](A1_api_reference) | [📖 Book 6: Go Target](./) | [Appendix C: Database Persistence →](A3_database_persistence)
