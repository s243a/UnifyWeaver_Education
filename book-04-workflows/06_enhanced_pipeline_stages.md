<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 6: Enhanced Pipeline Stages

This chapter covers UnifyWeaver's **enhanced pipeline stages** - powerful building blocks for complex data processing workflows within a single target.

## Overview

While Chapter 3 covered cross-target pipeline orchestration (AWK → Python → Go), this chapter focuses on **within-target** pipeline composition using enhanced stages.

Enhanced pipeline stages let you:
- Combine multiple data streams (`interleave`, `concat`, `zip`)
- Control flow (`fan_out`, `parallel`, `merge`, `route_by`)
- Filter and transform (`filter_by`, `batch`, `unbatch`)
- Aggregate data (`group_by`, `reduce`, `unique`)
- Handle errors (`try_catch`, `retry`, `timeout`)
- Control throughput (`rate_limit`, `throttle`, `buffer`)

## Stream Combination Stages

### Interleave: Fair Merging

The `interleave(Stages)` stage alternates records from multiple stage outputs in round-robin fashion:

```prolog
compile_enhanced_pipeline([
    parse/1,
    interleave([
        filter_by(is_priority),    % High priority items
        filter_by(is_standard)     % Standard items
    ]),
    process/1,
    output/1
], [pipeline_name(balanced_processing)], Code).
```

**How It Works:**

```
Stage A outputs: [A1, A2, A3]
Stage B outputs: [B1, B2]
                    ↓
interleave([A, B]) → [A1, B1, A2, B2, A3]
```

**Use Cases:**
- Merging multiple data sources with fair ordering
- Load balancing across different processing paths
- Combining results from filters while maintaining balance

### Concat: Sequential Combination

The `concat(Stages)` stage concatenates multiple stage outputs sequentially:

```prolog
compile_enhanced_pipeline([
    parse/1,
    concat([
        filter_by(is_active),      % Active records first
        filter_by(is_archived)     % Then archived records
    ]),
    distinct,                      % Remove any duplicates
    output/1
], [pipeline_name(all_records)], Code).
```

**How It Works:**

```
Stage A outputs: [A1, A2]
Stage B outputs: [B1, B2, B3]
                    ↓
concat([A, B]) → [A1, A2, B1, B2, B3]
```

**Use Cases:**
- Combining results from different transformations
- Union of filtered subsets
- Appending fallback results to primary results

### Interleave vs Concat

| Aspect | `interleave` | `concat` |
|--------|--------------|----------|
| Order | Round-robin alternating | Sequential (all of A, then all of B) |
| Fairness | Balanced across sources | First source gets priority |
| Memory | Holds iterators for all streams | Processes one stream at a time |
| Use case | Fair merging | Union/append |

### Zip: Parallel Enrichment

The `zip(Stages)` stage runs multiple stages on the same input and combines outputs:

```prolog
compile_enhanced_pipeline([
    parse/1,
    zip([
        geocode/1,           % Add location data
        sentiment_analyze/1, % Add sentiment score
        categorize/1         % Add category
    ]),
    output/1
], [pipeline_name(multi_enrich)], Code).
```

## Flow Control Stages

### Fan-out and Merge

Broadcast records to multiple stages and combine results:

```prolog
compile_enhanced_pipeline([
    parse/1,
    fan_out([validate/1, enrich/1, audit/1]),
    merge,
    output/1
], [pipeline_name(broadcast_pipeline)], Code).
```

### Parallel Execution

Execute stages concurrently for performance:

```prolog
compile_enhanced_pipeline([
    parse/1,
    parallel([
        slow_transform_a/1,
        slow_transform_b/1
    ]),
    merge,
    output/1
], [pipeline_name(concurrent_pipeline)], Code).
```

### Conditional Routing

Route records to different stages based on conditions:

```prolog
compile_enhanced_pipeline([
    parse/1,
    route_by(get_type, [
        (error, error_handler/1),
        (warning, warning_handler/1),
        (info, info_handler/1)
    ]),
    output/1
], [pipeline_name(routed_pipeline)], Code).
```

## Filtering and Transformation

### Filter By

Filter records using a predicate:

```prolog
compile_enhanced_pipeline([
    parse/1,
    filter_by(is_valid),
    filter_by(has_required_fields),
    output/1
], [pipeline_name(filtered_pipeline)], Code).
```

### Batch and Unbatch

Collect records into batches for bulk processing:

```prolog
compile_enhanced_pipeline([
    parse/1,
    batch(100),           % Collect 100 records
    bulk_insert/1,        % Process as batch
    unbatch,              % Flatten back
    output/1
], [pipeline_name(batched_pipeline)], Code).
```

## Aggregation Stages

### Group By

Group records and apply aggregations:

```prolog
compile_enhanced_pipeline([
    parse/1,
    group_by(category, [count, sum(amount), avg(price)]),
    output/1
], [pipeline_name(grouped_pipeline)], Code).
```

### Unique/Distinct

Remove duplicates:

```prolog
compile_enhanced_pipeline([
    parse/1,
    distinct,                    % Remove all duplicates
    % or
    distinct_by(user_id),        % Unique by field
    output/1
], [pipeline_name(deduped_pipeline)], Code).
```

## Error Handling Stages

### Try-Catch

Handle errors gracefully:

```prolog
compile_enhanced_pipeline([
    parse/1,
    try_catch(risky_transform/1, error_fallback/1),
    output/1
], [pipeline_name(safe_pipeline)], Code).
```

### Retry with Backoff

Retry failed operations:

```prolog
compile_enhanced_pipeline([
    parse/1,
    retry(call_external_api/1, 3, [delay(1000), backoff(exponential)]),
    output/1
], [pipeline_name(resilient_pipeline)], Code).
```

### Timeout

Set time limits:

```prolog
compile_enhanced_pipeline([
    parse/1,
    timeout(slow_operation/1, 5000, use_cached/1),
    output/1
], [pipeline_name(bounded_pipeline)], Code).
```

## Throughput Control

### Rate Limiting

Control processing rate:

```prolog
compile_enhanced_pipeline([
    parse/1,
    rate_limit(100, second),     % 100 records per second
    call_api/1,
    output/1
], [pipeline_name(rate_limited_pipeline)], Code).
```

### Buffer and Debounce

Control emission timing:

```prolog
compile_enhanced_pipeline([
    parse/1,
    buffer(10),           % Collect 10 records
    debounce(100),        % Wait 100ms quiet period
    output/1
], [pipeline_name(buffered_pipeline)], Code).
```

## Combining Stages

Enhanced stages compose naturally:

```prolog
compile_enhanced_pipeline([
    parse/1,

    % Combine multiple sources with fair ordering
    interleave([
        filter_by(is_priority),
        filter_by(is_standard)
    ]),

    % Remove duplicates
    distinct_by(id),

    % Process with error handling
    try_catch(
        retry(process/1, 3),
        log_error/1
    ),

    % Control throughput
    rate_limit(50, second),

    % Batch for output
    batch(100),
    bulk_output/1
], [pipeline_name(production_pipeline)], Code).
```

## Supported Targets

Enhanced pipeline stages are available for:

| Target | Entry Point |
|--------|-------------|
| Python | `compile_enhanced_pipeline/3` |
| Go | `compile_go_enhanced_pipeline/3` |
| Rust | `compile_rust_enhanced_pipeline/3` |
| C# | `compile_csharp_enhanced_pipeline/3` |
| PowerShell | `compile_powershell_enhanced_pipeline/3` |
| AWK | `compile_awk_enhanced_pipeline/3` |
| Bash | `compile_bash_enhanced_pipeline/3` |
| IronPython | `compile_ironpython_enhanced_pipeline/3` |

## Stage Reference

| Stage | Description |
|-------|-------------|
| `interleave(Stages)` | Round-robin alternate from multiple outputs |
| `concat(Stages)` | Sequential concatenation |
| `zip(Stages)` | Run on same input, combine outputs |
| `fan_out(Stages)` | Broadcast to multiple stages |
| `parallel(Stages)` | Concurrent execution |
| `merge` | Combine fan_out/parallel results |
| `route_by(Pred, Routes)` | Conditional routing |
| `filter_by(Pred)` | Filter by predicate |
| `batch(N)` | Collect N records |
| `unbatch` | Flatten batches |
| `group_by(Field, Aggs)` | Group and aggregate |
| `distinct` / `distinct_by(F)` | Remove duplicates |
| `dedup` / `dedup_by(F)` | Remove consecutive duplicates |
| `order_by(Field)` | Sort by field |
| `try_catch(Stage, Handler)` | Error handling |
| `retry(Stage, N, Opts)` | Retry on failure |
| `timeout(Stage, Ms)` | Time limit |
| `rate_limit(N, Per)` | Throughput control |
| `throttle(Ms)` | Fixed delay |
| `buffer(N)` | Collect into batches |
| `debounce(Ms)` | Quiet period emission |

## Summary

Enhanced pipeline stages enable sophisticated data processing within a single target:

- **Stream combination**: `interleave`, `concat`, `zip`
- **Flow control**: `fan_out`, `parallel`, `merge`, `route_by`
- **Filtering**: `filter_by`, `distinct`, `dedup`
- **Aggregation**: `group_by`, `reduce`, `unique`
- **Error handling**: `try_catch`, `retry`, `timeout`
- **Throughput**: `rate_limit`, `throttle`, `buffer`

For cross-target pipelines (AWK → Python → Go), see [Chapter 3: Pipeline Orchestration](03_pipeline_orchestration.md).

---

## Navigation

**←** [Previous: Chapter 5: Example Libraries](05_example_libraries) | [📖 Book 4: Workflows](./) | [Next: Book 5: Python Target →](../book-05-python-target/)
