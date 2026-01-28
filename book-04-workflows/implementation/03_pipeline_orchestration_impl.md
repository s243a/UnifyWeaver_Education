<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: Pipeline Orchestration - Implementation Details

This document provides function-level documentation for multi-stage pipeline orchestration.

**Source**: `src/unifyweaver/workflows/pipeline.pl`

---

## Overview: Pipeline Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                     ORCHESTRATION LAYER                      │
│  Prolog: Pipeline definition, stage coordination            │
├─────────────────────────────────────────────────────────────┤
│                       STAGE LAYER                            │
│  ┌─────────┐    ┌─────────┐    ┌─────────┐                  │
│  │  AWK    │ →  │ Python  │ →  │   Go    │                  │
│  │ (parse) │    │(analyze)│    │(aggregate)│                │
│  └─────────┘    └─────────┘    └─────────┘                  │
├─────────────────────────────────────────────────────────────┤
│                      TRANSPORT LAYER                         │
│  Pipes (TSV/JSON) | In-process | Network (HTTP/TCP)         │
└─────────────────────────────────────────────────────────────┘
```

---

## pipeline_definition/2

Defines a multi-stage pipeline specification.

### Signature

```prolog
pipeline_definition(+Name, +Steps)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Name` | `atom` | Pipeline identifier |
| `Steps` | `list` | List of `step/4` terms |

### Step Format

```prolog
step(Name, Target, Script, Options)
```

| Field | Type | Description |
|-------|------|-------------|
| `Name` | `atom` | Stage identifier |
| `Target` | `atom` | `awk`, `python`, `go`, etc. |
| `Script` | `string` | Implementation file |
| `Options` | `list` | `input_format/1`, `output_format/1` |

### Example

```prolog
pipeline_definition(log_analysis, [
    step(parse, awk, 'parse_logs.awk', [
        input_format(text),
        output_format(tsv)
    ]),
    step(analyze, python, 'analyze_errors.py', [
        input_format(tsv),
        output_format(json)
    ]),
    step(summarize, awk, 'summarize.awk', [
        input_format(json),
        output_format(text)
    ])
]).
```

---

## generate_pipeline/3

Generates a shell script orchestrating multiple stages.

### Signature

```prolog
generate_pipeline(+Steps, +Options, -Script)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Steps` | `list` | List of `step/4` terms |
| `Options` | `list` | `format/1`, `input/1`, `output/1` |
| `Script` | `string` | Generated bash script |

### Example

```prolog
generate_pipeline(
    [
        step(parse, awk, 'parse_logs.awk', []),
        step(analyze, python, 'analyze_errors.py', []),
        step(summarize, awk, 'summarize.awk', [])
    ],
    [format(tsv), input('access.log')],
    Script
).
```

### Generated Output

```bash
#!/bin/bash
set -euo pipefail

cat "access.log" \
    | awk -f "parse_logs.awk" \
    | python3 "analyze_errors.py" \
    | awk -f "summarize.awk"
```

---

## Target Selection Strategies

### recommend_target/2

Recommends optimal target based on task type.

```prolog
recommend_target(+TaskType, -Target)
```

### Task-Target Mapping

| Task Type | Recommended Target | Reason |
|-----------|-------------------|--------|
| `parse_text` | `awk` | Fast field extraction |
| `field_extraction` | `awk` | Native regex support |
| `complex_transform` | `python` | Rich control flow |
| `parallel_aggregate` | `go` | Goroutines |
| `memory_sensitive` | `rust` | No GC overhead |
| `dotnet_ecosystem` | `csharp` | In-process calls |
| `ml_analytics` | `python` | NumPy, Pandas |

---

## Communication Patterns

### 1. Pipe-Based (Default)

```bash
stage1 | stage2 | stage3
```

| Format | Parse Speed | Size | Use Case |
|--------|-------------|------|----------|
| TSV | Very Fast | Compact | Simple tabular |
| JSON | Medium | Larger | Nested structures |
| Binary | Fastest | Smallest | High-volume |

### 2. In-Process

For same-runtime targets:

```prolog
in_process_stages([csharp, powershell, ironpython]).
```

No serialization overhead between .NET targets.

### 3. Network-Based

For distributed pipelines:

```prolog
remote_stage(heavy_compute, [
    host('gpu-worker.local'),
    port(8080),
    protocol(http)
]).
```

---

## retry_stage/3

Retries a failed stage with configurable attempts.

### Signature

```prolog
retry_stage(+Stage, +MaxRetries, -Result)
```

### Implementation Pattern

```prolog
retry_stage(Stage, MaxRetries, Result) :-
    between(1, MaxRetries, Attempt),
    (execute_stage(Stage, Result) -> true
    ; Attempt < MaxRetries,
      sleep(1),
      fail
    ).
```

---

## checkpoint_pipeline/3

Saves intermediate results for resume capability.

### Signature

```prolog
checkpoint_pipeline(+Pipeline, +Stage, +Data)
```

### Usage

```prolog
% Save checkpoint
checkpoint_pipeline(log_analysis, analyze, IntermediateData).

% Resume from checkpoint
resume_pipeline(log_analysis, analyze).
```

---

## partition_pipeline/3

Splits input for parallel processing.

### Signature

```prolog
partition_pipeline(+Input, +NumPartitions, -Results)
```

### Implementation Pattern

```prolog
partition_pipeline(Input, NumPartitions, Results) :-
    split_input(Input, NumPartitions, Partitions),
    parallel_map(process_partition, Partitions, Results),
    merge_results(Results, FinalResult).
```

---

## execute_pipeline/2

Executes a generated pipeline script.

### Signature

```prolog
execute_pipeline(+Script, -Results)
```

### Error Handling

```bash
#!/bin/bash
set -euo pipefail

if ! stage1 | stage2 | stage3; then
    echo "Pipeline failed" >&2
    exit 1
fi
```

---

## Performance Optimization

### Stage Parallelism

```bash
# Parallel stage execution with GNU Parallel
parallel --pipe --block 10M stage_worker ::: input_chunks/*
```

### Format Selection by Use Case

| Data Type | Recommended Format | Rationale |
|-----------|-------------------|-----------|
| Flat records | TSV | 5x faster parsing |
| Nested objects | JSON | Self-describing |
| Binary data | Native | Zero serialization |
| Mixed types | JSON | Type preservation |

---

## Related Documentation

- [Book 4 Chapter 2: Playbook Format](../02_playbook_format.md)
- [Book 4 Chapter 4: Economic Decisions](../04_economic_decisions.md)
- [Book 7: Cross-Target Glue](../../book-07-cross-target-glue/)
