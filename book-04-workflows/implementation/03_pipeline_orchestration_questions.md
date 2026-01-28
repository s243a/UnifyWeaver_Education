<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: Pipeline Orchestration - Questions

Q&A companion for [03_pipeline_orchestration_impl.md](./03_pipeline_orchestration_impl.md).

---

## Question Index

1. [What does pipeline_definition/2 do?](#b04c03-q-pipeline-definition)
2. [What does generate_pipeline/3 produce?](#b04c03-q-generate-pipeline)
3. [How do I specify stage input/output formats?](#b04c03-q-stage-formats)
4. [Which target should I use for each task type?](#b04c03-q-target-selection)
5. [What communication patterns are available?](#b04c03-q-communication)
6. [How do in-process stages work?](#b04c03-q-in-process)
7. [What does retry_stage/3 do?](#b04c03-q-retry)
8. [How do checkpoints work?](#b04c03-q-checkpoints)
9. [How do I parallelize a pipeline?](#b04c03-q-parallel)
10. [What format should I use for data transfer?](#b04c03-q-formats)
11. [How is error handling implemented?](#b04c03-q-error-handling)

---

## Questions and Answers

### <a id="b04c03-q-pipeline-definition"></a>Q1: What does pipeline_definition/2 do?

**Answer**: Defines a named multi-stage pipeline specification:

```prolog
pipeline_definition(log_analysis, [
    step(parse, awk, 'parse.awk', [input_format(text), output_format(tsv)]),
    step(analyze, python, 'analyze.py', [input_format(tsv)])
]).
```

Each step specifies: name, target, script file, and options.

**See**: [pipeline_definition/2](./03_pipeline_orchestration_impl.md#pipeline_definition2)

---

### <a id="b04c03-q-generate-pipeline"></a>Q2: What does generate_pipeline/3 produce?

**Answer**: A bash script connecting stages via pipes:

```bash
#!/bin/bash
set -euo pipefail
cat "access.log" \
    | awk -f "parse_logs.awk" \
    | python3 "analyze_errors.py" \
    | awk -f "summarize.awk"
```

**See**: [generate_pipeline/3](./03_pipeline_orchestration_impl.md#generate_pipeline3)

---

### <a id="b04c03-q-stage-formats"></a>Q3: How do I specify stage input/output formats?

**Answer**: Use options in the step specification:

```prolog
step(parse, awk, 'parse.awk', [
    input_format(text),
    output_format(tsv)
])
```

Supported formats: `text`, `tsv`, `json`, `binary`.

**See**: [Step Format](./03_pipeline_orchestration_impl.md#step-format)

---

### <a id="b04c03-q-target-selection"></a>Q4: Which target should I use for each task type?

**Answer**:

| Task | Target | Reason |
|------|--------|--------|
| Text parsing | AWK | Fast field extraction |
| Complex logic | Python | Rich control flow |
| Parallel work | Go | Goroutines |
| Memory critical | Rust | No GC overhead |
| .NET ecosystem | C# | In-process calls |

**See**: [Target Selection Strategies](./03_pipeline_orchestration_impl.md#target-selection-strategies)

---

### <a id="b04c03-q-communication"></a>Q5: What communication patterns are available?

**Answer**: Three patterns:

| Pattern | Use Case |
|---------|----------|
| Pipe-based | Default, streaming via Unix pipes |
| In-process | Same runtime (.NET family) |
| Network | Distributed pipelines (HTTP/TCP) |

**See**: [Communication Patterns](./03_pipeline_orchestration_impl.md#communication-patterns)

---

### <a id="b04c03-q-in-process"></a>Q6: How do in-process stages work?

**Answer**: Targets in the same runtime share memory directly:

```prolog
in_process_stages([csharp, powershell, ironpython]).
```

No serialization overhead between C#, PowerShell, and IronPython.

**See**: [In-Process](./03_pipeline_orchestration_impl.md#2-in-process)

---

### <a id="b04c03-q-retry"></a>Q7: What does retry_stage/3 do?

**Answer**: Retries a failed stage with exponential backoff:

```prolog
retry_stage(Stage, MaxRetries, Result) :-
    between(1, MaxRetries, Attempt),
    (execute_stage(Stage, Result) -> true
    ; Attempt < MaxRetries, sleep(1), fail).
```

**See**: [retry_stage/3](./03_pipeline_orchestration_impl.md#retry_stage3)

---

### <a id="b04c03-q-checkpoints"></a>Q8: How do checkpoints work?

**Answer**: Save and resume intermediate results:

```prolog
% Save
checkpoint_pipeline(log_analysis, analyze, Data).

% Resume
resume_pipeline(log_analysis, analyze).
```

Useful for long-running pipelines.

**See**: [checkpoint_pipeline/3](./03_pipeline_orchestration_impl.md#checkpoint_pipeline3)

---

### <a id="b04c03-q-parallel"></a>Q9: How do I parallelize a pipeline?

**Answer**: Use `partition_pipeline/3`:

```prolog
partition_pipeline(Input, NumPartitions, Results) :-
    split_input(Input, NumPartitions, Partitions),
    parallel_map(process_partition, Partitions, Results).
```

Or GNU Parallel: `parallel --pipe --block 10M stage_worker ::: chunks/*`

**See**: [partition_pipeline/3](./03_pipeline_orchestration_impl.md#partition_pipeline3)

---

### <a id="b04c03-q-formats"></a>Q10: What format should I use for data transfer?

**Answer**:

| Format | Speed | Use When |
|--------|-------|----------|
| TSV | Very Fast | Simple tabular data |
| JSON | Medium | Nested structures |
| Binary | Fastest | High-volume, same language |

TSV is 5x faster for flat records.

**See**: [Format Selection](./03_pipeline_orchestration_impl.md#format-selection-by-use-case)

---

### <a id="b04c03-q-error-handling"></a>Q11: How is error handling implemented?

**Answer**: Generated scripts use `set -euo pipefail`:

```bash
#!/bin/bash
set -euo pipefail

if ! stage1 | stage2 | stage3; then
    echo "Pipeline failed" >&2
    exit 1
fi
```

Any stage failure stops the pipeline.

**See**: [Error Handling](./03_pipeline_orchestration_impl.md#error-handling)

---

## Summary

Pipeline orchestration provides:
- Multi-stage pipeline definitions with `pipeline_definition/2`
- Bash script generation with `generate_pipeline/3`
- Task-to-target recommendations
- Three communication patterns (pipe, in-process, network)
- Retry logic and checkpoint/resume
- Parallel execution via partitioning
