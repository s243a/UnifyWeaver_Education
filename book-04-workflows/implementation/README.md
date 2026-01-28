<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book 4: Workflows - Implementation Documentation

Technical deep-dive documentation for pipeline orchestration.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 3: Pipeline Orchestration | [03_pipeline_orchestration_impl.md](./03_pipeline_orchestration_impl.md) | [03_pipeline_orchestration_questions.md](./03_pipeline_orchestration_questions.md) |

## Question Count

- Chapter 3: 11 questions

## Topics Covered

### Chapter 3: Pipeline Orchestration
- `pipeline_definition/2` specification
- `generate_pipeline/3` script generation
- Stage input/output format specification
- Target selection strategies (`recommend_target/2`)
- Communication patterns (pipe, in-process, network)
- `retry_stage/3` with exponential backoff
- `checkpoint_pipeline/3` for resume capability
- `partition_pipeline/3` for parallelization
- Format selection (TSV, JSON, binary)
- Error handling with `set -euo pipefail`

## Source Files

- `src/unifyweaver/workflows/pipeline.pl`

## Related Books

- [Book 7: Cross-Target Glue](../book-07-cross-target-glue/) - Pipe protocols
- [Book 2: Bash Target](../book-02-bash-target/) - Stage implementation
