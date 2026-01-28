<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book C Target - Implementation Documentation

Technical deep-dive documentation for C code generation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 2: Pipeline Mode | [02_pipeline_mode_impl.md](./02_pipeline_mode_impl.md) | [02_pipeline_mode_questions.md](./02_pipeline_mode_questions.md) |

## Question Count

- Chapter 2: 10 questions

## Topics Covered

- `compile_predicate_to_c/3` API
- JSONL pipeline via cJSON library
- cJSON API reference
- Filter compilation with type checks
- Manual memory management
- Error handling (stderr)
- Build/compilation commands

## Source Files

- `src/unifyweaver/targets/c_target.pl`
