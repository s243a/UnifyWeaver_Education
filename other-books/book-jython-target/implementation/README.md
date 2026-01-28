<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book Jython Target - Implementation Documentation

Technical deep-dive documentation for Jython code generation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 2: Pipeline Mode | [02_pipeline_mode_impl.md](./02_pipeline_mode_impl.md) | [02_pipeline_mode_questions.md](./02_pipeline_mode_questions.md) |

## Question Count

- Chapter 2: 10 questions

## Topics Covered

- `compile_predicate_to_jython/3` API
- Java I/O integration (BufferedReader)
- Jython vs CPython differences
- Safe dictionary access with .get()
- `from __future__ import print_function`
- JVM performance considerations
- JSONL pipeline processing

## Source Files

- `src/unifyweaver/targets/jython_target.pl`
