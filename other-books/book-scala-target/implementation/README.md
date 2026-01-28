<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book Scala Target - Implementation Documentation

Technical deep-dive documentation for Scala code generation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 2: Pipeline Mode | [02_pipeline_mode_impl.md](./02_pipeline_mode_impl.md) | [02_pipeline_mode_questions.md](./02_pipeline_mode_questions.md) |

## Question Count

- Chapter 2: 10 questions

## Topics Covered

- `compile_predicate_to_scala/3` API
- Option monad for success/failure
- Pattern matching with guards
- flatMap chaining
- Type alias for Record
- Automatic None filtering

## Source Files

- `src/unifyweaver/targets/scala_target.pl`
