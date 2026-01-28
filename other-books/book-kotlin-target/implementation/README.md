<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book Kotlin Target - Implementation Documentation

Technical deep-dive documentation for Kotlin code generation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 2: Pipeline Mode | [02_pipeline_mode_impl.md](./02_pipeline_mode_impl.md) | [02_pipeline_mode_questions.md](./02_pipeline_mode_questions.md) |

## Question Count

- Chapter 2: 10 questions

## Topics Covered

- `compile_predicate_to_kotlin/3` API
- Lazy sequences via `lineSequence()`
- Filter compilation with safe cast
- `mapNotNull` for null filtering
- Object singleton pattern
- Gson integration
- Kotlin null safety idioms

## Source Files

- `src/unifyweaver/targets/kotlin_target.pl`
