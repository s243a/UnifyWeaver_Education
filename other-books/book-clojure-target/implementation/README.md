<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book Clojure Target - Implementation Documentation

Technical deep-dive documentation for Clojure code generation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 2: Pipeline Mode | [02_pipeline_mode_impl.md](./02_pipeline_mode_impl.md) | [02_pipeline_mode_questions.md](./02_pipeline_mode_questions.md) |

## Question Count

- Chapter 2: 10 questions

## Topics Covered

- `compile_predicate_to_clojure/3` API
- Threading macro (->>)
- `keep` vs `filter` for nil handling
- Keyword access with defaults
- Lazy sequence evaluation
- clojure.data.json integration
- JSONL pipeline processing

## Source Files

- `src/unifyweaver/targets/clojure_target.pl`
