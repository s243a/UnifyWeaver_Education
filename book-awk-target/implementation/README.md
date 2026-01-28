<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book AWK Target - Implementation Documentation

Technical deep-dive documentation for AWK code generation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Overview | [01_overview_impl.md](./01_overview_impl.md) | [01_overview_questions.md](./01_overview_questions.md) |

## Question Count

- Overview: 10 questions

## Topics Covered

- `compile_predicate_to_awk/3` API
- Input formats (TSV, CSV, JSONL)
- Fact compilation to associative arrays
- Rule compilation with filters
- Aggregations (sum, count, max, min)
- Tail recursion to while loops
- Regex matching (`match/2`, `match/3`, `match/4`)
- AWK target limitations

## Source Files

- `src/unifyweaver/targets/awk_target.pl`
