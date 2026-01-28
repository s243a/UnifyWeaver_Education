<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book 10: SQL Target - Implementation Documentation

Technical deep-dive documentation for SQL query compilation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 3: JOINs | [03_joins_impl.md](./03_joins_impl.md) | [03_joins_questions.md](./03_joins_questions.md) |

## Question Count

- Chapter 3: 11 questions

## Topics Covered

### Chapter 3: JOINs
- INNER JOIN via shared variables (implicit)
- `sql_left_join/1`, `sql_right_join/1`, `sql_full_outer_join/1`
- Self-joins with auto-generated aliases
- Chained multi-table JOINs
- Finding non-matching rows with `sql_is_null/1`
- Mixed JOIN types in one query
- JOIN with WHERE conditions
- `sql_order_by/2` and `sql_limit/1`
- `compile_predicate_to_sql/3`
- Visual JOIN reference

## Source Files

- `src/unifyweaver/targets/sql_target.pl`

## Related Books

- [Book 3: C# Target](../book-03-csharp-target/) - Query engine runtime
- [Book 5: Python Target](../book-05-python-target/) - Database connectivity
