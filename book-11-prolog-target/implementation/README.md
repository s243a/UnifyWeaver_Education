<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book 11: Prolog Target - Implementation Documentation

Technical deep-dive documentation for Prolog script generation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 2: Architecture | [02_architecture_impl.md](./02_architecture_impl.md) | [02_architecture_questions.md](./02_architecture_questions.md) |

## Question Count

- Chapter 2: 10 questions

## Topics Covered

### Chapter 2: Architecture
- `generate_prolog_script/3` main entry point
- 8-step generation pipeline
- `analyze_dependencies/2` module scanning
- Dialect predicates: `supported_dialect/1`, `dialect_capabilities/2`
- Shebang generation (`dialect_shebang/2`)
- Import generation (`dialect_imports/3`)
- Initialization code differences (SWI vs GNU)
- `write_prolog_script/3` file output
- `compile_script_safe/3` error handling
- `validate_for_dialect/3` compatibility checking
- Data flow diagram
- Design principles (separation, extensibility, robustness)

## Source Files

- `src/unifyweaver/targets/prolog_target.pl`
- `src/unifyweaver/targets/prolog_dialects.pl`
- `src/unifyweaver/targets/prolog_service_target.pl`

## Related Books

- [Book 1: Foundations](../book-01-foundations/) - Core architecture
- [Book 8: Security & Firewall](../book-08-security-firewall/) - Policy integration
