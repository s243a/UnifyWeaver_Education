<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book 16: Ruby Target - Implementation Documentation

Technical deep-dive documentation for Ruby code generation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 2: Basic Compilation | [02_basic_compilation_impl.md](./02_basic_compilation_impl.md) | [02_basic_compilation_questions.md](./02_basic_compilation_questions.md) |

## Question Count

- Chapter 2: 10 questions

## Topics Covered

### Chapter 2: Basic Compilation
- `compile_predicate_to_ruby/3` API
- Fact compilation to arrays with `each`
- Simple rule compilation with nested blocks
- Join compilation via nested blocks
- Unique variable naming (`p0`, `p1`, ...) to avoid shadowing
- Multiple clause compilation (OR semantics)
- `next unless` guard pattern
- Block syntax (`do...end` vs `{...}`)
- Block/yield pattern for result streaming
- Enumerator support for Enumerable methods

## Source Files

- `src/unifyweaver/targets/ruby_target.pl`

## Related Books

- [Book 15: Perl Target](../book-15-perl-target/) - Similar callback patterns
- [Book 5: Python Target](../book-05-python-target/) - Generator comparison
