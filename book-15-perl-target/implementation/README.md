<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book 15: Perl Target - Implementation Documentation

Technical deep-dive documentation for Perl code generation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 2: Basic Compilation | [02_basic_compilation_impl.md](./02_basic_compilation_impl.md) | [02_basic_compilation_questions.md](./02_basic_compilation_questions.md) |

## Question Count

- Chapter 2: 10 questions

## Topics Covered

### Chapter 2: Basic Compilation
- `compile_predicate_to_perl/3` API
- Fact compilation to `@facts` arrays
- Simple rule compilation with nested callbacks
- Join compilation via nested subs
- Unique variable naming (`p0`, `p1`, ...) to avoid shadowing
- Multiple clause compilation (OR semantics)
- `return unless` guard pattern
- String/numeric comparison operators
- Callback pattern for result streaming

## Source Files

- `src/unifyweaver/targets/perl_target.pl`

## Related Books

- [Book 16: Ruby Target](../book-16-ruby-target/) - Similar callback patterns
- [Book 2: Bash Target](../book-02-bash-target/) - Unix text processing
