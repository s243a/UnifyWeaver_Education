<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book 2: Implementation Documentation

**Deep-dive technical documentation for RAG systems**

This folder contains detailed implementation documentation for each chapter in Book 2: Bash Target.

## Available Docs

| File | Chapter | Key Topics |
|------|---------|------------|
| `01_first_program_impl.md` | Ch 1: First Program | Compilation workflow, compile_recursive, compile_facts |
| `01_first_program_questions.md` | Ch 1: First Program | Q&A companion with 9 questions |
| `02_stream_compilation_impl.md` | Ch 2: Stream Compilation | compile_stream, joins, OR handling |
| `02_stream_compilation_questions.md` | Ch 2: Stream Compilation | Q&A companion with 10 questions |
| `03_constraints_impl.md` | Ch 3: Constraints | Deduplication strategies, constraint pragma |
| `03_constraints_questions.md` | Ch 3: Constraints | Q&A companion with 12 questions |

## Purpose

These implementation docs are designed for:

1. **RAG Systems** - Atomic, self-contained sections for retrieval
2. **Developers** - Understanding Bash code generation
3. **Contributors** - Reference for extending functionality

## Source Files

The implementation docs reference these core modules:

- `src/unifyweaver/core/stream_compiler.pl`
- `src/unifyweaver/core/recursive_compiler.pl`
- `src/unifyweaver/core/constraint_analyzer.pl`
- `src/unifyweaver/core/template_system.pl`

## Contributing

When adding implementation docs:

1. Use the established format
2. Include Bash code examples
3. Document pipeline patterns
4. Add source file references
