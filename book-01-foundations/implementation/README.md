<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book 1: Implementation Documentation

**Deep-dive technical documentation for RAG systems**

This folder contains detailed implementation documentation for each chapter in Book 1: Foundations. These docs explain the internals of key functions and algorithms.

## Available Docs

| File | Chapter | Key Topics |
|------|---------|------------|
| `01_introduction_impl.md` | Ch 1: Introduction | Installation, project structure, targets |
| `01_introduction_questions.md` | Ch 1: Introduction | Q&A companion with 13 questions |
| `02_prolog_fundamentals_impl.md` | Ch 2: Prolog Fundamentals | Terms, unification, modules, file I/O |
| `02_prolog_fundamentals_questions.md` | Ch 2: Prolog Fundamentals | Q&A companion with 22 questions |
| `03_architecture_impl.md` | Ch 3: Architecture | Constraint analyzer, template system, caching |
| `03_architecture_questions.md` | Ch 3: Architecture | Q&A companion with 20 questions |

## Purpose

These implementation docs are designed for:

1. **RAG Systems** - Atomic, self-contained sections for retrieval
2. **Developers** - Understanding the codebase internals
3. **Contributors** - Reference for extending functionality

## Document Structure

Each implementation doc follows a consistent format:

```markdown
## Function Name

### Purpose
What the function does

### Algorithm
Step-by-step breakdown

### Implementation
Key code excerpts with explanation

### Example
Usage examples with expected output
```

## Source Files

The implementation docs reference these core modules:

- `src/unifyweaver/core/constraint_analyzer.pl`
- `src/unifyweaver/core/template_system.pl`
- `src/unifyweaver/core/recursive_compiler.pl`
- `src/unifyweaver/incremental/incremental_compiler.pl`

## Contributing

When adding implementation docs:

1. Use the established format
2. Include working code examples
3. Document edge cases
4. Add source file references
