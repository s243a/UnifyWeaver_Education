<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book 5: Implementation Documentation

**Deep-dive technical documentation for RAG systems**

This folder contains detailed implementation documentation for Book 5: Python Target.

## Available Docs

| File | Chapter | Key Topics |
|------|---------|------------|
| `03_generator_mode_impl.md` | Ch 3: Generator Mode | Semi-naive evaluation, FrozenDict, fixpoint |
| `03_generator_mode_questions.md` | Ch 3: Generator Mode | Q&A companion with 14 questions |

## Purpose

These implementation docs are designed for:

1. **RAG Systems** - Atomic, self-contained sections for retrieval
2. **ML Engineers** - Understanding fixpoint evaluation
3. **Python Developers** - Generator-based data processing

## Key Concepts

### Semi-Naive Evaluation

Unlike Bash's BFS approach, Python generator mode uses Datalog-style semi-naive fixpoint evaluation:

1. Maintain `total` (all facts) and `delta` (new facts)
2. Apply rules only to `delta` facts
3. Iterate until fixpoint (no new facts)

### FrozenDict

Python dicts are unhashable. `FrozenDict` provides:
- Immutability for set membership
- O(1) hash-based lookup
- Dictionary-like interface

## Source Files

- `src/unifyweaver/targets/python_target.pl`
- `src/unifyweaver/targets/python_runtime/`

## Contributing

When adding implementation docs:

1. Document Python-specific patterns
2. Include type hints in examples
3. Compare with other targets where useful
4. Add performance characteristics
