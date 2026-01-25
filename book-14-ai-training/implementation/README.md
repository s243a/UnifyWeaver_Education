<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book 14: Implementation Documentation

**Deep-dive technical documentation for RAG systems**

This folder contains detailed implementation documentation for each chapter in Book 14: AI Training. These docs explain the internals of key functions and algorithms.

## Available Docs

| File | Chapter | Key Topics |
|------|---------|------------|
| `06_transformer_distillation_impl.md` | Ch 6: Transformer Distillation | Givens rotations, angle supervision, rotation transformer |
| `06_transformer_distillation_questions.md` | Ch 6: Transformer Distillation | Q&A companion with 21 questions |

## Purpose

These implementation docs are designed for:

1. **RAG Systems** - Atomic, self-contained sections for retrieval
2. **ML Engineers** - Understanding the distillation algorithms
3. **Researchers** - Reference for geometric deep learning techniques

## Document Structure

Each implementation doc follows a consistent format:

```markdown
## Function Name

### Purpose
What the function does

### Mathematical Derivation
Formal derivation (where applicable)

### Algorithm
Step-by-step breakdown

### Implementation
Key code with explanation

### Edge Cases
Known limitations and handling
```

## Source Files

The implementation docs reference these modules:

- `scripts/test_rotation_distillation.py`
- `src/unifyweaver/targets/python_runtime/rotation_transformer.py`

## Key Algorithms

### Givens Rotation Mathematics

Givens rotations are 2D rotations in n-dimensional space affecting only coordinates (i, j). Any orthogonal matrix can be decomposed into Givens rotations.

### Angle-Supervised Distillation

Instead of supervising on output vectors, we compute optimal rotation angles for each input→target pair and use circular loss for proper wraparound handling.

### MinimalTransformProjection

Blends rotation parameters instead of output vectors, keeping transformations in the rotation manifold during interpolation.

## Contributing

When adding implementation docs:

1. Include mathematical derivations
2. Document numerical stability considerations
3. Add benchmark results where available
4. Reference related academic literature
