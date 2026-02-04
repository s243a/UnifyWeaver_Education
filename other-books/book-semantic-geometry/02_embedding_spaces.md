<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 UnifyWeaver Contributors

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 2: Embedding Spaces

## Fixed Geometry

Embedding models like BERT, MiniLM, and Nomic map text to fixed-dimensional vectors. These vectors live in a geometry determined entirely by the model's pre-training:

- **MiniLM** (384D) — Fast, general-purpose, trained on sentence pairs
- **Nomic** (768D) — Better clustering, supports asymmetric search with `search_query:` and `search_document:` prefixes

The geometry is "fixed" in the sense that the distance between any two texts is determined by the model weights, which don't change at inference time. Cosine similarity is the standard metric:

```python
import numpy as np

def cosine_similarity(a, b):
    return np.dot(a, b) / (np.linalg.norm(a) * np.linalg.norm(b))
```

This works well for finding semantically similar documents. But it doesn't capture organizational relationships — "Physics" and "Quantum Mechanics" may be close in cosine similarity, but their hierarchical relationship (parent → child) is lost.

## Projecting to 2D

To visualize 768-dimensional embeddings, we project to 2D using SVD (Singular Value Decomposition):

![Embedding space projection](images/embedding_projection.png)

*200 Wikipedia physics articles in Nomic embedding space, projected to 2D via SVD. The two SVD components capture the directions of maximum variance.*

```python
from numpy.linalg import svd

def project_to_2d(embeddings):
    """Project N×D embeddings to N×2 via SVD."""
    centered = embeddings - embeddings.mean(axis=0)
    U, S, Vt = svd(centered, full_matrices=False)
    points_2d = U[:, :2] * S[:2]
    var_explained = S[:2]**2 / (S**2).sum()
    return points_2d, var_explained
```

The variance explained by each SVD component tells us how much structure is captured. When SVD 1 + SVD 2 approaches 100%, the data essentially lives on a 2D manifold within the high-dimensional space.

## Limitations of Fixed Geometry

Fixed embedding spaces have a fundamental limitation: **the metric is determined by pre-training, not by your data**. For a physics knowledge base:

- "Physics" and "Applied Physics" are semantically close (cosine ~0.85)
- "Physics" and "List of Physicists" are also close (cosine ~0.80)
- But organizationally, "Physics" is a parent concept while "List of Physicists" is a categorization index

Cosine similarity cannot distinguish these relationships. We need a **learned metric** — a distance function trained on the actual organizational structure of our data.

## Learned Transformations

UnifyWeaver uses several approaches to learn task-specific geometries:

1. **Bivector projections** — Orthogonal transformations that preserve distances while aligning to organizational structure
2. **Organizational metric models** — Neural networks that predict hierarchical distance between document pairs
3. **Wikipedia Physics distance model** — Trained specifically on Wikipedia-Pearltrees hierarchical mappings

Each of these creates a different geometric "lens" for viewing the same underlying data. We explore these in detail in Chapters 4 and 7.

## Key Insight

The embedding space is just the starting point. The real geometric structure — the hierarchy, the organizational relationships, the information flow — requires either learning a new metric or projecting through a trained model. The raw embedding geometry captures semantics, but not structure.

---

**Previous**: [Chapter 1: Introduction](01_introduction.md) | **Next**: [Chapter 3: Density Manifolds](03_density_manifolds.md)
