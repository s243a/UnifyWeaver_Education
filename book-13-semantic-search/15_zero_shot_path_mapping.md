<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 15: Zero-Shot Path Mapping

*Mapping queries to hierarchical paths via Minimal Transformation*

## Overview

This chapter covers Zero-Shot Path Mapping—a technique for retrieving hierarchical paths from short queries without per-query training. Given a query like `locate_tree('Physics')`, the system returns the full materialized path to that item in a tree structure.

### Use Case: Pearltrees

Pearltrees is a visual bookmarking tool that organizes content in tree hierarchies. Users want to find items by title, but the actual location requires knowing the full path. Zero-Shot Path Mapping bridges this gap.

## The Materialized Path Format

Target strings encode both IDs and hierarchical titles:

```text
/10311468/11820376/10388356/11110453
- s243a
  - Subjects of learning
    - science
      - Physics
```

### Multi-Account Paths

When paths span multiple accounts, boundaries are marked with `@account`:

```text
/10311468/90289371@s243a_groups/90289478/91849041
- s243a
  - s243a_groups @s243a_groups
    - s243a
      - Main topic classifications
```

## Architecture

```
┌──────────────────┐    ┌──────────────────┐    ┌──────────────────┐
│  Query           │    │  Embed + Project │    │  Search          │
│  locate_tree(X)  │───▶│  q' = softmax(W) │───▶│  argmax(cos)     │
└──────────────────┘    └──────────────────┘    └──────────────────┘
                              │
                              ▼
                    ┌──────────────────┐
                    │  Per-Tree        │
                    │  Procrustes      │
                    │  Matrices        │
                    └──────────────────┘
```

### Per-Tree Procrustes

Instead of one global transformation, we learn a separate Procrustes (rotation + scaling) matrix for each parent tree:

```python
from minimal_transform import MinimalTransformProjection

projector = MinimalTransformProjection(
    smooth_method="none",   # Pure Procrustes per-tree
    fidelity_weight=1.0,
    allow_scaling=True
)

# Train on clusters (grouped by parent tree)
clusters = [(Q_emb_tree1, A_emb_tree1), (Q_emb_tree2, A_emb_tree2), ...]
stats = projector.train(clusters)
```

At inference, **softmax routing** selects which transformation to apply based on query similarity to cluster centroids:

```python
# Similarity to each cluster centroid
similarities = query @ centroids.T
weights = softmax(similarities / temperature)

# Weighted combination of projections
projected = sum(w * (query @ W_i) for w, W_i in zip(weights, matrices))
```

## Data Generation

### Single Account

```bash
python scripts/pearltrees_target_generator.py \
  export.rdf \
  targets.jsonl \
  --query-template "locate_tree('{title}')" \
  --item-type tree \
  --filter-path "Physics"
```

### Multiple Accounts

```bash
python scripts/pearltrees_multi_account_generator.py \
  --account s243a s243a.rdf \
  --account s243a_groups s243a_groups.rdf \
  targets.jsonl \
  --query-template "locate_tree('{title}')"
```

**Features:**
- First `--account` is automatically the primary account
- RefPearls (actual subtrees) preferred over AliasPearls (shortcuts)
- "Drop zone" trees skipped as entry points

## Training

```bash
python scripts/train_pearltrees_projection.py \
  targets.jsonl \
  models/projection.pkl
```

The model saves:
- **W_stack**: Per-cluster transformation matrices (N × 768 × 768)
- **centroids**: Cluster centroids for routing (N × 768)
- **target_embeddings**: Pre-computed for fast search
- **target_ids/titles**: For result display

## Evaluation

```bash
python scripts/evaluate_pearltrees_projection.py \
  models/projection.pkl \
  targets.jsonl
```

### Results (Physics Subset - 263 Trees)

| Metric | Value |
|--------|-------|
| Recall@1 | **72.24%** |
| Recall@5 | **94.30%** |
| Recall@10 | **96.20%** |
| MRR | **0.8261** |

## Fast Inference

```bash
# Single query (~30ms)
python scripts/infer_pearltrees.py models/projection.pkl "locate_tree('Thermodynamics')"

# Interactive mode
python scripts/infer_pearltrees.py models/projection.pkl --interactive
```

### Latency Breakdown

| Phase | Time |
|-------|------|
| Load model (one-time) | ~1.2s |
| Embed query | ~15-20ms |
| Project + search | ~11-14ms |
| **Total per query** | **~30ms** |

## Why It Works

1. **ModernBERT Embeddings**: Understands code-like syntax (`locate_tree(...)`)
2. **Hierarchical Targets**: Path strings encode context
3. **Per-Tree Transforms**: Fine-grained alignment per subtree
4. **Softmax Routing**: Smooth interpolation between transforms
5. **Cached Embeddings**: No re-embedding at inference

## Connection to Minimal Transformation

This builds on Chapter 7's density scoring and the Minimal Transformation Projection proposal. The key insight is that **per-cluster Procrustes** with **softmax routing** outperforms a single global transform.

## Exercises

1. **Vary Temperature**: Try `temperature=0.05` vs `temperature=1.0`. How does it affect routing sharpness?

2. **Compare Smoothing**: Train with `smooth_method="fft"`. Does it improve on small clusters?

3. **Cross-Account Filter**: Generate data for a topic that spans both accounts. What's the path structure?

4. **Hard Negative Mining**: Find cases where Recall@1 fails but Recall@5 succeeds. What patterns emerge?

## Summary

Zero-Shot Path Mapping combines:
- Materialized path representations
- Per-tree Procrustes transformations
- Softmax routing for inference
- Cached embeddings for speed

This enables ~30ms query latency with 72% Recall@1 on hierarchical retrieval tasks.

---

*Next: Chapter 16 covers federated path mapping across distributed nodes.*
