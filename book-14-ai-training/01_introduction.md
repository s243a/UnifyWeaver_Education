<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 1: Introduction

**Why Train Custom AI Models?**

## The Problem: Asymmetric Semantics

When users search for information, they phrase queries differently than how answers are written:

| User Query | Document Text |
|------------|---------------|
| "How do I log in?" | "Authentication requires valid credentials..." |
| "What's the syntax for loops?" | "The `for` statement iterates over..." |
| "Why is my code slow?" | "Performance optimization involves..." |

Standard embedding models encode both into the same space, but the *intent* differs:
- Queries are **questions** - short, informal, seeking
- Documents are **answers** - detailed, formal, providing

This **asymmetric semantics** problem means direct cosine similarity often misses the best matches.

## The Solution: Learned Projections

Instead of hoping embeddings align naturally, we *learn* a projection that maps question embeddings toward their corresponding answer embeddings:

```
question_emb ──► W ──► projected_emb ≈ answer_emb
```

This projection W is trained on question-answer pairs from your domain, capturing the semantic relationship between how users ask and how answers are written.

## What This Book Covers

### Part 1: Foundations (Chapters 1-3)
- Embedding providers across targets
- Basic LDA projection (single W matrix)
- The pseudoinverse approach

### Part 2: Multi-Head Routing (Chapter 4)
- Why single projection isn't enough
- Clustering by question type
- Soft routing with temperature

### Part 3: Training Infrastructure (Chapter 5)
- Database schema for embeddings
- Batch tracking and incremental updates
- Migration workflows

### Part 4: Scaling (Chapters 6-7)
- Transformer distillation for large scale
- Cross-target deployment
- Performance optimization

### Part 5: Advanced Topics (Chapter 8)
- Regularization techniques
- Hierarchical ensembles
- Future directions

## The UnifyWeaver AI Stack

```
┌────────────────────────────────────────────────────────┐
│                    Applications                         │
│         (Playbooks, Semantic Search, RAG)              │
├────────────────────────────────────────────────────────┤
│                   Inference Layer                       │
│     Python Runtime │ Go Runtime │ Rust Runtime         │
├────────────────────────────────────────────────────────┤
│                   Model Storage                         │
│          SQLite (lda.db) + NPY files                   │
├────────────────────────────────────────────────────────┤
│                  Training Layer                         │
│    LDA Projection │ Multi-Head │ Transformer           │
├────────────────────────────────────────────────────────┤
│                 Embedding Layer                         │
│   sentence-transformers │ ONNX │ nomic-embed           │
└────────────────────────────────────────────────────────┘
```

## Prerequisites

Before starting this book, you should:

1. **Understand basic semantic search** (Book 13)
   - What embeddings are
   - How vector similarity works
   - Using PtSearcher for retrieval

2. **Know some linear algebra**
   - Vectors and dot products
   - Matrix multiplication
   - What a pseudoinverse is (we'll explain)

3. **Have Python ML tools installed**
   ```bash
   pip install numpy torch sentence-transformers
   ```

## A Taste of What's Coming

Here's a preview of multi-head LDA projection:

```python
# Each cluster has its own centroid and answer embedding
for query in queries:
    # Compute similarity to each cluster centroid
    similarities = query @ centroids.T

    # Soft routing via temperature-controlled softmax
    weights = softmax(similarities / temperature)

    # Weighted combination of answer embeddings
    projected = weights @ answer_embeddings
```

And transformer distillation that approximates this:

```python
# Transformer with H^L = N equivalent capacity
transformer = ProjectionTransformer(
    num_heads=4,   # H
    num_layers=2   # L → 4^2 = 16 equivalent heads
)

# Train to match LDA output
train_distillation(
    transformer,
    lda_projection,
    training_queries,
    cosine_weight=0.7  # Essential for directional alignment
)
```

## Chapter Summary

- Semantic search has an asymmetry problem: queries ≠ documents
- Learned projections bridge this gap
- UnifyWeaver provides a full stack: train in Python, deploy anywhere
- This book covers the theory and practice of training these models

## Next Chapter

[Chapter 2: Embedding Providers](02_embedding_providers.md) - Understanding the foundation layer that converts text to vectors.
