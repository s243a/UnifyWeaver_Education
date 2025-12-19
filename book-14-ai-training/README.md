<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Book 14: AI Training with UnifyWeaver

**LDA Projections, Multi-Head Routing, and Transformer Distillation**

*Part of the [UnifyWeaver Education Series](../README.md)*

This book covers training AI models for semantic search using UnifyWeaver. While [Book 13](../book-13-semantic-search/README.md) teaches you how to *use* semantic search, this book teaches you how to *train* the underlying models and understand the theory behind them.

## Prerequisites

**Required:**
- [Book 1: Foundations](../book-01-foundations/README.md)
- [Book 13: Semantic Search](../book-13-semantic-search/README.md)

**Recommended:**
- Basic linear algebra (vectors, matrices, dot products)
- Familiarity with machine learning concepts (embeddings, softmax)

**Technical:**
- Python 3.9+ with PyTorch
- sentence-transformers library
- SQLite3

## What You'll Learn

- How embedding models encode semantic meaning
- Training LDA projections that map questions to answers
- Multi-head routing with soft attention
- The connection between LDA multi-head and transformer attention
- Knowledge distillation from LDA to transformers
- Cross-target deployment (train in Python, deploy in Rust/Go)

## Chapter Overview

### [Chapter 1: Introduction](01_introduction.md)
- Why train custom models?
- The asymmetric embedding problem
- Overview of the training pipeline
- Train once, deploy anywhere

### [Chapter 2: Embedding Providers](02_embedding_providers.md)
- Sentence transformers and ONNX
- Multi-target support (Python, Go, Rust)
- Choosing embedding dimensions and context length
- GPU acceleration with Flash Attention

### [Chapter 3: LDA Semantic Projection](03_lda_projection.md)
- The question-answer asymmetry problem
- Computing the W projection matrix
- Pseudoinverse and regularization
- Validation: projected vs direct similarity

### [Chapter 4: Multi-Head Routing](04_multi_head_routing.md)
- From single projection to multiple heads
- Clustering questions by semantic similarity
- Soft routing with temperature-controlled softmax
- Per-cluster answer embeddings

### [Chapter 5: Training Pipeline](05_training_pipeline.md)
- The LDA database schema
- Training batch tracking with file hashes
- Migration scripts and data formats
- Incremental training workflows

### [Chapter 6: Transformer Distillation](06_transformer_distillation.md)
- The H^L = N equivalence conjecture
- Knowledge distillation with MSE + cosine loss
- Optimal architecture selection
- When transformers beat flat LDA (scaling analysis)

### [Chapter 7: Cross-Target Deployment](07_cross_target_deployment.md)
- Train in Python, deploy in Rust/Go
- Loading NPY files across languages
- Unified model file conventions
- Performance benchmarks

### [Chapter 8: Advanced Topics](08_advanced_topics.md)
- Smoothness regularization for sparse clusters
- Hierarchical transformer ensembles
- Asymmetric embedding models (query vs document)
- Future directions

### [Chapter 9: Knowledge Graph Topology](09_kg_topology.md)
- 11 relation types (foundational, preliminary, compositional, etc.)
- Seed level tracking for provenance
- Learning path generation
- Knowledge gap detection
- Integration with distributed search

## Key Concepts

### The Core Insight: LDA as Attention

Multi-head LDA projection is structurally equivalent to transformer attention:

| LDA Multi-Head | Transformer Attention |
|----------------|----------------------|
| Centroids | Keys |
| Answer embeddings | Values |
| Query | Query |
| Softmax routing | Attention weights |

This insight enables knowledge distillation from LDA to compact transformers.

### The H^L = N Equivalence

A transformer with H heads per layer and L layers has routing capacity equivalent to H^L flat LDA heads:

```
H^L = N_flat

Example: H=4, L=3 → 64 equivalent flat heads
```

This provides a principled way to choose transformer architecture.

### Train Once, Deploy Anywhere

```
┌─────────────────┐
│  Python         │ ← Training (PyTorch, sentence-transformers)
│  Training       │
└────────┬────────┘
         │
         ▼
    ┌─────────┐
    │ lda.db  │ ← SQLite + NPY files
    │ + NPY   │
    └────┬────┘
         │
    ┌────┴────┬──────────┐
    ▼         ▼          ▼
┌───────┐ ┌───────┐ ┌───────┐
│Python │ │  Go   │ │ Rust  │  ← Inference (any target)
│Runtime│ │Runtime│ │Runtime│
└───────┘ └───────┘ └───────┘
```

## Quick Start

```bash
# Train multi-head projection
python3 scripts/train_multi_head_projection.py \
    --db playbooks/lda-training-data/lda.db \
    --model all-MiniLM-L6-v2 \
    --temperature 0.1 \
    --validate

# Test transformer distillation
python3 scripts/test_transformer_distillation.py \
    --db playbooks/lda-training-data/lda.db \
    --epochs 200 \
    --cosine-weight 0.7
```

## Installation

```bash
# Core dependencies
pip install numpy torch sentence-transformers

# Optional: GPU support
pip install flash-attn  # requires CUDA

# Optional: Advanced embedding models
pip install transformers  # for nomic-embed, ModernBERT
```

## Relationship to Book 13

| Book 13: Semantic Search | Book 14: AI Training |
|--------------------------|---------------------|
| Using PtSearcher | Training the models PtSearcher uses |
| Consuming embeddings | Producing embeddings |
| Application patterns | Theory and algorithms |
| "How to search" | "How search works" |

## What's Next?

After completing this book, you'll be able to:
- Train custom LDA projections for your domain
- Understand the theory connecting LDA and transformers
- Choose optimal architectures for your scale
- Deploy trained models across Python, Go, and Rust
- Extend the system with new embedding providers
- Build knowledge graphs with typed relationships
- Generate learning paths from KG topology
- Integrate with distributed semantic search

## License

This educational content is licensed under CC BY 4.0.
Code examples are dual-licensed under MIT OR Apache-2.0.
