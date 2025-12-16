<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Embedding Providers

**Multi-Target Embeddings: Python, Go, and Rust**

## What Are Embeddings?

Embeddings are dense vector representations of text that capture semantic meaning. Similar texts have similar vectors (high cosine similarity), enabling semantic search.

```
"How do I authenticate?" → [0.12, -0.34, 0.56, ...] (384 dimensions)
"Login process"         → [0.11, -0.32, 0.58, ...] (similar vector)
"Pizza recipe"          → [-0.45, 0.23, -0.12, ...] (different vector)
```

## Embedding Models in UnifyWeaver

### all-MiniLM-L6-v2 (Default)

| Property | Value |
|----------|-------|
| Dimensions | 384 |
| Context | 256 tokens |
| Speed | Very fast |
| Quality | Good for general use |

```python
from sentence_transformers import SentenceTransformer
model = SentenceTransformer('all-MiniLM-L6-v2')
embedding = model.encode("How do I log in?")
```

### nomic-embed-text-v1.5 (Large Context)

| Property | Value |
|----------|-------|
| Dimensions | 768 |
| Context | 8192 tokens |
| Speed | Moderate |
| Quality | Excellent, supports prefixes |

```python
from sentence_transformers import SentenceTransformer
model = SentenceTransformer('nomic-ai/nomic-embed-text-v1.5', trust_remote_code=True)

# Asymmetric prefixes
query_emb = model.encode("search_query: How do I log in?")
doc_emb = model.encode("search_document: Authentication requires...")
```

## Multi-Target Support

UnifyWeaver provides embedding support across multiple runtime targets:

### Python Runtime

Full-featured with GPU support:

```python
# src/unifyweaver/targets/python_runtime/modernbert_embedding.py
from modernbert_embedding import ModernBertEmbedding

embedder = ModernBertEmbedding(device="cuda")  # or "cpu", "mps"
embeddings = embedder.encode_batch(texts, batch_size=32)
```

Features:
- Flash Attention 2 (auto-detected)
- CUDA/MPS/CPU device selection
- Batch processing

### Go Runtime

ONNX-based inference:

```go
// src/unifyweaver/targets/go_runtime/embedding/onnx_embedder.go
embedder, _ := embedding.NewOnnxEmbedder(modelPath)
vector := embedder.Embed("How do I log in?")
```

Features:
- Pure Go with ONNX Runtime
- No Python dependency
- Good for microservices

### Rust Runtime

High-performance inference:

```rust
// examples/pearltrees/embedding.rs
let embedder = OnnxEmbedder::new(model_path)?;
let vector = embedder.embed("How do I log in?")?;
```

Features:
- Zero-copy where possible
- Excellent for CLI tools
- Integrates with PtSearcher

## Choosing an Embedding Model

| Use Case | Recommended Model | Why |
|----------|------------------|-----|
| Short queries (<256 tokens) | all-MiniLM-L6-v2 | Fast, good quality |
| Long documents | nomic-embed-text-v1.5 | 8K context |
| Asymmetric search | nomic with prefixes | Query/doc distinction |
| Resource constrained | all-MiniLM-L6-v2 | Smaller, faster |
| Maximum quality | nomic-embed-text-v1.5 | Better representations |

## GPU Acceleration

### Flash Attention 2

For supported models, Flash Attention provides significant speedups:

```python
# Auto-detected when available
from modernbert_embedding import ModernBertEmbedding

embedder = ModernBertEmbedding()
# Logs: "Flash Attention 2 enabled" if available
```

Requirements:
- CUDA-capable GPU
- `pip install flash-attn`
- Compatible model architecture

### Batch Processing

Always batch for efficiency:

```python
# Good: batch processing
embeddings = model.encode(texts, batch_size=32)

# Bad: one at a time
embeddings = [model.encode(t) for t in texts]  # Much slower
```

## Storage Format: NPY Files

Embeddings are stored as NumPy `.npy` files for cross-target compatibility:

```python
import numpy as np

# Save
np.save("embedding.npy", vector)

# Load (works in Python, Go, Rust)
vector = np.load("embedding.npy")
```

The NPY format is simple and supported by all UnifyWeaver runtimes.

## Practical Exercise

Try embedding some queries and measuring similarity:

```python
from sentence_transformers import SentenceTransformer
import numpy as np

model = SentenceTransformer('all-MiniLM-L6-v2')

queries = [
    "How do I log in?",
    "What's the authentication process?",
    "How to make pizza?"
]

embeddings = model.encode(queries)

# Compute pairwise cosine similarities
def cosine_sim(a, b):
    return np.dot(a, b) / (np.linalg.norm(a) * np.linalg.norm(b))

for i, q1 in enumerate(queries):
    for j, q2 in enumerate(queries):
        if i < j:
            sim = cosine_sim(embeddings[i], embeddings[j])
            print(f"{q1[:20]:20} ↔ {q2[:20]:20} = {sim:.3f}")
```

Expected output:
```
How do I log in?     ↔ What's the authenti = 0.782
How do I log in?     ↔ How to make pizza?  = 0.234
What's the authenti  ↔ How to make pizza?  = 0.189
```

## Chapter Summary

- Embeddings convert text to vectors capturing semantic meaning
- all-MiniLM-L6-v2: fast, 384 dims, 256 token context
- nomic-embed-text-v1.5: quality, 768 dims, 8K context
- UnifyWeaver supports Python, Go, and Rust runtimes
- NPY format enables cross-target compatibility
- Always use batch processing for efficiency

## Next Chapter

[Chapter 3: LDA Semantic Projection](03_lda_projection.md) - Learning to map question embeddings toward answer embeddings.
