<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 6: Transformer Distillation - Implementation Details

This document provides function-level documentation for the H^L = N equivalence and knowledge distillation.

**Source**: `scripts/test_transformer_distillation.py`, `src/unifyweaver/ml/projection_transformer.py`

---

## Overview: H^L = N Equivalence

The key insight: a transformer with H attention heads per layer and L layers has routing capacity equivalent to H^L flat LDA heads.

| Configuration | Equivalent Flat Heads |
|---------------|----------------------|
| H=4, L=2 | 4² = 16 |
| H=4, L=3 | 4³ = 64 |
| H=4, L=4 | 4⁴ = 256 |

---

## optimal_architecture Function

Selects H and L given target N flat heads.

### Signature

```python
def optimal_architecture(n_flat_heads: int, prefer_h: int = 4) -> Tuple[int, int]
```

### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `n_flat_heads` | `int` | required | Target number of equivalent flat heads |
| `prefer_h` | `int` | 4 | Preferred heads per layer |

### Returns

`Tuple[int, int]` - (H, L) where H^L ≈ N

### Algorithm

```python
def optimal_architecture(n_flat_heads, prefer_h=4):
    import math

    # Solve H^L = N for L
    l_exact = math.log(n_flat_heads) / math.log(prefer_h)

    # Try floor and ceiling
    l_floor = max(1, int(l_exact))
    l_ceil = l_floor + 1

    # Pick closer approximation
    equiv_floor = prefer_h ** l_floor
    equiv_ceil = prefer_h ** l_ceil

    if abs(equiv_floor - n_flat_heads) <= abs(equiv_ceil - n_flat_heads):
        return (prefer_h, l_floor)
    else:
        return (prefer_h, l_ceil)
```

### Examples

```python
optimal_architecture(18)   # → (4, 2) = 16 heads
optimal_architecture(64)   # → (4, 3) = 64 heads
optimal_architecture(100)  # → (4, 3) = 64 heads (closest)
```

### Why H=4?

Mathematical analysis shows optimal H ≈ e ≈ 2.718, but practical constraints favor powers of 2. H=4 balances:
- Parallelization efficiency (power of 2)
- Minimal total parameters (H × L)
- Close approximation to theoretical optimum

---

## ProjectionTransformer Class

Transformer architecture for LDA distillation.

### Constructor

```python
class ProjectionTransformer:
    def __init__(
        self,
        embed_dim: int,
        num_heads: int,      # H
        num_layers: int,     # L
        ff_dim: int
    )
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `embed_dim` | `int` | Input/output embedding dimension |
| `num_heads` | `int` | Attention heads per layer (H) |
| `num_layers` | `int` | Number of transformer layers (L) |
| `ff_dim` | `int` | Feed-forward hidden dimension |

### Architecture

```python
def __init__(self, embed_dim, num_heads, num_layers, ff_dim):
    # Input projection
    self.input_proj = nn.Linear(embed_dim, embed_dim)

    # Transformer encoder
    encoder_layer = nn.TransformerEncoderLayer(
        d_model=embed_dim,
        nhead=num_heads,
        dim_feedforward=ff_dim,
        activation='gelu'
    )
    self.encoder = nn.TransformerEncoder(encoder_layer, num_layers)

    # Output projection
    self.output_proj = nn.Linear(embed_dim, embed_dim)
```

### Forward Pass

```python
def forward(self, query: torch.Tensor) -> torch.Tensor:
    x = self.input_proj(query).unsqueeze(1)  # Add seq dim
    x = self.encoder(x)
    x = self.output_proj(x.squeeze(1))
    return x
```

---

## train_distillation Function

Trains transformer to match LDA outputs.

### Signature

```python
def train_distillation(
    transformer: ProjectionTransformer,
    lda_projection: MultiHeadLDA,
    query_embeddings: torch.Tensor,
    num_epochs: int = 200,
    cosine_weight: float = 0.7,
    learning_rate: float = 1e-4
) -> Dict[str, List[float]]
```

### Parameters

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `transformer` | `ProjectionTransformer` | required | Student model |
| `lda_projection` | `MultiHeadLDA` | required | Teacher model |
| `query_embeddings` | `Tensor` | required | Training data |
| `num_epochs` | `int` | 200 | Training epochs |
| `cosine_weight` | `float` | 0.7 | Weight for cosine loss (λ) |
| `learning_rate` | `float` | 1e-4 | Optimizer learning rate |

### Loss Function

```
L = (1 - λ) × MSE(pred, target) + λ × (1 - cosine_sim(pred, target))
```

**Critical**: MSE alone achieves low error but wrong direction. Cosine loss ensures directional alignment.

### Training Loop

```python
def train_distillation(transformer, lda_projection, query_embeddings,
                       num_epochs=200, cosine_weight=0.7, learning_rate=1e-4):
    optimizer = torch.optim.AdamW(transformer.parameters(), lr=learning_rate)

    # Pre-compute teacher outputs
    with torch.no_grad():
        teacher_outputs = lda_projection(query_embeddings)

    history = {'loss': [], 'mse': [], 'cosine': []}

    for epoch in range(num_epochs):
        optimizer.zero_grad()

        # Forward pass
        student_outputs = transformer(query_embeddings)

        # Compute losses
        mse_loss = F.mse_loss(student_outputs, teacher_outputs)
        cosine_loss = 1 - F.cosine_similarity(
            student_outputs, teacher_outputs, dim=-1
        ).mean()

        # Combined loss
        loss = (1 - cosine_weight) * mse_loss + cosine_weight * cosine_loss

        # Backward pass
        loss.backward()
        optimizer.step()

        history['loss'].append(loss.item())
        history['mse'].append(mse_loss.item())
        history['cosine'].append(cosine_loss.item())

    return history
```

---

## Validation Metrics

### Cosine Similarity

Primary metric for directional alignment:

```python
def evaluate_distillation(transformer, lda_projection, test_data):
    with torch.no_grad():
        teacher_out = lda_projection(test_data)
        student_out = transformer(test_data)

        cosine_sim = F.cosine_similarity(student_out, teacher_out, dim=-1)

        return {
            'mean_cosine': cosine_sim.mean().item(),
            'min_cosine': cosine_sim.min().item(),
            'max_cosine': cosine_sim.max().item(),
            'std_cosine': cosine_sim.std().item()
        }
```

### Expected Results

| Metric | Expected Value |
|--------|----------------|
| Mean Cosine Similarity | > 0.99 |
| Min Cosine Similarity | > 0.98 |
| Standard Deviation | < 0.01 |

---

## Latency Analysis

### Benchmark Code

```python
def benchmark_latency(lda, transformer, queries, batch_size=32):
    import time

    # Single query latency
    start = time.perf_counter()
    for q in queries:
        lda(q.unsqueeze(0))
    lda_single = (time.perf_counter() - start) / len(queries) * 1000

    start = time.perf_counter()
    for q in queries:
        transformer(q.unsqueeze(0))
    trans_single = (time.perf_counter() - start) / len(queries) * 1000

    # Batched latency
    batches = queries.split(batch_size)
    start = time.perf_counter()
    for batch in batches:
        lda(batch)
    lda_batch = (time.perf_counter() - start) / len(batches) * 1000

    start = time.perf_counter()
    for batch in batches:
        transformer(batch)
    trans_batch = (time.perf_counter() - start) / len(batches) * 1000

    return {
        'lda_single_ms': lda_single,
        'transformer_single_ms': trans_single,
        'lda_batch_ms': lda_batch,
        'transformer_batch_ms': trans_batch
    }
```

### Crossover Analysis

| Scenario | Slowdown at N=18 | Estimated Crossover |
|----------|------------------|---------------------|
| Single query | 24x | >400 heads |
| Batched (32) | 1.5x | ~27 heads |

---

## When to Use Each Approach

### Use Transformer When

- N > 400 heads (single query) or N > 30 (batched)
- Memory constrained (mobile/edge deployment)
- Natural domain structure for hierarchical routing
- GPU available for inference

### Stay with LDA When

- N < 400 heads (most practical cases)
- Interpretability matters (LDA weights are meaningful)
- Clusters change frequently (retraining transformer is expensive)
- CPU-only inference

---

## Command Line Usage

```bash
python3 scripts/test_transformer_distillation.py \
    --db playbooks/lda-training-data/lda.db \
    --epochs 200 \
    --cosine-weight 0.7
```

### Arguments

| Argument | Default | Description |
|----------|---------|-------------|
| `--db` | required | Path to LDA database |
| `--epochs` | 200 | Training epochs |
| `--cosine-weight` | 0.7 | Cosine loss weight |
| `--num-heads` | 4 | Heads per layer (H) |
| `--num-layers` | 2 | Number of layers (L) |
| `--embed-dim` | 384 | Embedding dimension |

---

## Complete Example

```python
from projection_transformer import ProjectionTransformer, train_distillation
from multi_head_lda import MultiHeadLDA

# Load LDA teacher (18 heads)
lda = MultiHeadLDA.load('lda.db')

# Create transformer student (4² = 16 ≈ 18)
transformer = ProjectionTransformer(
    embed_dim=384,
    num_heads=4,
    num_layers=2,
    ff_dim=512
)

# Load training data
train_data = torch.load('embeddings.pt')

# Train via distillation
history = train_distillation(
    transformer=transformer,
    lda_projection=lda,
    query_embeddings=train_data,
    num_epochs=200,
    cosine_weight=0.7
)

# Evaluate
metrics = evaluate_distillation(transformer, lda, test_data)
print(f"Mean cosine similarity: {metrics['mean_cosine']:.4f}")
```

---

## Related Documentation

- [Book 14 Chapter 5: Training Pipeline](../05_training_pipeline.md)
- [Book 14 Chapter 7: Cross-Target Deployment](../07_cross_target_deployment.md)
- [Projection Transformer Source](../../../../src/unifyweaver/ml/projection_transformer.py)
