<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 6: Transformer Distillation

**The H^L = N Equivalence and Knowledge Distillation**

## Why Distill to Transformers?

Multi-head LDA scales linearly: O(N) for N heads. At large scale (hundreds+ heads), the softmax computation becomes expensive:

```
query → softmax(query @ centroids.T / τ) → weights → weights @ answers
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        O(N) computation
```

Transformers offer logarithmic scaling through depth.

## The H^L = N Equivalence Conjecture

**Key insight**: A transformer with H attention heads per layer and L layers has routing capacity equivalent to H^L flat LDA heads.

```
Flat LDA:    N independent heads
Transformer: H heads × L layers → H^L effective routing patterns
```

### Why H^L?

Each layer routes through H attention patterns. Layers compose sequentially:

```
Layer 1: H choices
Layer 2: H choices (for each Layer 1 choice)
...
Layer L: H choices

Total combinations: H × H × ... × H = H^L
```

### Examples

| Flat LDA Heads (N) | Transformer H | Transformer L | Equivalent |
|-------------------|---------------|---------------|------------|
| 16 | 4 | 2 | 4² = 16 |
| 64 | 4 | 3 | 4³ = 64 |
| 256 | 4 | 4 | 4⁴ = 256 |
| 18 | 4 | 2 | 4² = 16 ≈ 18 |

## Optimal Architecture Selection

Given N flat heads, how to choose H and L?

**Constraint**: H^L ≈ N
**Objective**: Minimize total heads (H × L)

Mathematical analysis shows optimal H ≈ e ≈ 2.718, but practical constraints favor powers of 2. **Recommendation: H = 4**.

```python
def optimal_architecture(n_flat_heads, prefer_h=4):
    """Choose H and L for target N."""
    import math

    l_exact = math.log(n_flat_heads) / math.log(prefer_h)
    l_floor = max(1, int(l_exact))
    l_ceil = l_floor + 1

    # Pick closer approximation
    equiv_floor = prefer_h ** l_floor
    equiv_ceil = prefer_h ** l_ceil

    if abs(equiv_floor - n_flat_heads) <= abs(equiv_ceil - n_flat_heads):
        return (prefer_h, l_floor)
    else:
        return (prefer_h, l_ceil)

# Examples
optimal_architecture(18)   # → (4, 2) = 16 heads
optimal_architecture(64)   # → (4, 3) = 64 heads
optimal_architecture(100)  # → (4, 3) = 64 heads (closest)
```

## Knowledge Distillation

Train the transformer to match LDA outputs using MSE + cosine loss:

```
# Loss function (mathematical notation)
L = (1 - λ) × MSE(pred, target) + λ × (1 - cosine_sim(pred, target))

# Recommended: λ = 0.7 (cosine-weighted)
```

**Critical**: MSE alone achieves low error but wrong direction. Cosine loss ensures directional alignment.

### Training Code

```python
from projection_transformer import ProjectionTransformer, train_distillation

# Create transformer matching LDA capacity
transformer = ProjectionTransformer(
    embed_dim=384,
    num_heads=4,      # H
    num_layers=2,     # L → 4² = 16 equivalent heads
    ff_dim=512
)

# Train via distillation
train_distillation(
    transformer=transformer,
    lda_projection=lda,           # Teacher (multi-head LDA)
    query_embeddings=train_data,
    num_epochs=200,
    cosine_weight=0.7             # Essential!
)
```

## Validation Results

Testing H^L = N equivalence (N=18 LDA heads, H=4, L=2):

| Metric | Result |
|--------|--------|
| Mean Cosine Similarity | 0.9928 ± 0.0027 |
| Min Cosine Similarity | 0.9827 |
| Max Cosine Similarity | 0.9954 |

**99.28% cosine similarity** validates the equivalence conjecture.

## Latency Analysis

Benchmark at N=18 heads (1000 queries):

| Method | Single Query | Batch (32) |
|--------|-------------|------------|
| LDA (NumPy CPU) | 0.046 ms | 0.046 ms |
| Transformer (CUDA) | 1.110 ms | 0.069 ms |

### Crossover Analysis

| Scenario | Slowdown at N=18 | Estimated Crossover |
|----------|------------------|---------------------|
| Single query | 24x | >400 heads (lower bound) |
| Batched (32) | 1.5x | ~27 heads |

**Key insight**: At small scale, LDA wins due to GPU kernel overhead. Transformer benefits emerge at larger scale where O(N) softmax becomes expensive.

## When to Use Transformer Distillation

**Use transformer when:**
- N > 400 heads (single query) or N > 30 (batched)
- Memory constrained (mobile/edge)
- Natural domain structure for hierarchical routing

**Stay with LDA when:**
- N < 400 heads (most cases)
- Interpretability matters (LDA weights are meaningful)
- Clusters change frequently (retraining is expensive)

## The ProjectionTransformer Architecture

```python
class ProjectionTransformer:
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

    def forward(self, query):
        x = self.input_proj(query).unsqueeze(1)  # Add seq dim
        x = self.encoder(x)
        x = self.output_proj(x.squeeze(1))
        return x
```

## Practical Exercise

Test transformer distillation:

```bash
python3 scripts/test_transformer_distillation.py \
    --db playbooks/lda-training-data/lda.db \
    --epochs 200 \
    --cosine-weight 0.7
```

Experiment with different architectures:

```python
# Try different H and L
for h in [2, 4, 8]:
    for l in [1, 2, 3]:
        equiv = h ** l
        print(f"H={h}, L={l} → {equiv} equivalent heads")
```

## Chapter Summary

- H^L = N conjecture: transformer capacity equals H^L flat LDA heads
- Optimal H ≈ 4 for practical efficiency
- Cosine loss is essential (MSE alone fails directionally)
- Validated: 99.28% cosine similarity on test set
- Crossover: >400 heads (single) or ~27 heads (batched)
- At small scale, LDA is faster; transformers win at large scale

## Next Chapter

[Chapter 7: Cross-Target Deployment](07_cross_target_deployment.md) - Train in Python, deploy in Rust/Go.
