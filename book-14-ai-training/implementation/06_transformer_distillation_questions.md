<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 6: Transformer Distillation - Questions

Q&A companion for [06_transformer_distillation_impl.md](./06_transformer_distillation_impl.md).

---

## Question Index

1. [What is the H^L = N equivalence?](#b14c06-q-hl-equivalence)
2. [Why is H=4 recommended?](#b14c06-q-why-h4)
3. [What does optimal_architecture do?](#b14c06-q-optimal-architecture)
4. [What is the ProjectionTransformer architecture?](#b14c06-q-projection-transformer)
5. [What is the loss function for distillation?](#b14c06-q-loss-function)
6. [Why is cosine loss essential?](#b14c06-q-why-cosine)
7. [What does train_distillation do?](#b14c06-q-train-distillation)
8. [What validation metrics should I expect?](#b14c06-q-validation-metrics)
9. [What is the latency crossover point?](#b14c06-q-latency-crossover)
10. [When should I use transformer vs LDA?](#b14c06-q-when-transformer)
11. [How do I run transformer distillation?](#b14c06-q-run-distillation)
12. [What are the key hyperparameters?](#b14c06-q-hyperparameters)

---

## Questions and Answers

### <a id="b14c06-q-hl-equivalence"></a>Q1: What is the H^L = N equivalence?

**Answer**: A transformer with H attention heads per layer and L layers has routing capacity equivalent to H^L flat LDA heads:

| H | L | Equivalent Flat Heads |
|---|---|----------------------|
| 4 | 2 | 4² = 16 |
| 4 | 3 | 4³ = 64 |
| 4 | 4 | 4⁴ = 256 |

Each layer routes through H attention patterns, and layers compose sequentially: H × H × ... = H^L combinations.

**See**: [Overview: H^L = N Equivalence](./06_transformer_distillation_impl.md#overview-hl--n-equivalence)

---

### <a id="b14c06-q-why-h4"></a>Q2: Why is H=4 recommended?

**Answer**: Mathematical analysis shows optimal H ≈ e ≈ 2.718, but practical constraints favor powers of 2. H=4 balances:

1. **Parallelization efficiency** - Power of 2 fits GPU architectures
2. **Minimal parameters** - H × L is minimized
3. **Close to theoretical optimum** - Good approximation

**See**: [Why H=4?](./06_transformer_distillation_impl.md#why-h4)

---

### <a id="b14c06-q-optimal-architecture"></a>Q3: What does optimal_architecture do?

**Answer**: Selects H and L given target N flat heads:

```python
def optimal_architecture(n_flat_heads, prefer_h=4):
    # Solve H^L = N for L
    l_exact = math.log(n_flat_heads) / math.log(prefer_h)
    # Pick floor or ceiling based on closer approximation
    ...
    return (H, L)
```

Examples:
- `optimal_architecture(18)` → (4, 2) = 16 heads
- `optimal_architecture(100)` → (4, 3) = 64 heads

**See**: [optimal_architecture Function](./06_transformer_distillation_impl.md#optimal_architecture-function)

---

### <a id="b14c06-q-projection-transformer"></a>Q4: What is the ProjectionTransformer architecture?

**Answer**: A transformer encoder with:

1. **Input projection** - Linear layer (embed_dim → embed_dim)
2. **Transformer encoder** - H heads × L layers with GELU activation
3. **Output projection** - Linear layer (embed_dim → embed_dim)

```python
transformer = ProjectionTransformer(
    embed_dim=384,
    num_heads=4,      # H
    num_layers=2,     # L
    ff_dim=512
)
```

**See**: [ProjectionTransformer Class](./06_transformer_distillation_impl.md#projectiontransformer-class)

---

### <a id="b14c06-q-loss-function"></a>Q5: What is the loss function for distillation?

**Answer**: Combined MSE and cosine loss:

```
L = (1 - λ) × MSE(pred, target) + λ × (1 - cosine_sim(pred, target))
```

Recommended: λ = 0.7 (cosine-weighted)

**See**: [Loss Function](./06_transformer_distillation_impl.md#loss-function)

---

### <a id="b14c06-q-why-cosine"></a>Q6: Why is cosine loss essential?

**Answer**: MSE alone achieves low error but **wrong direction**. A prediction with low MSE might still point in a different direction than the target.

Cosine loss ensures **directional alignment** - the transformer learns to output vectors pointing the same direction as LDA outputs, not just vectors with similar magnitudes.

**See**: [Loss Function](./06_transformer_distillation_impl.md#loss-function)

---

### <a id="b14c06-q-train-distillation"></a>Q7: What does train_distillation do?

**Answer**: Trains the transformer to match LDA outputs:

```python
history = train_distillation(
    transformer=transformer,      # Student
    lda_projection=lda,          # Teacher
    query_embeddings=train_data,
    num_epochs=200,
    cosine_weight=0.7
)
```

Returns training history with loss, MSE, and cosine metrics per epoch.

**See**: [train_distillation Function](./06_transformer_distillation_impl.md#train_distillation-function)

---

### <a id="b14c06-q-validation-metrics"></a>Q8: What validation metrics should I expect?

**Answer**: With proper training (N=18, H=4, L=2):

| Metric | Expected Value |
|--------|----------------|
| Mean Cosine Similarity | > 0.99 |
| Min Cosine Similarity | > 0.98 |
| Standard Deviation | < 0.01 |

**99.28% cosine similarity** validates the H^L = N conjecture.

**See**: [Validation Metrics](./06_transformer_distillation_impl.md#validation-metrics)

---

### <a id="b14c06-q-latency-crossover"></a>Q9: What is the latency crossover point?

**Answer**: At N=18 heads:

| Scenario | Slowdown | Crossover Point |
|----------|----------|-----------------|
| Single query | 24x slower | >400 heads |
| Batched (32) | 1.5x slower | ~27 heads |

At small scale, LDA wins due to GPU kernel overhead. Transformer benefits emerge at larger scale.

**See**: [Latency Analysis](./06_transformer_distillation_impl.md#latency-analysis)

---

### <a id="b14c06-q-when-transformer"></a>Q10: When should I use transformer vs LDA?

**Answer**:

**Use Transformer when:**
- N > 400 heads (single) or N > 30 (batched)
- Memory constrained (mobile/edge)
- GPU available

**Stay with LDA when:**
- N < 400 heads (most cases)
- Interpretability matters
- Clusters change frequently
- CPU-only inference

**See**: [When to Use Each Approach](./06_transformer_distillation_impl.md#when-to-use-each-approach)

---

### <a id="b14c06-q-run-distillation"></a>Q11: How do I run transformer distillation?

**Answer**: Use the command line script:

```bash
python3 scripts/test_transformer_distillation.py \
    --db playbooks/lda-training-data/lda.db \
    --epochs 200 \
    --cosine-weight 0.7
```

Or programmatically:
```python
from projection_transformer import ProjectionTransformer, train_distillation
transformer = ProjectionTransformer(384, 4, 2, 512)
train_distillation(transformer, lda, data)
```

**See**: [Command Line Usage](./06_transformer_distillation_impl.md#command-line-usage)

---

### <a id="b14c06-q-hyperparameters"></a>Q12: What are the key hyperparameters?

**Answer**:

| Parameter | Default | Description |
|-----------|---------|-------------|
| `num_epochs` | 200 | Training epochs |
| `cosine_weight` | 0.7 | λ in loss function |
| `learning_rate` | 1e-4 | Optimizer LR |
| `num_heads` | 4 | H (heads per layer) |
| `num_layers` | 2 | L (number of layers) |
| `ff_dim` | 512 | Feed-forward dimension |

**See**: [train_distillation Function](./06_transformer_distillation_impl.md#train_distillation-function)

---

## Summary

Transformer distillation provides:
- H^L = N equivalence for architecture selection
- Combined MSE + cosine loss for directional alignment
- 99%+ cosine similarity with proper training
- Crossover at >400 heads (single) or ~27 (batched)
