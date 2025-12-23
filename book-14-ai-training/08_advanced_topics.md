<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 8: Advanced Topics

**Regularization, Hierarchical Ensembles, and Future Directions**

## Smoothness Regularization

When clusters have few training examples, projections can overfit. **Smoothness regularization** encourages similar clusters to have similar projections.

### The Graph Laplacian Approach

Build a similarity graph between clusters, then penalize projection differences:

```
# Cluster similarity graph (mathematical notation)
S[i,j] = cosine_sim(centroid_i, centroid_j)

# Graph Laplacian
D = diag(sum(S, axis=1))  # Degree matrix
L = D - S                  # Laplacian

# Regularized loss
Loss = MSE(pred, target) + λ * sum_ij S[i,j] * ||W_i - W_j||²
     = MSE(pred, target) + λ * trace(W^T L W)
```

This encourages nearby clusters (in embedding space) to have similar answer projections.

### When to Use

- Clusters with < 5 training questions
- High variance in per-cluster performance
- Domain knowledge suggests cluster similarity

### Implementation Status

See `docs/proposals/SMOOTHNESS_REGULARIZATION.md` for the full proposal. Implementation is planned for a future release.

## Hierarchical Transformer Ensembles

For very large cluster counts, use multiple transformers:

```
                ┌─────────────────┐
                │  Meta-Router    │
                │  (small MLP)    │
                └────────┬────────┘
                         │
       ┌─────────────────┼─────────────────┐
       ▼                 ▼                 ▼
┌──────────────┐  ┌──────────────┐  ┌──────────────┐
│ Transformer  │  │ Transformer  │  │ Transformer  │
│   Domain A   │  │   Domain B   │  │   Domain C   │
│ (clusters    │  │ (clusters    │  │ (clusters    │
│  1-100)      │  │  101-200)    │  │  201-300)    │
└──────────────┘  └──────────────┘  └──────────────┘
```

### Domain Assignment

Clusters are assigned to domains via:

1. **K-means on centroids**: Automatic grouping
2. **Manual labels**: "auth", "data", "performance"
3. **Hierarchical clustering**: Build tree, cut at desired level

### Meta-Router

Small MLP predicting which domain(s) to invoke:

```python
domain_weights = softmax(meta_router(query))  # [0.8, 0.15, 0.05]
projected = sum(domain_weights[d] * transformer_d(query) for d in domains)
```

For sparse routing, use top-k selection.

### Training Process

1. Pre-train domain transformers (each on its cluster subset)
2. Train meta-router (on full query set)
3. Optional: fine-tune end-to-end

## Asymmetric Embedding Models

Some models provide separate query vs document encodings:

```python
# nomic-embed-text-v1.5 with prefixes
query_emb = model.encode("search_query: How do I log in?")
doc_emb = model.encode("search_document: Authentication requires...")
```

### Benefits

- Model learns query-document relationship directly
- No need for learned projection (in theory)
- Often higher quality for retrieval

### Integration with LDA

Even with asymmetric models, LDA can improve:

1. Use query prefix for questions, document prefix for answers
2. Train multi-head on the asymmetric embeddings
3. Projection learns *domain-specific* refinements on top

## Scaling Strategies Summary

| Scale | Strategy |
|-------|----------|
| Moderate (<100 heads) | Flat LDA multi-head |
| Large (100-500 heads) | Consider transformer distillation |
| Very Large (500-2000) | Hierarchical ensemble |
| Massive (2000+) | ANN + hierarchical + sparse routing |

## Future Directions

### 1. Online Learning

Update projections incrementally as new Q-A pairs arrive:

```python
# Current: full retraining
train_multi_head(all_data)

# Future: incremental update
update_multi_head(new_data, existing_model)
```

### 2. Mixture of Experts (MoE)

Sparse routing where only k experts are activated:

```python
# Dense routing (current)
weights = softmax(similarities)  # All heads contribute

# Sparse routing (MoE)
top_k = topk(similarities, k=2)
weights = softmax(top_k)         # Only top-2 heads
```

### 3. Contrastive Training

Learn embeddings directly for Q-A matching:

```python
# Contrastive loss
pos_sim = sim(question, correct_answer)
neg_sim = sim(question, wrong_answer)
loss = -log(exp(pos_sim) / (exp(pos_sim) + exp(neg_sim)))
```

### 4. Cross-Lingual Projection

Learn projections that work across languages:

```
English Q → Projection → English A
French Q  → Projection → French A
German Q  → Projection → German A
```

Using multilingual embedding models as the base.

### 5. Graph-Aware Routing

Use knowledge graph structure for routing:

```
Query about "authentication" →
  Graph neighbors: ["JWT", "OAuth", "sessions"] →
  Route to auth-related clusters
```

## Research Questions

Open questions we're exploring:

1. **Capacity bounds**: Is H^L exactly the capacity, or an upper bound?
2. **Optimal temperature**: Theory for choosing τ given cluster structure?
3. **Transfer learning**: Can projections transfer across domains?
4. **Interpretability**: Can we understand what each head learns?

## Contributing

Areas where contributions are welcome:

- [ ] Smoothness regularization implementation
- [ ] Hierarchical ensemble prototype
- [ ] Online learning experiments
- [ ] Cross-lingual projection tests
- [ ] Benchmark on larger datasets

See `CONTRIBUTING.md` in the main repository.

## Chapter Summary

- Smoothness regularization helps sparse clusters
- Hierarchical ensembles scale to thousands of heads
- Asymmetric models can be combined with LDA
- Future: online learning, MoE, contrastive training, cross-lingual
- Many open research questions remain

## Book Summary

You've learned:

1. **Embeddings**: How text becomes vectors (Ch. 2)
2. **LDA Projection**: Mapping questions to answers (Ch. 3)
3. **Multi-Head**: Domain-specific routing (Ch. 4)
4. **Training Pipeline**: Database and batch management (Ch. 5)
5. **Transformer Distillation**: H^L = N compression (Ch. 6)
6. **Cross-Target**: Train Python, deploy anywhere (Ch. 7)
7. **Advanced Topics**: Regularization and future work (Ch. 8)

You're now equipped to train custom AI models for semantic search with UnifyWeaver!

## Further Reading

- `docs/proposals/SEMANTIC_PROJECTION_LDA.md`
- `docs/proposals/MULTI_HEAD_PROJECTION_THEORY.md`
- `docs/proposals/TRANSFORMER_DISTILLATION.md`
- `docs/proposals/SMOOTHNESS_REGULARIZATION.md`

## Acknowledgments

This book covers work developed collaboratively with AI assistance from Claude (Anthropic), demonstrating human-AI collaboration in technical documentation and implementation.
