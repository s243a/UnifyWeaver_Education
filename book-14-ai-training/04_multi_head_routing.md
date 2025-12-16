<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 4: Multi-Head Routing

**Cluster-Specific Projections with Soft Attention**

## Beyond Single Projection

Chapter 3's single W matrix assumes all questions map to answers the same way. Reality is messier:

- "How do I authenticate?" → needs auth-domain knowledge
- "What's the CSV format?" → needs data-domain knowledge
- "Why is my query slow?" → needs performance-domain knowledge

Different question *types* benefit from different projections.

## The Multi-Head Approach

Instead of one global projection, we create **multiple heads**, each specialized for a cluster of related questions:

```
┌─────────────────────────────────────────────────┐
│                    Query                         │
└─────────────────────┬───────────────────────────┘
                      │
        ┌─────────────┼─────────────┐
        ▼             ▼             ▼
   ┌─────────┐   ┌─────────┐   ┌─────────┐
   │ Head 1  │   │ Head 2  │   │ Head 3  │
   │ (Auth)  │   │ (Data)  │   │ (Perf)  │
   └────┬────┘   └────┬────┘   └────┬────┘
        │             │             │
        └─────────────┼─────────────┘
                      ▼
              Weighted Combination
                      │
                      ▼
               Projected Query
```

## Anatomy of a Head

Each head stores:

1. **Centroid**: Mean embedding of questions in this cluster
2. **Answer embedding**: The answer for this cluster

```python
head = {
    'centroid': np.mean(question_embeddings, axis=0),  # (d,)
    'answer_emb': answer_embedding                      # (d,)
}
```

## Soft Routing via Softmax

Given a query, we compute similarity to each head's centroid, then softmax to get routing weights:

```python
def route_query(query_emb, heads, temperature=0.1):
    """Route query to heads with soft attention."""

    # Normalize query
    query_norm = query_emb / np.linalg.norm(query_emb)

    # Compute similarities to each centroid
    similarities = []
    for head in heads:
        centroid_norm = head['centroid'] / np.linalg.norm(head['centroid'])
        sim = np.dot(query_norm, centroid_norm)
        similarities.append(sim)

    similarities = np.array(similarities)

    # Temperature-controlled softmax
    scaled = similarities / temperature
    exp_scaled = np.exp(scaled - np.max(scaled))  # stability
    weights = exp_scaled / np.sum(exp_scaled)

    return weights
```

## Temperature: Sharp vs Soft Routing

The temperature τ controls routing sharpness:

| Temperature | Behavior | Use Case |
|-------------|----------|----------|
| τ = 0.01 | Nearly hard routing (winner-take-all) | Distinct clusters |
| τ = 0.1 | Sharp but soft | Recommended default |
| τ = 1.0 | Soft blending | Overlapping clusters |
| τ = 10.0 | Nearly uniform | Regularization |

```python
# Sharp routing (τ=0.1): weights = [0.85, 0.10, 0.05]
# Soft routing (τ=1.0):  weights = [0.45, 0.30, 0.25]
```

## Computing the Projected Embedding

The final projection is a weighted combination of answer embeddings:

```python
def project_multi_head(query_emb, heads, temperature=0.1):
    """Project query using multi-head routing."""

    weights = route_query(query_emb, heads, temperature)

    # Weighted combination of answer embeddings
    projected = np.zeros_like(query_emb)
    for i, head in enumerate(heads):
        projected += weights[i] * head['answer_emb']

    return projected
```

## Connection to Transformer Attention

Multi-head LDA is structurally identical to attention:

| LDA Multi-Head | Transformer Attention |
|----------------|----------------------|
| Centroids | Keys (K) |
| Answer embeddings | Values (V) |
| Query embedding | Query (Q) |
| softmax(sim/τ) | softmax(QK^T/√d) |
| Σ weights × answers | Attention output |

This isn't coincidence—both are learned routing mechanisms!

## Training Multi-Head Projection

```bash
python3 scripts/train_multi_head_projection.py \
    --db playbooks/lda-training-data/lda.db \
    --model all-MiniLM-L6-v2 \
    --temperature 0.1 \
    --validate
```

The training process:
1. Load Q-A clusters from database
2. Embed all questions and answers
3. Compute centroid for each cluster
4. Store heads (centroid + answer_emb) to database

## Validation Results

Testing on novel queries (not in training):

```
Multi-head (τ=0.1): 76.7% Recall@1
Direct similarity:  70.0% Recall@1
Improvement:        +6.7%
```

Multi-head outperforms direct similarity by routing queries to domain-appropriate answers.

## Visualizing Routing

```python
def visualize_routing(query, heads, temperature=0.1):
    """Show which heads a query routes to."""

    query_emb = embedder.encode(query)
    weights = route_query(query_emb, heads, temperature)

    print(f"Query: {query}")
    for i, (head, weight) in enumerate(zip(heads, weights)):
        bar = "█" * int(weight * 40)
        print(f"  Head {i}: {weight:.2f} {bar}")
```

Output:
```
Query: How do I authenticate users?
  Head 0 (Auth):  0.82 █████████████████████████████████
  Head 1 (Data):  0.12 █████
  Head 2 (Perf):  0.06 ██
```

## Practical Exercise

Experiment with temperature:

```python
for temp in [0.01, 0.1, 0.5, 1.0]:
    weights = route_query(query_emb, heads, temperature=temp)
    print(f"τ={temp}: {weights}")
```

Observe how routing sharpness changes.

## Chapter Summary

- Multi-head routing assigns queries to specialized heads
- Each head has a centroid (routing target) and answer embedding (output)
- Temperature controls routing sharpness (τ=0.1 recommended)
- Structurally equivalent to transformer attention
- Improves over single projection for diverse domains

## Next Chapter

[Chapter 5: Training Pipeline](05_training_pipeline.md) - Database schema and infrastructure for managing training data.
