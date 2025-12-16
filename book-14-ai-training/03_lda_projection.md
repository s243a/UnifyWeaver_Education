<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: LDA Semantic Projection

**Learning to Map Questions to Answers**

## The Asymmetry Problem Revisited

Recall from Chapter 1: users ask questions differently than documents provide answers. Direct cosine similarity between query and document embeddings often fails.

```
Query:    "How do I read a CSV file?"
Document: "The pandas.read_csv() function loads comma-separated..."

Direct similarity: 0.45 (mediocre)
After projection:  0.89 (much better)
```

## The W Matrix

We learn a projection matrix W that transforms question embeddings to be closer to their corresponding answer embeddings:

```
projected_query = query_embedding @ W
```

Where:
- `query_embedding`: shape (d,) - e.g., 384 dimensions
- `W`: shape (d, d) - the learned projection
- `projected_query`: shape (d,) - now closer to answer space

## Computing W: The Pseudoinverse Approach

Given training pairs (question_i, answer_i):

```
Q = stack of question embeddings  (n × d)
A = stack of answer embeddings    (n × d)

Goal: Find W such that Q @ W ≈ A

Solution: W = pinv(Q) @ A
```

Where `pinv` is the Moore-Penrose pseudoinverse.

### Why Pseudoinverse?

The pseudoinverse provides the least-squares solution:
- Minimizes ||Q @ W - A||² (reconstruction error)
- Works even when Q is not square or full rank
- Numerically stable with regularization

### Python Implementation

```python
import numpy as np

def compute_W(question_embeddings, answer_embeddings, regularization=1e-6):
    """
    Compute projection matrix W using regularized pseudoinverse.

    Args:
        question_embeddings: (n, d) array of question vectors
        answer_embeddings: (n, d) array of answer vectors
        regularization: small value for numerical stability

    Returns:
        W: (d, d) projection matrix
    """
    Q = question_embeddings
    A = answer_embeddings

    # Regularized pseudoinverse: (Q^T Q + λI)^-1 Q^T
    d = Q.shape[1]
    QTQ = Q.T @ Q + regularization * np.eye(d)
    W = np.linalg.solve(QTQ, Q.T @ A)

    return W
```

## Training Data Format

Training data consists of Q-A clusters:

```json
{
  "clusters": [
    {
      "answer": "The pandas.read_csv() function loads CSV files...",
      "answer_id": "doc_csv_001",
      "questions": [
        "How do I read a CSV file?",
        "Loading CSV in pandas",
        "Parse comma-separated values"
      ]
    },
    {
      "answer": "Authentication uses JWT tokens...",
      "answer_id": "doc_auth_001",
      "questions": [
        "How do I log in?",
        "What's the auth process?",
        "User authentication"
      ]
    }
  ]
}
```

Each cluster has:
- One **answer** (the document/response)
- Multiple **questions** (different ways users might ask)

## Validation: Does It Work?

Compare retrieval with and without projection:

```python
def validate_projection(W, test_queries, corpus, embedder):
    """Compare projected vs direct retrieval."""

    results = {'projected': [], 'direct': []}

    for query, expected_answer_id in test_queries:
        query_emb = embedder.encode(query)

        # Direct similarity
        direct_scores = query_emb @ corpus_embeddings.T
        direct_top = corpus_ids[np.argmax(direct_scores)]
        results['direct'].append(direct_top == expected_answer_id)

        # Projected similarity
        projected = query_emb @ W
        proj_scores = projected @ corpus_embeddings.T
        proj_top = corpus_ids[np.argmax(proj_scores)]
        results['projected'].append(proj_top == expected_answer_id)

    print(f"Direct Recall@1:    {np.mean(results['direct']):.1%}")
    print(f"Projected Recall@1: {np.mean(results['projected']):.1%}")
```

Typical results:
```
Direct Recall@1:    70.0%
Projected Recall@1: 93.3%
```

## The Single-W Limitation

A single W matrix assumes one global mapping from questions to answers. This works well when:
- Questions are semantically similar
- There's one dominant pattern

It struggles when:
- Questions span diverse topics
- Different question types need different mappings

This limitation motivates **multi-head routing** (Chapter 4).

## Practical Exercise

Train a projection on the sample data:

```bash
# Train W matrix
python3 scripts/train_lda_projection.py \
    --input playbooks/lda-training-data/raw/qa_pairs_v1.json \
    --model all-MiniLM-L6-v2 \
    --output playbooks/lda-training-data/trained/all-MiniLM-L6-v2/W_matrix.npy

# Validate on held-out queries
python3 scripts/validate_lda_projection.py
```

## Chapter Summary

- The W matrix learns to project questions toward answer space
- Computed via regularized pseudoinverse: W = pinv(Q) @ A
- Significantly improves retrieval accuracy (70% → 93% in tests)
- Single W assumes uniform question-answer mapping
- Diverse domains need multi-head routing (next chapter)

## Next Chapter

[Chapter 4: Multi-Head Routing](04_multi_head_routing.md) - Handling diverse question types with cluster-specific projections.
