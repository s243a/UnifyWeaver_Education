<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 11: Adversarial Robustness

*Protecting federated search from malicious nodes*

## Overview

In open or semi-trusted environments, malicious nodes can manipulate federated search results. This chapter covers defense mechanisms inspired by Freenet's KSK system and the FMS web of trust.

## The Problem

Federated semantic search assumes honest nodes. Attackers can exploit this through:

| Attack | Description | Impact |
|--------|-------------|--------|
| **Density Inflation** | Inject similar embeddings | Artificial high-density clusters |
| **Consensus Manipulation** | Echo other nodes' results | Inflated agreement scores |
| **Sybil Attacks** | Create many fake nodes | Dominate voting |
| **Latency Attacks** | Slow responses | Bias adaptive-k calculations |

## Two Types of Collision Detection

The defense strategy uses two complementary approaches:

```
Query Results
     │
     ▼
┌─────────────────────────────┐
│  Soft Collision Detection   │  ← Statistical outliers rejected gradually
│  (Output Smoothing)         │     (Z-score, MAD, IQR methods)
└─────────────────────────────┘
     │
     ▼
┌─────────────────────────────┐
│  Hard Collision Detection   │  ← Different answers in locked regions
│  (KSK-style)                │     rejected outright
└─────────────────────────────┘
     │
     ▼
  Filtered Results
```

### Soft Collisions (Outliers)

Statistical outliers are gradually rejected:

```python
from adversarial_robustness import smooth_outliers, OutlierSmoother
import numpy as np

# Scores from federated query (one outlier)
scores = np.array([0.8, 0.85, 0.82, 0.79, 0.81, 5.0])  # 5.0 is suspicious

# MAD method (most robust to multiple outliers)
mask = smooth_outliers(scores, method="mad", threshold=2.5)
# mask = [True, True, True, True, True, False]

# Or use the class-based interface
smoother = OutlierSmoother(method="mad", threshold=2.5)
# Filter result objects
filtered_results = smoother.filter_results(results, score_key="combined_score")
```

**Methods:**

| Method | Formula | Best For |
|--------|---------|----------|
| Z-score | `(x - mean) / std` | Normal distributions |
| IQR | `Q1 - 1.5*IQR, Q3 + 1.5*IQR` | Skewed distributions |
| MAD | `0.6745 * (x - median) / MAD` | Adversarial scenarios |

### Hard Collisions (KSK-style)

Inspired by Freenet's Keyword-Signed Keys:

> "There is voluntary collision detection in fred, which tries to prevent overwriting of a once-inserted page."

Once an answer is established with high confidence in a semantic region, new conflicting answers are rejected:

```python
from adversarial_robustness import SemanticCollisionDetector
import numpy as np

detector = SemanticCollisionDetector(
    lock_threshold=0.8,      # Confidence to lock
    min_nodes_to_lock=3,     # Quorum
    region_granularity=100   # Embedding quantization
)

embedding = np.array([0.1, 0.2, 0.3, 0.4])

# First answer establishes in region
detector.register_result(
    answer_hash="hash_correct",
    answer_text="Paris is the capital of France",
    embedding=embedding,
    confidence=0.9,
    node_count=5
)

# Region is now locked
print(detector.is_locked(embedding))  # True

# Conflicting answer is rejected
allowed, reason = detector.check_collision("hash_wrong", embedding)
print(allowed)  # False
print(reason)   # "Collision with established answer in region..."

# Same answer is allowed (reinforces consensus)
allowed, _ = detector.check_collision("hash_correct", embedding)
print(allowed)  # True
```

## Consensus Voting

Multiple nodes must agree before locking a region:

```python
from adversarial_robustness import ConsensusCollisionDetector

detector = ConsensusCollisionDetector(
    quorum=3,               # Minimum votes to lock
    supersede_margin=2,     # Extra votes to override
    lock_threshold=0.8,
    region_granularity=100
)

embedding = np.array([0.1, 0.2, 0.3])

# Node 1 votes for answer A
detector.register_vote("hash_A", "Answer A", embedding, "node_1")

# Node 2 votes for answer A
detector.register_vote("hash_A", "Answer A", embedding, "node_2")

# Not locked yet (only 2 votes, need 3)
print(detector.is_locked(embedding))  # False

# Node 3 votes for answer A - reaches quorum!
detector.register_vote("hash_A", "Answer A", embedding, "node_3")
print(detector.is_locked(embedding))  # True

# To override, need quorum + supersede_margin (3 + 2 = 5 votes)
for i in range(5):
    detector.register_vote("hash_B", "Answer B", embedding, f"other_{i}")

# Now answer B has superseded
established = detector.get_established(embedding)
print(established.answer_hash)  # "hash_B"
```

## Trust Management

### Direct Trust

Track per-node reliability:

```python
from adversarial_robustness import DirectTrustManager

trust = DirectTrustManager(default_trust=0.5, ema_weight=0.1)

# Successful verification increases trust
trust.update_trust("reliable_node", success=True)
trust.update_trust("reliable_node", success=True)
print(trust.get_trust("reliable_node"))  # ~0.6

# Failed verification decreases trust
trust.update_trust("sketchy_node", success=False)
trust.update_trust("sketchy_node", success=False)
print(trust.get_trust("sketchy_node"))  # ~0.4

# Weight responses by trust
weighted = trust.weight_responses([
    ("reliable_node", response1),
    ("sketchy_node", response2),
])
# [(response1, 0.6), (response2, 0.4)]
```

### FMS-style Two-Dimensional Trust

From Freenet Messaging System - separate trust dimensions:

```python
from adversarial_robustness import TwoDimensionalTrust

trust = TwoDimensionalTrust(
    message_trust=80,      # Trust for content quality
    trust_list_trust=40    # Trust for judgment about others
)

# A node might return good results but have poor judgment
# about which other nodes to trust (or vice versa)

print(trust.effective_trust())  # 60 (average)
print(trust.normalized_message_trust())  # 0.9 (0-1 scale)
```

### Trust-Weighted Consensus

Combine trust with collision detection:

```python
from adversarial_robustness import (
    DirectTrustManager,
    TrustWeightedConsensusDetector
)

# Build trust for some nodes
trust = DirectTrustManager(default_trust=0.5)
for _ in range(10):
    trust.update_trust("trusted_node_1", success=True)
    trust.update_trust("trusted_node_2", success=True)

# Trust-weighted consensus detector
detector = TrustWeightedConsensusDetector(
    trust_manager=trust,
    quorum=3,
    trust_quorum_factor=0.5  # Trust-based quorum is lower
)

embedding = np.array([0.1, 0.2, 0.3])

# Two trusted votes can lock (trust ~0.9 each = ~1.8 > 1.5 threshold)
detector.register_vote("hash_A", "Answer A", embedding, "trusted_node_1")
detector.register_vote("hash_A", "Answer A", embedding, "trusted_node_2")

print(detector.is_locked(embedding))  # True!

# Many untrusted nodes can't easily override
for i in range(5):
    detector.register_vote("hash_B", "Answer B", embedding, f"untrusted_{i}")

# Still answer A (trusted nodes have more weight)
print(detector.get_established(embedding).answer_hash)  # "hash_A"
```

## Integration with Federated Query

Enable adversarial protection in the query engine:

```python
from federated_query import (
    FederatedQueryEngine,
    AggregationConfig,
    AdversarialConfig
)

config = AggregationConfig(
    adversarial=AdversarialConfig(
        # Soft collision detection
        outlier_rejection=True,
        outlier_method="mad",
        outlier_threshold=2.5,

        # Hard collision detection
        collision_detection=True,
        collision_quorum=3,
        collision_supersede_margin=2,

        # Trust weighting
        trust_enabled=True,
        trust_default=0.5,
        trust_quorum_factor=0.5
    )
)

engine = FederatedQueryEngine(router, aggregation_config=config)

# Query automatically applies adversarial protection
response = engine.federated_query(
    query_text="What is the capital of France?",
    query_embedding=embedding,
    top_k=5
)

# Check adversarial stats
stats = engine.get_adversarial_stats()
print(stats)
# {
#   "enabled": True,
#   "outlier_smoother": {"method": "mad", "threshold": 2.5},
#   "collision_detector": {"total_regions": 10, "locked_regions": 3},
#   "trust_manager": {"node_count": 5, "avg_trust": 0.7}
# }
```

## Prolog Configuration

```prolog
service(secure_kg_node, [
    transport(http('/kg', [port(8080)])),

    federation([
        aggregation_strategy(sum),
        federation_k(5),

        % Phase 6f: Adversarial robustness
        adversarial_protection([
            % Soft collision detection
            outlier_rejection(enabled),
            outlier_method(mad),
            outlier_threshold(2.5),

            % Hard collision detection
            collision_detection([
                quorum(3),
                supersede_margin(2),
                lock_threshold(0.8),
                trust_weighted(true)
            ]),

            % Trust model
            trust_model(direct),
            default_trust(0.5),
            min_trust_score(10)
        ])
    ])
], Handler).
```

Validate configuration:

```prolog
?- is_valid_federation_option(adversarial_protection([
       outlier_method(mad),
       trust_model(fms),
       collision_detection([quorum(3)])
   ])).
true.

?- is_valid_outlier_method(mad).
true.

?- is_valid_trust_model(fms).
true.
```

## Defense Layers

The full defense stack:

```
┌─────────────────────────────────────────────────────────────┐
│                      Query Arrives                          │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│  Layer 1: Node Discovery (Kleinberg routing)                │
│  - Route to semantically similar nodes                      │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│  Layer 2: Parallel Query                                    │
│  - Query federation_k nodes concurrently                    │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│  Layer 3: Aggregation (SUM, MAX, DIVERSITY_WEIGHTED)        │
│  - Merge results with pluggable strategy                    │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│  Layer 4: Soft Collision Detection                          │
│  - Reject statistical outliers (MAD, Z-score)               │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│  Layer 5: Hard Collision Detection                          │
│  - Protect established consensus in semantic regions        │
│  - Trust-weighted voting                                    │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│  Layer 6: Consensus Threshold                               │
│  - Require minimum node agreement                           │
└─────────────────────────────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────┐
│                    Filtered Results                         │
└─────────────────────────────────────────────────────────────┘
```

## Summary

This chapter covered:

1. **Soft collisions (outliers):** Statistical outlier rejection using Z-score, MAD, IQR
2. **Hard collisions (KSK-style):** Semantic region locking based on Freenet's approach
3. **Consensus voting:** Quorum-based locking with supersede margin
4. **Trust management:** Direct trust with EMA updates, FMS-style two-dimensional trust
5. **Trust-weighted consensus:** High-trust nodes lock faster, Sybil-resistant
6. **Integration:** AdversarialConfig in FederatedQueryEngine

## Next Steps

- Phase 6b: Performance Benchmarking
- Advanced trust: Transitive trust propagation (PGP-style)
- Cryptographic attestation: Signed results
