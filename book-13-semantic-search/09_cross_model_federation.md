# Chapter 9: Cross-Model Federation

Previous chapters assumed all nodes use the same embedding model. This chapter tackles a real-world challenge: federating queries across nodes using **different** embedding models.

## The Problem

Production deployments often have heterogeneous embedding models:

- **Legacy nodes** still running `all-MiniLM-L6-v2`
- **Upgraded nodes** using `E5-large-v2` or `BGE-large`
- **Specialized nodes** with domain-specific fine-tuned models
- **Multi-team deployments** where different groups chose different models

The challenge: **cosine similarity between vectors from different models is meaningless**. The embedding spaces are completely incompatible.

```python
# This comparison is MEANINGLESS
vec_minilm = minilm_model.encode("How do I parse CSV?")      # 384 dims
vec_e5 = e5_model.encode("How do I parse CSV?")              # 1024 dims

similarity = cosine_sim(vec_minilm, vec_e5)  # Nonsense!
```

So how can we federate across models?

## Core Insight: Probabilities Are Comparable

While raw embeddings are incompatible, **normalized scores are comparable**:

```python
def to_probability(similarities: List[float]) -> List[float]:
    """Convert similarities to probabilities via softmax."""
    exp_scores = [math.exp(s) for s in similarities]
    total = sum(exp_scores)
    return [e / total for e in exp_scores]
```

After softmax normalization, each model outputs a probability distribution:

```
P(answer_i | query, model_m) = exp(sim_i) / Σ_j exp(sim_j)
```

This probability represents "how confident is this model that answer_i is relevant?" - a question that's meaningful regardless of the underlying embedding space.

## Two-Phase Architecture

The key insight is that density-based methods work **within** each model pool, and their outputs can be combined **across** pools.

```
                              Query
                                │
                ┌───────────────┼───────────────┐
                ▼               ▼               ▼
        ┌─────────────┐ ┌─────────────┐ ┌─────────────┐
        │  Pool A     │ │  Pool B     │ │  Pool C     │
        │ (MiniLM)    │ │ (E5-large)  │ │ (BGE-base)  │
        │             │ │             │ │             │
        │ Phase 1:    │ │ Phase 1:    │ │ Phase 1:    │
        │ - HDBSCAN   │ │ - HDBSCAN   │ │ - HDBSCAN   │
        │ - KDE       │ │ - KDE       │ │ - KDE       │
        │ - Flux-soft │ │ - Flux-soft │ │ - Flux-soft │
        └──────┬──────┘ └──────┬──────┘ └──────┬──────┘
               │               │               │
               │   density-adjusted scores     │
               │               │               │
               └───────────────┼───────────────┘
                               ▼
                    ┌───────────────────┐
                    │    Phase 2:       │
                    │  Cross-Pool Fusion│
                    │  (no embeddings!) │
                    └─────────┬─────────┘
                              ▼
                        Final Results
```

**Phase 1 (Intra-Pool):** Full density scoring within each model pool. This is the same HDBSCAN + KDE pipeline from Chapter 7.

**Phase 2 (Cross-Pool):** Fuse the density-adjusted scores. No embeddings needed - just scores!

## Implementation

### Configuration

```python
from dataclasses import dataclass, field
from enum import Enum
from typing import List, Optional

class FusionMethod(Enum):
    """Methods for combining results across model pools."""
    WEIGHTED_SUM = "weighted_sum"      # Σ w_m * P(answer|model_m)
    RECIPROCAL_RANK = "rrf"            # Σ 1/(k + rank_m)
    CONSENSUS = "consensus"            # Boost multi-pool agreement
    GEOMETRIC_MEAN = "geometric_mean"  # (Π score_m)^(1/n)
    MAX = "max"                        # max(score_m)


@dataclass
class ModelPoolConfig:
    """Configuration for a single model pool."""
    model_name: str
    weight: float = 1.0
    federation_k: int = 5


@dataclass
class CrossModelConfig:
    """Configuration for cross-model federation."""
    pools: List[ModelPoolConfig] = field(default_factory=list)
    fusion_method: FusionMethod = FusionMethod.WEIGHTED_SUM
    rrf_k: int = 60  # RRF smoothing constant
    consensus_threshold: float = 0.1
    consensus_boost_factor: float = 1.5
```

### Pool Results

Each pool returns density-adjusted scores:

```python
@dataclass
class PoolResult:
    """Result from a single model pool with density information."""
    answer_id: str
    answer_text: str
    answer_hash: str

    # Scores
    raw_score: float
    density_adjusted_score: float

    # Density information (computed within pool)
    density_score: float = 0.0
    cluster_id: int = -1
    cluster_size: int = 0

    # Provenance
    source_nodes: List[str] = field(default_factory=list)
    model_name: str = ""
```

## Fusion Methods

### Weighted Sum

The simplest approach - weighted average of probabilities:

```python
def weighted_sum_fusion(
    pool_responses: Dict[str, PoolResponse],
    weight_map: Dict[str, float],
    top_k: int
) -> List[FusedResult]:
    """Weighted sum of density-adjusted probabilities."""

    answer_scores = defaultdict(float)

    for model, response in pool_responses.items():
        weight = weight_map.get(model, 1.0)

        # Normalize to probabilities within pool
        total = sum(r.density_adjusted_score for r in response.results)

        for r in response.results:
            prob = r.density_adjusted_score / total if total > 0 else 0
            answer_scores[r.answer_hash] += weight * prob

    # Rank and return top-k
    ranked = sorted(answer_scores.items(), key=lambda x: -x[1])
    return [build_fused_result(h, s) for h, s in ranked[:top_k]]
```

### Reciprocal Rank Fusion (RRF)

Score-agnostic - only uses rankings:

```python
def rrf_fusion(
    pool_responses: Dict[str, PoolResponse],
    k: int = 60,  # Smoothing constant
    top_k: int = 10
) -> List[FusedResult]:
    """Reciprocal Rank Fusion - combines rankings, not scores."""

    answer_rrf = defaultdict(float)

    for model, response in pool_responses.items():
        # Sort by score to get ranks
        sorted_results = sorted(
            response.results,
            key=lambda r: -r.density_adjusted_score
        )

        for rank, r in enumerate(sorted_results, start=1):
            answer_rrf[r.answer_hash] += 1.0 / (k + rank)

    ranked = sorted(answer_rrf.items(), key=lambda x: -x[1])
    return [build_fused_result(h, s) for h, s in ranked[:top_k]]
```

RRF is robust because it only depends on relative ordering, not absolute scores. The constant `k=60` prevents top-ranked items from dominating.

### Consensus Fusion

Boost answers that appear in high-density clusters across **multiple** pools:

```python
def consensus_fusion(
    pool_responses: Dict[str, PoolResponse],
    config: CrossModelConfig,
    top_k: int
) -> List[FusedResult]:
    """Boost answers with high density across multiple pools."""

    answer_evidence = defaultdict(list)

    # Collect evidence from each pool
    for model, response in pool_responses.items():
        for r in response.results:
            if r.density_adjusted_score >= config.consensus_threshold:
                answer_evidence[r.answer_hash].append({
                    'model': model,
                    'density': r.density_score,
                    'cluster_size': r.cluster_size,
                    'prob': r.density_adjusted_score
                })

    # Score with consensus boost
    scores = {}
    for answer_hash, evidences in answer_evidence.items():
        base_score = sum(e['prob'] for e in evidences)

        # Boost if multiple pools agree
        if len(evidences) >= config.min_pools_for_consensus:
            densities = [e['density'] for e in evidences]
            density_agreement = geometric_mean(densities)
            boost = 1 + (config.consensus_boost_factor - 1) * density_agreement
            base_score *= boost

        scores[answer_hash] = base_score

    ranked = sorted(scores.items(), key=lambda x: -x[1])
    return [build_fused_result(h, s) for h, s in ranked[:top_k]]
```

The key insight: an answer in **dense clusters across multiple embedding spaces** is extremely high confidence. Different models independently concluded it's relevant!

## The Cross-Model Engine

```python
class CrossModelFederatedEngine:
    """Federate queries across nodes with different embedding models."""

    def __init__(
        self,
        router: KleinbergRouter,
        config: CrossModelConfig
    ):
        self.router = router
        self.config = config
        self.pool_engines: Dict[str, FederatedQueryEngine] = {}

    def discover_pools(self) -> Dict[str, List[str]]:
        """Group nodes by embedding model."""
        all_nodes = self.router.discover_nodes()
        pools = defaultdict(list)

        for node in all_nodes:
            model = node.metadata.get('embedding_model', 'unknown')
            pools[model].append(node.node_id)

        return pools

    def federated_query(
        self,
        query_text: str,
        top_k: int = 10
    ) -> CrossModelResponse:
        """Execute cross-model federated query."""

        # Phase 1: Query each pool in parallel
        pool_responses = {}
        with ThreadPoolExecutor() as executor:
            futures = {
                executor.submit(self._query_pool, model, query_text): model
                for model in self.pool_engines
            }
            for future in as_completed(futures):
                model = futures[future]
                pool_responses[model] = future.result()

        # Phase 2: Fuse results
        if self.config.fusion_method == FusionMethod.WEIGHTED_SUM:
            fused = weighted_sum_fusion(pool_responses, self.config, top_k)
        elif self.config.fusion_method == FusionMethod.RECIPROCAL_RANK:
            fused = rrf_fusion(pool_responses, self.config, top_k)
        elif self.config.fusion_method == FusionMethod.CONSENSUS:
            fused = consensus_fusion(pool_responses, self.config, top_k)
        # ... other methods

        return CrossModelResponse(results=fused)
```

## Adaptive Model Weights

Which models should we trust more? We can learn this from user feedback:

```python
class AdaptiveModelWeights:
    """Learn optimal model weights from relevance feedback."""

    def __init__(self, models: List[str], learning_rate: float = 0.01):
        # Start with uniform weights
        n = len(models)
        self.weights = {m: 1.0 / n for m in models}
        self.learning_rate = learning_rate

    def update(
        self,
        chosen_answer: str,
        pool_rankings: Dict[str, List[str]]
    ):
        """Update weights based on which models ranked answer highest."""

        # Compute rewards (inverse rank)
        rewards = {}
        for model, ranking in pool_rankings.items():
            if chosen_answer in ranking:
                rank = ranking.index(chosen_answer) + 1
                rewards[model] = 1.0 / rank
            else:
                rewards[model] = 0.0

        # Normalize rewards
        total = sum(rewards.values())
        if total > 0:
            rewards = {m: r / total for m, r in rewards.items()}

        # Gradient update
        for model in self.weights:
            target = rewards.get(model, 0.0)
            self.weights[model] += self.learning_rate * (target - self.weights[model])

        # Renormalize
        total = sum(self.weights.values())
        self.weights = {m: w / total for m, w in self.weights.items()}

    def save_to_file(self, path: str):
        """Persist learned weights."""
        with open(path, 'w') as f:
            json.dump({'weights': self.weights}, f)

    @classmethod
    def load_from_file(cls, path: str) -> 'AdaptiveModelWeights':
        """Load previously learned weights."""
        with open(path) as f:
            data = json.load(f)
        obj = cls(list(data['weights'].keys()))
        obj.weights = data['weights']
        return obj
```

## HTTP Endpoints

The cross-model engine exposes these endpoints:

```python
@app.route("/kg/cross-model", methods=["POST"])
def cross_model_query():
    """Execute cross-model federated query."""
    data = request.json
    response = engine.federated_query(
        query_text=data["query_text"],
        top_k=data.get("top_k", 10)
    )
    return jsonify(response.to_dict())

@app.route("/kg/cross-model/pools", methods=["GET"])
def get_pools():
    """List available model pools."""
    pools = engine.discover_pools()
    return jsonify({
        "pools": {model: len(nodes) for model, nodes in pools.items()}
    })

@app.route("/kg/cross-model/weights", methods=["GET", "PUT"])
def model_weights():
    """Get or set model weights."""
    if request.method == "GET":
        return jsonify(adaptive_weights.to_dict())
    else:
        adaptive_weights.set_weights(request.json["weights"])
        return jsonify({"status": "updated"})

@app.route("/kg/cross-model/feedback", methods=["POST"])
def submit_feedback():
    """Submit relevance feedback for weight learning."""
    data = request.json
    adaptive_weights.update(
        data["chosen_answer"],
        data["pool_rankings"]
    )
    return jsonify({"status": "updated"})
```

## Prolog Configuration

Cross-model federation is configured via service definitions:

```prolog
service(multi_model_kg, [
    transport(http('/kg', [port(8080)])),
    discovery_backend(consul),
    cross_model([
        fusion_method(consensus),
        consensus_threshold(0.1),
        consensus_boost(1.5),
        pools([
            pool('all-MiniLM-L6-v2', [weight(0.3), federation_k(5)]),
            pool('E5-large-v2', [weight(0.5), federation_k(3)]),
            pool('BGE-base-en', [weight(0.2), federation_k(5)])
        ]),
        learn_weights(true)
    ])
], [
    receive(Query),
    handle_cross_model_query(Query, Response),
    respond(Response)
]).
```

Validation predicates ensure correct configuration:

```prolog
is_valid_cross_model_option(fusion_method(M)) :-
    member(M, [weighted_sum, rrf, consensus, geometric_mean, max]).

is_valid_cross_model_option(pools(Pools)) :-
    is_list(Pools), maplist(is_valid_pool_config, Pools).

is_valid_pool_config(pool(Model, Opts)) :-
    atom(Model),
    maplist(is_valid_pool_option, Opts).

is_valid_pool_option(weight(W)) :- number(W), W > 0.
is_valid_pool_option(federation_k(K)) :- integer(K), K > 0.
```

## When to Use Each Fusion Method

| Method | Use Case | Pros | Cons |
|--------|----------|------|------|
| `weighted_sum` | General purpose, known model quality | Simple, interpretable | Requires weight tuning |
| `rrf` | Unknown model quality, diverse sources | Score-agnostic, robust | Ignores confidence levels |
| `consensus` | High-precision requirements | Strong signal from agreement | May miss unique insights |
| `geometric_mean` | Consistency matters | Rewards uniform performance | Penalizes specialization |
| `max` | Any model might have the answer | Captures specialists | No consensus boost |

## Summary

Cross-model federation enables heterogeneous deployments by:

1. **Preserving density scoring** within each model pool
2. **Fusing probabilities** across pools (embeddings not needed)
3. **Boosting consensus** when multiple models agree
4. **Learning optimal weights** from user feedback

The key insight: while embeddings are incompatible, the semantic question "is this answer relevant?" transcends embedding spaces. By normalizing to probabilities, we make model outputs comparable.

## What's Next

Chapter 10 will cover performance benchmarking - measuring query latency, throughput, and the quality impact of different federation strategies.
