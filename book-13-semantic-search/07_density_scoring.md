<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 7: Density-Based Confidence Scoring

**Flux-Softmax and Semantic Clustering for Result Ranking**

In distributed semantic search, results from multiple nodes need to be aggregated and ranked. Traditional approaches use simple score aggregation (sum, max, average), but these ignore an important signal: **semantic density**. When multiple results cluster tightly in embedding space, they represent consensus - a stronger confidence signal than isolated high-scoring results.

This chapter covers UnifyWeaver's density-based scoring system, implementing a "flux-softmax" where probability flows preferentially through dense semantic regions.

## The Problem: Beyond Raw Scores

Consider a federated query returning results from 5 nodes:

```
Node A: "Parse CSV with pandas" (score: 0.85)
Node B: "Use pandas read_csv" (score: 0.82)
Node C: "CSV parsing in pandas" (score: 0.80)
Node D: "XML parsing with lxml" (score: 0.88)  <- Highest score!
Node E: "Parse delimited files" (score: 0.75)
```

A naive `max` or `sum` aggregation would rank the XML result (D) highest. But semantically, results A, B, C, and E form a dense cluster around "CSV/pandas" - representing **consensus** across multiple nodes.

Density scoring captures this: results in dense regions get probability boosts.

## Core Concepts

### Kernel Density Estimation (KDE)

KDE estimates the probability density at each point based on nearby neighbors:

```
p̂(x) = (1/n) Σᵢ K_h(x - eᵢ)
```

Where:
- `K_h` is a kernel function (we use Gaussian)
- `h` is the bandwidth controlling smoothness
- `eᵢ` are the embedding vectors

In semantic space, we use **cosine distance** rather than Euclidean:

```python
def cosine_distance(a, b):
    sim = np.dot(a, b) / (np.linalg.norm(a) * np.linalg.norm(b))
    return 1.0 - sim  # Distance = 1 - similarity
```

### Bandwidth Selection

The bandwidth `h` controls the density estimate's smoothness:
- Too small: Every point is its own cluster (overfitting)
- Too large: All points blend together (underfitting)

UnifyWeaver implements **Silverman's rule**:

```python
def silverman_bandwidth(distances, n):
    if n == 0 or len(distances) == 0:
        return 0.1
    std = np.std(distances)
    if std < 1e-10:
        return 0.1
    return 1.06 * std * (n ** -0.2)
```

### The Flux-Softmax Formula

Standard softmax converts scores to probabilities:

```
P(i) = exp(sᵢ) / Σⱼ exp(sⱼ)
```

Flux-softmax adds a density modulator:

```
P(i) = exp(sᵢ/τ) * (1 + w * dᵢ) / Z
```

Where:
- `τ` is temperature (controls sharpness)
- `w` is density weight (0 = ignore density, 1 = strong influence)
- `dᵢ` is the normalized density score
- `Z` is the partition function

```python
def flux_softmax(scores, densities, density_weight=0.3, temperature=1.0):
    """Density-weighted softmax normalization."""
    scaled = scores / temperature - scores.max()
    flux_factor = 1.0 + density_weight * densities
    exp_scores = np.exp(scaled) * flux_factor
    return exp_scores / exp_scores.sum()
```

## Two-Stage Pipeline

A critical insight: computing density across **all** results dilutes the signal. Unrelated results shouldn't affect each other's density.

UnifyWeaver uses a two-stage pipeline:

### Stage 1: Cluster by Similarity

Group results into semantic clusters before computing density:

```python
def cluster_by_similarity(embeddings, threshold=0.7, min_cluster_size=2):
    """Greedy centroid-based clustering."""
    clusters = []
    labels = np.full(len(embeddings), -1)

    for i, emb in enumerate(embeddings):
        best_cluster = -1
        best_sim = threshold

        for j, (centroid, members) in enumerate(clusters):
            sim = cosine_similarity(emb, centroid)
            if sim > best_sim:
                best_sim = sim
                best_cluster = j

        if best_cluster >= 0:
            # Join existing cluster
            clusters[best_cluster][1].append(i)
            labels[i] = best_cluster
        else:
            # Create new cluster
            clusters.append([emb.copy(), [i]])
            labels[i] = len(clusters) - 1

    # Mark small clusters as noise
    for j, (_, members) in enumerate(clusters):
        if len(members) < min_cluster_size:
            for i in members:
                labels[i] = -1

    return labels, [c[0] for c in clusters]
```

### Stage 2: Intra-Cluster Density

Compute density **within** each cluster:

```python
def compute_cluster_density(embeddings, cluster_labels, config):
    """KDE within each cluster."""
    densities = np.zeros(len(embeddings))

    for cluster_id in set(cluster_labels):
        if cluster_id < 0:
            continue  # Skip noise

        mask = cluster_labels == cluster_id
        cluster_embs = embeddings[mask]

        if len(cluster_embs) < 2:
            densities[mask] = 1.0
            continue

        # Compute pairwise distances within cluster
        cluster_densities = compute_density_scores(
            cluster_embs, config
        )
        densities[mask] = cluster_densities

    return densities
```

### Complete Pipeline

```python
def two_stage_density_pipeline(embeddings, scores, config=None):
    """
    Full density scoring pipeline.

    Returns:
        flux_probs: Density-weighted probabilities
        densities: Raw density scores
        labels: Cluster assignments
        centroids: Cluster centroids
    """
    if config is None:
        config = DensityConfig()

    # Stage 1: Cluster
    if config.clustering_enabled:
        labels, centroids = cluster_by_similarity(
            embeddings,
            threshold=config.similarity_threshold,
            min_cluster_size=config.min_cluster_size
        )
    else:
        labels = np.zeros(len(embeddings), dtype=int)
        centroids = [embeddings.mean(axis=0)]

    # Stage 2: Intra-cluster density
    densities = compute_cluster_density(embeddings, labels, config)

    # Stage 3: Flux-softmax
    flux_probs = flux_softmax(
        scores, densities, config.density_weight
    )

    return flux_probs, densities, labels, centroids
```

## Integration with Federated Queries

### DensityAwareFederatedEngine

Extend the base federated engine with density scoring:

```python
from federated_query import (
    FederatedQueryEngine,
    AggregationStrategy,
    AggregationConfig
)

class DensityAwareFederatedEngine(FederatedQueryEngine):
    """Federated engine with density-based scoring."""

    def _normalize_and_rank(self, aggregated, total_partition, top_k):
        results_list = list(aggregated.values())

        if self.config.strategy == AggregationStrategy.DENSITY_FLUX:
            # Apply two-stage density scoring
            results_list = apply_density_scoring(
                results_list, self.config
            )

            # Use flux-softmax normalization
            result_probs = density_flux_normalize(
                results_list, self.config
            )

            return [
                result.to_dict(normalized_prob=prob)
                for result, prob in result_probs[:top_k]
            ]

        return super()._normalize_and_rank(
            aggregated, total_partition, top_k
        )
```

### Prolog Configuration

Configure density scoring in service definitions:

```prolog
service(semantic_search_node, [
    transport(http('/kg', [port(8080)])),
    federation([
        aggregation(density_flux, [
            density_weight(0.3),
            clustering_enabled(true),
            similarity_threshold(0.7),
            min_cluster_size(2)
        ]),
        federation_k(5)
    ])
], [
    receive(Query),
    handle_federated_query(Query, Response),
    respond(Response)
]).
```

Validation predicates ensure correct configuration:

```prolog
is_valid_aggregation_strategy(density_flux).
is_valid_aggregation_strategy(density_flux(Opts)) :-
    is_list(Opts), maplist(is_valid_density_option, Opts).

is_valid_density_option(bandwidth(auto)).
is_valid_density_option(bandwidth(silverman)).
is_valid_density_option(bandwidth(B)) :- number(B), B > 0.
is_valid_density_option(density_weight(W)) :-
    number(W), W >= 0, W =< 1.
is_valid_density_option(similarity_threshold(T)) :-
    number(T), T >= 0, T =< 1.
```

## Response Format

Density-enhanced responses include cluster information:

```json
{
    "query_id": "uuid-123",
    "results": [
        {
            "answer_text": "Use pandas read_csv for CSV parsing",
            "combined_score": 2.47,
            "normalized_prob": 0.35,
            "density_score": 0.92,
            "cluster_id": 0,
            "cluster_confidence": 3.68,
            "source_nodes": ["node_a", "node_b", "node_c"],
            "diversity_score": 1.0
        },
        {
            "answer_text": "XML parsing with lxml",
            "combined_score": 0.88,
            "normalized_prob": 0.15,
            "density_score": 0.0,
            "cluster_id": null,
            "cluster_confidence": 0.0,
            "source_nodes": ["node_d"],
            "diversity_score": 1.0
        }
    ],
    "total_partition_sum": 7.23,
    "nodes_queried": 5
}
```

Key fields:
- `density_score`: Normalized density (0-1) within cluster
- `cluster_id`: Which semantic cluster (null = noise/singleton)
- `cluster_confidence`: density * cluster_size (higher = stronger consensus)

## Practical Example

### Setting Up Density-Aware Search

```python
from kleinberg_router import KleinbergRouter
from federated_query import (
    create_density_aware_engine,
    AggregationStrategy
)

# Create router
router = KleinbergRouter(
    discovery_client=create_discovery_client('consul'),
    alpha=2.0,
    max_hops=10
)

# Create density-aware engine
engine = create_density_aware_engine(
    router=router,
    federation_k=5,
    density_weight=0.3,
    clustering_enabled=True,
    similarity_threshold=0.7
)

# Execute query
response = engine.federated_query(
    query_text="How do I parse CSV files?",
    query_embedding=embed("How do I parse CSV files?"),
    top_k=10
)

# Results are ranked by flux-softmax
for result in response.results:
    print(f"{result['normalized_prob']:.2%}: {result['answer_text']}")
    print(f"  Cluster: {result['cluster_id']}, "
          f"Confidence: {result['cluster_confidence']:.2f}")
```

### Tuning Parameters

| Parameter | Default | Effect |
|-----------|---------|--------|
| `density_weight` | 0.3 | Higher = more influence from density |
| `similarity_threshold` | 0.7 | Higher = tighter clusters |
| `min_cluster_size` | 2 | Minimum results to form a cluster |
| `bandwidth` | silverman | Kernel smoothness (auto-selected) |

**Guidelines:**
- **High precision domains** (legal, medical): Use higher `similarity_threshold` (0.8+)
- **Exploratory search**: Use lower `similarity_threshold` (0.5-0.6)
- **Consensus-heavy**: Increase `density_weight` (0.5+)
- **Score-focused**: Decrease `density_weight` (0.1-0.2)

## Advanced Clustering: HDBSCAN

The greedy clustering shown above works well for simple cases, but hierarchical density clustering provides more robust results. UnifyWeaver supports **HDBSCAN** (Hierarchical Density-Based Spatial Clustering of Applications with Noise):

```python
from density_scoring import ClusterMethod, DensityConfig

config = DensityConfig(
    cluster_method=ClusterMethod.HDBSCAN,
    min_cluster_size=3,
    hdbscan_min_samples=2,
    hdbscan_cluster_selection_epsilon=0.0
)
```

### Soft Cluster Membership

Unlike hard clustering, HDBSCAN provides **probability scores** for cluster membership:

```python
def get_hdbscan_probabilities(clusterer, embeddings):
    """Get soft cluster membership probabilities."""
    return clusterer.probabilities_  # Array of [0, 1] values
```

This is valuable for borderline results that partially belong to multiple clusters.

### Graceful Fallback

If HDBSCAN isn't installed, the system automatically falls back to greedy clustering:

```python
try:
    import hdbscan
    HDBSCAN_AVAILABLE = True
except ImportError:
    HDBSCAN_AVAILABLE = False
    # Falls back to cluster_by_similarity()
```

## Adaptive Bandwidth Selection

Silverman's rule provides a reasonable default bandwidth, but optimal bandwidth varies by data distribution. UnifyWeaver implements two adaptive methods:

### Cross-Validation Bandwidth

Select bandwidth by maximizing leave-one-out likelihood:

```python
def cross_validation_bandwidth(embeddings, n_candidates=10):
    """Find optimal bandwidth via cross-validation."""
    # Log-spaced candidates from data-driven range
    distances = pairwise_cosine_distances(embeddings)
    h_min = np.percentile(distances[distances > 0], 5)
    h_max = np.percentile(distances[distances > 0], 95)
    candidates = np.logspace(np.log10(h_min), np.log10(h_max), n_candidates)

    best_h, best_score = None, float('-inf')
    for h in candidates:
        score = leave_one_out_cv_score(embeddings, h)
        if score > best_score:
            best_h, best_score = h, score

    return best_h
```

### Balloon Estimator (Local Bandwidth)

Different regions may need different bandwidths. The balloon estimator adapts per-point:

```python
def adaptive_local_bandwidth(embeddings, pilot_densities, alpha=0.5):
    """Compute per-point bandwidth using balloon estimator.

    h(x) = h₀ × (p̂(x) / g)^(-α)

    Where:
    - h₀ is the global bandwidth
    - p̂(x) is the pilot density at x
    - g is the geometric mean of pilot densities
    - α controls adaptation strength (0.5 = square root)
    """
    g = np.exp(np.mean(np.log(pilot_densities + 1e-10)))
    local_factors = (pilot_densities / g) ** (-alpha)
    return global_bandwidth * local_factors
```

Enable with:

```python
config = DensityConfig(
    use_adaptive_bandwidth=True,
    adaptive_alpha=0.5,  # Adaptation strength
    cv_n_candidates=10   # Cross-validation grid size
)
```

## Efficiency Optimizations

For large result sets (100+ embeddings), naive KDE is O(n²). UnifyWeaver provides several optimizations:

### Distance Caching

LRU cache for pairwise distances:

```python
class DistanceCache:
    """LRU cache for embedding distances."""
    def __init__(self, max_size=10000):
        self.cache = OrderedDict()
        self.max_size = max_size

    def get_or_compute(self, emb_i, emb_j, compute_fn):
        key = (hash(emb_i.tobytes()), hash(emb_j.tobytes()))
        if key in self.cache:
            self.cache.move_to_end(key)
            return self.cache[key]

        value = compute_fn(emb_i, emb_j)
        self.cache[key] = value
        if len(self.cache) > self.max_size:
            self.cache.popitem(last=False)
        return value
```

### Random Projection Sketching

Reduce dimensionality while preserving distances (Johnson-Lindenstrauss):

```python
def sketch_embeddings(embeddings, target_dim=64, seed=42):
    """Reduce dimensionality via random projection."""
    rng = np.random.RandomState(seed)
    d = embeddings.shape[1]

    # Random projection matrix
    R = rng.randn(d, target_dim) / np.sqrt(target_dim)
    return embeddings @ R
```

### Approximate Nearest Neighbors

For density estimation, we only need neighbors within bandwidth. ANN provides O(n log n) lookup:

```python
def approximate_nearest_neighbors(embeddings, k=50, n_projections=5):
    """Find approximate k-nearest neighbors."""
    candidates = set()
    for proj in range(n_projections):
        sketched = sketch_embeddings(embeddings, seed=proj)
        # Sort by first dimension, gather candidates
        order = np.argsort(sketched[:, 0])
        # Sliding window to find nearby points
        for i, idx in enumerate(order):
            window = order[max(0, i-k):min(len(order), i+k+1)]
            candidates.update((idx, j) for j in window if j != idx)

    # Refine with true distances
    return refine_candidates(embeddings, candidates, k)
```

### Complete Efficient Pipeline

```python
def compute_efficient_density_scores(embeddings, config):
    """O(n log n) density estimation for large datasets."""
    n = len(embeddings)

    if n < config.large_dataset_threshold:
        return compute_density_scores(embeddings, config)

    # Use sketching for initial estimates
    if config.use_sketching:
        sketched = sketch_embeddings(embeddings, config.sketch_dim)
    else:
        sketched = embeddings

    # ANN for neighbor finding
    neighbors = approximate_nearest_neighbors(
        sketched, k=min(50, n-1)
    )

    # Compute density using only neighbors
    densities = np.zeros(n)
    for i in range(n):
        neighbor_dists = [
            cosine_distance(embeddings[i], embeddings[j])
            for j in neighbors[i]
        ]
        densities[i] = kernel_density_at_point(neighbor_dists, config.bandwidth)

    return normalize_densities(densities)
```

Enable with:

```python
config = DensityConfig(
    use_sketching=True,
    sketch_dim=64,
    large_dataset_threshold=100,
    cache_distances=True
)
```

## The Smoothness Assumption

Density scoring rests on a **smoothness assumption**: semantically similar questions should have similar answer distributions. This is formalized as a Lipschitz continuity constraint:

```
||f(q₁) - f(q₂)|| ≤ L · ||q₁ - q₂||
```

Where `f` maps questions to answer distributions.

This assumption justifies:
1. Using KDE (smooth density estimates)
2. Clustering before density (discontinuities at topic boundaries)
3. Boosting dense regions (consensus = confidence)

The assumption can fail when:
- Questions are ambiguous (same embedding, different intents)
- Domains have sharp boundaries (legal vs. medical)
- Corpus has sampling bias (dense regions may be overrepresented)

## Summary

Density-based scoring extends federated search with semantic consensus signals:

1. **Cluster** results by embedding similarity (greedy or HDBSCAN)
2. **Compute density** within each cluster using KDE
3. **Apply flux-softmax** to boost dense regions
4. **Scale efficiently** with sketching and ANN for large result sets

Advanced features:
- **HDBSCAN clustering** for hierarchical density-based grouping with soft membership
- **Adaptive bandwidth** via cross-validation and balloon estimator
- **O(n log n) scaling** through random projections and approximate nearest neighbors

This approach surfaces results that represent **consensus** across nodes, not just high individual scores.

## What's Next

- [Book 14: AI Training](../book-14-ai-training/README.md) - Learn the theory behind density estimation and kernel methods
