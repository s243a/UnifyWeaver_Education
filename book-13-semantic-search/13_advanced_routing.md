<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 13: Advanced Routing Algorithms

This chapter covers **proper small-world networks** and advanced routing algorithms that improve on the basic federation approach from Chapter 6. We clarify the distinction between greedy forwarding (which works on any graph) and Kleinberg routing (which requires specific network structure).

## The Key Distinction: Greedy Routing vs Kleinberg Routing

### Greedy Routing

**Greedy routing** is a routing strategy that works on any graph:
- At each hop, forward to the neighbor closest to the target
- Simple to implement, no special network structure required
- Path length depends entirely on the graph topology
- **This is what Chapter 6 describes** (before adding proper small-world structure)

```python
def greedy_route(current_node, target_embedding, neighbors):
    """Route to the neighbor closest to target."""
    best_neighbor = None
    best_similarity = -1

    for neighbor in neighbors:
        sim = cosine_similarity(neighbor.centroid, target_embedding)
        if sim > best_similarity:
            best_similarity = sim
            best_neighbor = neighbor

    return best_neighbor
```

### Kleinberg Routing

**Kleinberg routing** = greedy routing + **properly structured small-world network**:
- Requires both local and long-range connections
- Long-range links follow a specific probability distribution: P(u→v) ∝ 1/distance^α
- Achieves O(log²n) expected path length

**If your network doesn't have the proper small-world structure, you have greedy routing, not Kleinberg routing.** Greedy routing still works, but you don't get the O(log²n) path length guarantee that makes Kleinberg routing efficient.

## Proper Small-World Networks

### The Structure

A proper small-world network has two types of connections:

1. **Local connections (k_local)**: Connect to your k nearest neighbors in embedding space
2. **Long-range shortcuts (k_long)**: Connect to distant nodes with probability proportional to 1/distance^α

```
┌────────────────────────────────────────────────────────┐
│                 Semantic Embedding Space               │
│                                                        │
│     ●───●───●───●───●                                  │
│     │   │   │   │   │  ← Local connections (k_local)   │
│     ●───●───●───●───●                                  │
│         │       ╲                                      │
│         │        ╲   ← Long-range shortcuts (k_long)   │
│         │         ╲                                    │
│     ●───●───●───●───●                                  │
│                                                        │
└────────────────────────────────────────────────────────┘
```

### Why Both Matter

| Connection Type | Purpose | Without It |
|-----------------|---------|------------|
| Local (k_local) | Navigate precisely when near target | May overshoot or oscillate |
| Long-range (k_long) | Jump across the space quickly | O(n) path length instead of O(log²n) |

### Kleinberg's Probability Distribution

Long-range links are chosen with probability:

```
P(u → v) ∝ 1 / distance(u, v)^α
```

Where α = 2.0 is optimal for 2D grids, but **for high-dimensional embedding spaces, α ≈ 2.0 still works well** because semantic similarity already captures the relevant structure.

## Python Implementation

The proper small-world network is implemented in `small_world_proper.py`:

```python
from small_world_proper import SmallWorldProper

# Create network with proper structure
network = SmallWorldProper(
    embedding_dim=384,
    k_local=10,      # 10 nearest neighbors
    k_long=5,        # 5 long-range shortcuts
    alpha=2.0        # Kleinberg exponent
)

# Add nodes
for node in nodes:
    network.add_node(
        node_id=node.id,
        centroid=node.centroid,
        metadata=node.metadata
    )

# Build the small-world structure
network.build_connections()

# Route a query using the unified route() method
path, comparisons = network.route(
    query=embed("How do I parse CSV?"),
    max_hops=10
)
```

### Backtracking (Default Enabled)

All routing implementations now default to **backtracking enabled** for better success rates in P2P scenarios:

```python
# Default: backtracking ON (recommended for P2P/distributed)
path, comps = network.route(query, use_backtrack=True)   # default

# Disable backtracking for performance-critical paths with known good entry points
path, comps = network.route(query, use_backtrack=False)
```

| Setting | Use Case | Success Rate |
|---------|----------|--------------|
| `use_backtrack=True` (default) | P2P, arbitrary start node | ~100% |
| `use_backtrack=False` | Centralized, optimal entry point | ~85-95% |

The unified `route()` method delegates to `route_greedy()` or `route_with_backtrack()` based on this parameter. This applies to both `SmallWorldProper` and `SmallWorldNetwork` (evolution variant).

### Cosine-Based Angle Ordering

For efficient neighbor lookup within nodes, we use **cosine-based angles** rather than 2D projection:

```python
def compute_cosine_angle(vec: np.ndarray, reference: np.ndarray) -> float:
    """Compute angle between vector and reference using cosine similarity.

    This uses the full high-dimensional vectors, not a 2D projection.
    Since we compute cosine anyway for routing, there's no savings in
    projecting to 2D, and we'd lose discrimination between vectors.
    """
    norm_vec = np.linalg.norm(vec)
    norm_ref = np.linalg.norm(reference)
    if norm_vec == 0 or norm_ref == 0:
        return math.pi / 2  # 90 degrees for zero vectors

    similarity = np.dot(vec, reference) / (norm_vec * norm_ref)
    similarity = max(-1.0, min(1.0, similarity))  # Clamp for numerical stability
    return math.acos(similarity)  # Returns angle in radians [0, π]
```

Neighbors are sorted by angle, enabling binary search for the closest direction to the query.

## HNSW: Hierarchical Navigable Small World

HNSW is a multi-layer variant that achieves O(log n) routing:

```
Layer 3:  ●─────────────────●  (few nodes, long jumps)
          │                 │
Layer 2:  ●───────●─────────●  (more nodes)
          │       │         │
Layer 1:  ●───●───●───●─────●  (even more nodes)
          │   │   │   │     │
Layer 0:  ●─●─●─●─●─●─●─●─●─●  (all nodes, local structure)
```

### Key Properties

- **Layer assignment**: Node appears in layer L with probability p^L
- **Search**: Start at top layer, greedily descend
- **Construction**: Insert at assigned layer, connect at all layers below

```python
from small_world_proper import HNSWNetwork

# Create HNSW network
hnsw = HNSWNetwork(
    embedding_dim=384,
    M=16,           # Max connections per layer
    M0=32,          # Max connections at layer 0
    ef_construction=200,  # Search width during construction
    ml=1/math.log(16)     # Level multiplier
)

# Add nodes (layer assigned automatically)
for node in nodes:
    hnsw.insert(node.id, node.centroid)

# Search
results = hnsw.search(query_embedding, k=5, ef=50)
```

### Tunable M Parameter

The M (max neighbors) parameter controls the recall vs memory/build-time tradeoff:

| M Value | Recall@5 | Edge Count | Use Case |
|---------|----------|------------|----------|
| 4 | ~85% | Low | Memory-constrained |
| 8 | ~90% | Moderate | Balanced |
| 16 | ~95% | Higher | Default, good precision |
| 32 | ~98% | High | Maximum recall needed |

**Key insights from benchmarks:**
- Higher M always increases edge count (monotonically)
- Recall improvement has diminishing returns above M=16
- Layer 0 can have more connections (M0 = 2*M) for finer local navigation

```python
from hnsw_layers import HNSWGraph, build_hnsw_index

# Low-memory configuration
graph_small = HNSWGraph(
    max_neighbors=8,      # M=8
    max_neighbors_layer0=16,  # M0=16
    ef_construction=100
)

# High-recall configuration
graph_precise = HNSWGraph(
    max_neighbors=32,     # M=32
    max_neighbors_layer0=64,  # M0=64
    ef_construction=200
)

# Build from vector list
graph = build_hnsw_index(vectors, max_neighbors=16, seed=42)

# Search from any starting node (P2P mode)
results, comparisons = graph.search_from_any_node(
    query_embedding,
    start_node_id="node_123",
    k=5,
    use_backtrack=True  # Enable backtracking for better recall
)
```

## Target Language Support

### Current Implementation Status

| Target | Greedy Routing | Proper Small-World | HNSW |
|--------|----------------|-------------------|------|
| Python | ✅ | ✅ `small_world_proper.py` | ✅ |
| Go | ✅ | ❌ | ❌ |
| Rust | ✅ | ❌ | ❌ |
| C# | ❌ | ❌ | ❌ |

**Important**: The Go and Rust implementations have basic federation and greedy routing, but they don't have the proper small-world network structure. This means:
- They work for distributed search
- They don't achieve O(log²n) path length
- Path length scales less efficiently with network size

### Prolog Configuration (Python only)

```prolog
routing_config(small_world_proper, [
    k_local(10),
    k_long(5),
    alpha(2.0),
    angle_ordering(cosine_based)
]).

routing_config(hnsw, [
    M(16),
    M0(32),
    ef_construction(200),
    ml_factor(0.5)
]).
```

## Comparison: Basic vs Proper Small-World

### Benchmark Results

| Metric | Basic Greedy | Proper Small-World | HNSW |
|--------|--------------|-------------------|------|
| Build time | O(n) | O(n log n) | O(n log n) |
| Memory | O(k × n) | O((k_local + k_long) × n) | O(M × n × log n) |
| Query (avg hops) | O(√n) typical | O(log²n) | O(log n) |
| Query (worst case) | O(n) | O(log²n) | O(log n) |

### When to Use Each

| Scenario | Recommended |
|----------|-------------|
| Small network (<100 nodes) | Basic greedy (simpler) |
| Medium network (100-10,000) | Proper small-world |
| Large network (>10,000) | HNSW |
| Cross-language deployment | Basic greedy (only Python has proper SW) |

## Integration with Federation

The proper small-world network integrates with the federated query engine:

```python
from small_world_proper import ProperSmallWorldNetwork
from federated_query import FederatedQueryEngine

# Build proper network
network = ProperSmallWorldNetwork(k_local=10, k_long=5)
for node in discovered_nodes:
    network.add_node(node.id, node.centroid)
network.build_connections()

# Create router that uses proper structure
class SmallWorldRouter:
    def __init__(self, network):
        self.network = network

    def route_query(self, query_embedding, max_hops=10):
        return self.network.route_to_target(query_embedding, max_hops)

# Use with federation
router = SmallWorldRouter(network)
engine = FederatedQueryEngine(router=router)
```

## Best Practices

### Network Construction

1. **Gather all nodes first**: Build connections after all nodes are known
2. **Use representative centroids**: Quality of centroids affects routing efficiency
3. **Tune k_local and k_long**:
   - k_local = 10 works well for most cases
   - k_long = 5-10 depending on network size

### Angle Computation

1. **Use full cosine similarity**: Don't project to 2D
2. **Reference vector**: Use node's own centroid as reference
3. **Sort once**: Sort neighbors by angle at construction time

### Monitoring

Track these metrics to validate your network:
- Average path length (should be O(log²n))
- Local clustering coefficient (should be high)
- Average node degree (should be k_local + k_long)

## Summary

- **Greedy forwarding** works on any graph but has no efficiency guarantee
- **Kleinberg routing** requires proper small-world structure for O(log²n) paths
- **Proper small-world networks** have k_local nearest neighbors + k_long probability-weighted shortcuts
- **HNSW** adds hierarchy for O(log n) routing
- **Currently only Python** has proper small-world implementation
- **Use cosine-based angles** for neighbor ordering, not 2D projection

## What's Next

- **[Chapter 14: Scale-Free Networks](14_scale_free_networks.md)** - Multi-interface nodes with power-law distribution, capacity-proportional sizing
