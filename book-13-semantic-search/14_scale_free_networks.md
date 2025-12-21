<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 14: Scale-Free Networks

This chapter covers **multi-interface nodes** with scale-free (power-law) distribution of interfaces. This design enables nodes to scale from lightweight edge devices to high-capacity hubs while maintaining efficient routing.

## The Scale-Free Insight

Real-world networks often follow a **power-law distribution**:
- Most nodes have few connections
- A few "hub" nodes have many connections
- Distribution: P(k) ∝ k^(-γ) where γ ≈ 2.5-3.0

This mirrors patterns in:
- Web links (few sites get most traffic)
- Social networks (few accounts have most followers)
- Content networks (few channels get most views)

## Multi-Interface Nodes

A single physical node can host multiple **semantic interfaces**, each with its own centroid:

```
┌─────────────────────────────────────────────────────────────┐
│                    High-Capacity Hub Node                    │
│                                                              │
│  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐│
│  │ CSV     │ │ JSON    │ │ XML     │ │ YAML    │ │ TOML    ││
│  │Interface│ │Interface│ │Interface│ │Interface│ │Interface││
│  └────┬────┘ └────┬────┘ └────┬────┘ └────┬────┘ └────┬────┘│
│       │          │          │          │          │         │
│       └──────────┴──────────┼──────────┴──────────┘         │
│                             │                                │
│                    Unified Binary Search                     │
│                   (all interfaces + connections)             │
│                             │                                │
│              ┌──────────────┼──────────────┐                 │
│              │              │              │                 │
│           Shared         Shared         Shared               │
│           Database      Embeddings     Compute               │
└─────────────────────────────────────────────────────────────┘
```

### Why Multiple Interfaces?

| Benefit | Description |
|---------|-------------|
| Resource sharing | One database, one embedding model, one server |
| Semantic diversity | Distinct centroids for different domains |
| Routing efficiency | Internal shortcuts between related interfaces |
| Capacity matching | Interface count matches node resources |

## Power-Law Interface Distribution

The number of interfaces per node follows a power law:

```python
def sample_interface_count(gamma: float = 2.5, k_min: int = 1, k_max: int = 100) -> int:
    """Sample interface count from power-law distribution.

    P(k) ∝ k^(-γ)

    Args:
        gamma: Exponent (2.5-3.0 typical for real networks)
        k_min: Minimum interfaces (usually 1)
        k_max: Maximum interfaces (based on node capacity)
    """
    # Inverse transform sampling for discrete power law
    u = random.random()
    # Zeta distribution approximation
    k = k_min
    cumulative = 0
    normalization = sum(i**(-gamma) for i in range(k_min, k_max + 1))

    for i in range(k_min, k_max + 1):
        cumulative += (i**(-gamma)) / normalization
        if u <= cumulative:
            return i
    return k_max
```

### Distribution Examples

For γ = 2.5, expected distribution:

| Interfaces | % of Nodes | Role |
|------------|-----------|------|
| 1-2 | 60% | Leaf specialists |
| 3-5 | 25% | Mid-tier nodes |
| 6-20 | 12% | Regional hubs |
| 20+ | 3% | Major hubs |

## Capacity-Proportional Sizing

The number of interfaces should match node resources:

```python
def compute_interface_capacity(
    data_size_gb: float,
    bandwidth_mbps: float,
    compute_cores: int,
    memory_gb: float
) -> int:
    """Estimate optimal interface count based on resources.

    Heuristic: Each interface needs ~100MB data, 10Mbps bandwidth,
    0.5 cores, and 1GB memory for reasonable performance.
    """
    data_capacity = int(data_size_gb * 10)        # 100MB per interface
    bandwidth_capacity = int(bandwidth_mbps / 10)  # 10Mbps per interface
    compute_capacity = int(compute_cores * 2)      # 2 interfaces per core
    memory_capacity = int(memory_gb)               # 1GB per interface

    return min(data_capacity, bandwidth_capacity, compute_capacity, memory_capacity)
```

### Hub vs Leaf Optimization

| Node Type | Interfaces | Strategy |
|-----------|-----------|----------|
| Leaf (1-2) | Specialize deeply | Distinct, focused centroids |
| Hub (20+) | Compress breadth | Multi-head LDA, topic clustering |

**Leaf nodes** should focus on semantic distinctiveness:
- Each interface covers a narrow domain
- Centroid is sharply defined
- Attracts specific queries

**Hub nodes** may use compression:
- Many interfaces → use topic modeling
- Compress overlapping domains
- Multi-head attention over topics

```python
# Hub compression example
class HubNode:
    def __init__(self, num_topics: int = 50):
        self.lda = LatentDirichletAllocation(n_components=num_topics)
        self.topic_centroids = []

    def compress_interfaces(self, all_documents: List[str]):
        """Compress many documents into topic-based interfaces."""
        # Fit LDA
        doc_topics = self.lda.fit_transform(vectorize(all_documents))

        # Create one interface per topic
        for topic_id in range(self.lda.n_components):
            # Centroid = mean embedding of top documents in topic
            top_docs = get_top_documents(doc_topics, topic_id, n=100)
            centroid = compute_centroid(embed(top_docs))
            self.topic_centroids.append(centroid)
```

## Unified Binary Search

With many interfaces, we need efficient lookup. Store ALL connection vectors in a single sorted array:

```python
class MultiInterfaceNode:
    def __init__(self, node_id: str, interfaces: List[Interface]):
        self.node_id = node_id
        self.interfaces = interfaces

        # Unified sorted array of all vectors
        self.all_vectors: List[Tuple[float, str, str]] = []  # (angle, interface_id, connection_id)

    def build_unified_index(self):
        """Build single sorted array across all interfaces."""
        self.all_vectors = []

        for interface in self.interfaces:
            for conn in interface.connections:
                angle = compute_cosine_angle(conn.centroid, self.global_reference)
                self.all_vectors.append((angle, interface.id, conn.id))

        # Sort by angle for binary search
        self.all_vectors.sort(key=lambda x: x[0])

    def find_nearest_direction(self, query_embedding: np.ndarray) -> Tuple[str, str]:
        """Binary search for connection closest to query direction."""
        query_angle = compute_cosine_angle(query_embedding, self.global_reference)

        # Binary search
        idx = bisect.bisect_left([v[0] for v in self.all_vectors], query_angle)

        # Check neighbors
        candidates = []
        if idx > 0:
            candidates.append(self.all_vectors[idx - 1])
        if idx < len(self.all_vectors):
            candidates.append(self.all_vectors[idx])

        # Return closest
        best = min(candidates, key=lambda v: abs(v[0] - query_angle))
        return best[1], best[2]  # interface_id, connection_id
```

### Complexity Analysis

| Operation | Separate Arrays | Unified Array |
|-----------|-----------------|---------------|
| Build | O(k × m log m) | O(n log n) where n = k × m |
| Lookup | O(k × log m) | O(log n) |
| Memory | O(k × m) | O(n) |

Where k = interfaces, m = connections per interface, n = total connections.

## Internal Shortcuts

Interfaces on the same node can shortcut to each other:

```python
def create_internal_shortcuts(self):
    """Create shortcuts between related interfaces on same node."""
    for i, iface_a in enumerate(self.interfaces):
        for j, iface_b in enumerate(self.interfaces):
            if i >= j:
                continue

            # Check if related (high centroid similarity)
            similarity = cosine_similarity(iface_a.centroid, iface_b.centroid)
            if similarity > 0.3:  # Related but not identical
                # Create bidirectional internal shortcut
                iface_a.internal_shortcuts.append(iface_b.id)
                iface_b.internal_shortcuts.append(iface_a.id)
```

This enables fast internal routing without network hops.

## Web Traffic Analogy

The scale-free design mirrors content networks:

| Web Pattern | KG Equivalent |
|-------------|---------------|
| YouTube channel | Semantic interface |
| Channel specialization | Distinct centroid |
| Subscriber count | Query volume |
| Algorithm promotion | Small-world shortcuts |

**Key insight**: Specialized channels get viewership. Similarly:
- Specialized interfaces (distinct centroids) get queries
- Generalist interfaces dilute their centroids
- The algorithm (router) promotes nodes that answer queries well

## Implementation

The full implementation is in `multi_interface_node.py`:

```python
from multi_interface_node import (
    MultiInterfaceNode,
    Interface,
    MultiInterfaceNetwork,
    sample_interface_count
)

# Create nodes with power-law interface counts
network = MultiInterfaceNetwork(gamma=2.5)

for node_config in node_configs:
    # Sample interface count from power law
    num_interfaces = sample_interface_count(gamma=2.5, k_max=node_config.capacity)

    # Create interfaces with distinct centroids
    interfaces = []
    for topic in node_config.topics[:num_interfaces]:
        interface = Interface(
            id=f"{node_config.id}_{topic}",
            centroid=compute_centroid(topic_documents[topic]),
            topics=[topic]
        )
        interfaces.append(interface)

    # Create multi-interface node
    node = MultiInterfaceNode(
        node_id=node_config.id,
        interfaces=interfaces
    )

    network.add_node(node)

# Build unified indices and connections
network.build()
```

## Configuration

### Prolog Interface (Python)

```prolog
multi_interface_config([
    gamma(2.5),                    % Power-law exponent
    k_min(1),                      % Minimum interfaces
    k_max(100),                    % Maximum interfaces
    internal_shortcuts(true),      % Enable internal shortcuts
    unified_search(true),          % Use unified binary search
    angle_ordering(cosine_based)   % Full cosine, not 2D projection
]).

capacity_config(node_type, [
    leaf([
        interfaces(1, 2),
        strategy(specialize)
    ]),
    hub([
        interfaces(20, 100),
        strategy(compress),
        compression_method(lda),
        num_topics(50)
    ])
]).
```

### Runtime Parameters

| Parameter | Default | Description |
|-----------|---------|-------------|
| `gamma` | 2.5 | Power-law exponent |
| `k_min` | 1 | Minimum interfaces per node |
| `k_max` | 100 | Maximum interfaces per node |
| `internal_shortcut_threshold` | 0.3 | Similarity for internal shortcuts |
| `use_unified_search` | true | Single sorted array vs per-interface |

## Design Considerations

### Things We Tried

1. **Cosine-based angles**: Works well, uses full high-D information
2. **Unified binary search**: O(log n) lookup across all interfaces
3. **Power-law distribution**: Matches real-world network patterns

### Things We Considered But Didn't Implement

| Alternative | Why Not |
|-------------|---------|
| 2D angle projection | Uses only 2 dimensions, loses discrimination |
| Per-interface arrays | O(k × log m) instead of O(log n) |
| Uniform interface distribution | Doesn't match real capacity variation |
| Dynamic centroid updates | Centroid = data, not routing connections |

### Open Questions

1. **Optimal γ**: 2.5 is typical, but semantic networks may differ
2. **Hub compression**: When is LDA better than separate interfaces?
3. **Cross-language**: How to implement in Go/Rust?

## Summary

- **Scale-free networks** follow power-law distribution of interfaces
- **Multi-interface nodes** share resources while maintaining semantic diversity
- **Capacity-proportional sizing** matches interfaces to node resources
- **Hubs compress** (LDA, topic clustering), **leaves specialize** (distinct centroids)
- **Unified binary search** provides O(log n) lookup across all connections
- **Internal shortcuts** enable fast routing within multi-interface nodes

## Related Chapters

- **[Chapter 13: Advanced Routing](13_advanced_routing.md)** - Proper small-world networks and HNSW
- **[Chapter 6: Distributed Search](06_distributed_search.md)** - Basic federation and Kleinberg routing concepts
