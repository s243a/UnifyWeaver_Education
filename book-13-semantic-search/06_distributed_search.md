<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 6: Distributed Semantic Search

This chapter extends the semantic search capabilities from previous chapters with **distributed multi-node architectures**. You'll learn how to route queries across a network of specialized expert systems using Kleinberg small-world routing.

## Overview

While chapters 1-5 covered single-node semantic search, real-world knowledge bases often require:

- **Horizontal scaling**: Distribute load across multiple nodes
- **Specialization**: Different nodes for different domains (CSV, JSON, SQL, etc.)
- **Fault tolerance**: Query can succeed even if some nodes are down
- **Low latency**: Route to the most relevant node first

## Architecture: Multi-Interface Nodes

A single expert system can serve multiple **semantic interfaces**, each with its own centroid:

```
┌─────────────────────────────────────────────────────┐
│            Expert System (Principal Node)           │
│                                                     │
│  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐   │
│  │  Interface  │ │  Interface  │ │  Interface  │   │
│  │   "CSV"     │ │   "JSON"    │ │  "Database" │   │
│  │ centroid    │ │ centroid    │ │ centroid    │   │
│  └──────┬──────┘ └──────┬──────┘ └──────┬──────┘   │
│         │               │               │           │
│         └───────────────┼───────────────┘           │
│                         │                           │
│              Shared KG Database                     │
└─────────────────────────────────────────────────────┘
```

### Creating Interfaces

Using the `KGTopologyAPI` from Chapter 3:

```python
from kg_topology_api import KGTopologyAPI

api = KGTopologyAPI(db_path="kg.db", embeddings_path="embeddings/")

# Create specialized interfaces
csv_interface = api.create_interface(
    name="csv_expert",
    centroid=compute_centroid(csv_questions),  # mean embedding
    topics=["csv", "delimited", "tabular", "spreadsheet"]
)

json_interface = api.create_interface(
    name="json_expert",
    centroid=compute_centroid(json_questions),
    topics=["json", "serialization", "api", "structured"]
)

# Map query to best interface
query = "How do I parse a CSV file with headers?"
interface_id, confidence = api.map_query_to_interface(query)
# interface_id = csv_interface, confidence = 0.89
```

## Greedy Routing

### The Small-World Insight

[Kleinberg's research](https://www.cs.cornell.edu/home/kleinber/icm06-swn.pdf) shows that **greedy routing** on small-world networks achieves O(log²n) path length when link probability is proportional to 1/distance^α.

For semantic search:
- **Distance** = cosine distance in embedding space
- **α** = tuning parameter (typically 2.0 for high-dimensional embeddings)
- **Greedy routing** = route to node with highest similarity to query

> **Note**: This chapter describes **greedy routing** - forwarding to the closest neighbor. To achieve Kleinberg's O(log²n) path length guarantee, you need proper small-world network structure (k_local nearest neighbors + k_long probability-weighted shortcuts). See [Chapter 13](13_advanced_routing.md) for the full implementation.

### Routing Protocol

```json
{
    "__type": "kg_query",
    "__id": "uuid-123",
    "__routing": {
        "origin_node": "node_a",
        "htl": 8,
        "visited": ["node_a"],
        "path_folding_enabled": true
    },
    "__embedding": {
        "model": "all-MiniLM-L6-v2",
        "vector": [0.1, 0.2, 0.3, 0.4, 0.5]
    },
    "payload": {
        "query_text": "How do I parse CSV?",
        "top_k": 5
    }
}
```

Key fields:
- **htl** (Hops-To-Live): Maximum hops before giving up
- **visited**: Prevents cycles
- **path_folding_enabled**: Create shortcuts for future queries

### Python Implementation

```python
from kleinberg_router import KleinbergRouter, RoutingEnvelope
from discovery_clients import create_discovery_client

# Create router with discovery
discovery = create_discovery_client("consul", host="consul.local")
router = KleinbergRouter(
    discovery_client=discovery,
    alpha=2.0,
    max_hops=10,
    similarity_threshold=0.5,
    path_folding_enabled=True
)

# Discover nodes
nodes = router.discover_nodes(tags=["kg_node"])

# Route a query
envelope = RoutingEnvelope(
    origin_node="my_node",
    htl=10,
    visited={"my_node"},
    path_folding_enabled=True
)

results = router.route_query(
    query_embedding=embed("How do I parse CSV?"),
    query_text="How do I parse CSV?",
    envelope=envelope,
    top_k=5
)
```

### Path Folding

After a successful query, the router creates a **shortcut** for faster future lookups:

```python
# After successful match
router.create_shortcut(
    query_text="How do I parse CSV?",
    node_id="csv_expert_node",
    interface_id="csv_interface"
)

# Future queries check shortcuts first
cached = router.check_shortcut("How do I parse CSV?")
if cached:
    # Skip routing, go directly to cached node
    result = forward_to_node(cached)
```

## Distributed API

The `DistributedKGTopologyAPI` extends `KGTopologyAPI` with distributed features:

```python
from kg_topology_api import DistributedKGTopologyAPI
from discovery_clients import create_discovery_client

# Initialize distributed API
api = DistributedKGTopologyAPI(
    db_path="kg.db",
    embeddings_path="embeddings/",
    discovery_client=create_discovery_client("consul")
)

# Register with discovery service
api.register_node(
    interface_id="csv_specialist",
    host="0.0.0.0",
    port=8081,
    tags=["kg_node", "expert_system"]
)

# Distributed search
results = api.distributed_search(
    query_text="How do I parse CSV with custom delimiters?",
    model_name="all-MiniLM-L6-v2",
    top_k=5,
    max_hops=10,
    local_first=True  # Check local DB before routing
)
```

### Discovery Metadata

Nodes publish their semantic identity in discovery metadata:

```python
metadata = {
    "semantic_centroid": base64.b64encode(centroid.tobytes()),
    "embedding_model": "all-MiniLM-L6-v2",
    "interface_topics": json.dumps(["csv", "delimited"]),
    "corpus_id": "my_dataset_v1",
    "data_sources": json.dumps(["docs", "wiki"]),
    "last_updated": datetime.now().isoformat()
}

discovery.register(
    name="kg_node",
    service_id="csv_node_1",
    host="192.168.1.10",
    port=8081,
    tags=["kg_node", "csv"],
    metadata=metadata
)
```

## Federated Queries

When querying multiple nodes, results must be aggregated with **diversity awareness**:

### The Problem: Source Independence

If two nodes trained on the same dataset return similar answers, should we:
1. **Sum** scores (boost consensus)?
2. **Max** scores (prevent echo)?

The answer depends on **source independence**:

| Scenario | Strategy | Rationale |
|----------|----------|-----------|
| Different corpora | SUM | Independent sources = real consensus |
| Same corpus | MAX | Shared data = potential echo |
| Mixed | Diversity-weighted | Partial independence |

### Diversity-Weighted Aggregation

```python
from federated_query import FederatedQueryEngine, AggregationConfig, AggregationStrategy

# Configure aggregation
config = AggregationConfig(
    strategy=AggregationStrategy.DIVERSITY_WEIGHTED,
    dedup_key="answer_id",
    consensus_threshold=3,
    diversity_field="corpus_id"
)

# Create federated engine
engine = FederatedQueryEngine(router, config)

# Execute federated query
results = engine.federated_query(
    query_text="How do I parse CSV?",
    query_embedding=embed("How do I parse CSV?"),
    top_k=5,
    timeout_ms=30000
)

# Results include diversity metrics
for result in results:
    print(f"Answer: {result.text}")
    print(f"Score: {result.score}")
    print(f"Diversity: {result.diversity_score}")  # unique_corpora / total_nodes
    print(f"Sources: {result.unique_corpora}")
```

### 3-Tier Diversity Model

The `_diversity_merge()` function implements:

```python
def _diversity_merge(self, existing_score, new_score, existing_prov, new_prov):
    # Tier 1: Different corpus = full independence = SUM
    if existing_prov.corpus_id != new_prov.corpus_id:
        return existing_score + new_score

    # Tier 2: Same corpus, disjoint sources = partial boost
    if not existing_prov.data_sources.intersection(new_prov.data_sources):
        return (existing_score + new_score + max(existing_score, new_score)) / 2

    # Tier 3: Overlapping sources = echo = MAX only
    return max(existing_score, new_score)
```

## Prolog Service Definition

Define a distributed KG service in Prolog:

```prolog
service(data_format_expert, [
    transport(http('/kg', [host('0.0.0.0'), port(8081)])),

    % Service discovery
    discovery_enabled(true),
    discovery_backend(consul),
    discovery_tags([kg_node, expert_system]),
    discovery_metadata([
        semantic_centroid("base64_encoded..."),
        embedding_model('all-MiniLM-L6-v2'),
        interface_topics([csv, json, xml]),
        corpus_id('data_formats_v2'),
        data_sources([docs, examples, tutorials])
    ]),

    % Kleinberg routing
    routing(kleinberg([
        alpha(2.0),
        max_hops(10),
        similarity_threshold(0.5),
        path_folding(true)
    ])),

    % Federation
    federation_enabled(true),
    federation_k(5),
    aggregation(diversity),
    consensus_threshold(3)
], [
    receive(Query),
    handle_kg_query(Query, Response),
    respond(Response)
]).
```

## Flask Service Example

A complete Flask service with distributed search:

```python
from flask import Flask, request, jsonify
from kg_topology_api import DistributedKGTopologyAPI
from discovery_clients import create_discovery_client

app = Flask(__name__)

# Initialize
discovery = create_discovery_client("consul", host="consul.local")
api = DistributedKGTopologyAPI(
    db_path="kg.db",
    embeddings_path="embeddings/",
    discovery_client=discovery
)

@app.before_first_request
def register():
    api.register_node(
        interface_id="csv_expert",
        host="0.0.0.0",
        port=8081,
        tags=["kg_node"]
    )

@app.route('/kg/query', methods=['POST'])
def query():
    data = request.get_json()
    results = api.distributed_search(
        query_text=data['query_text'],
        model_name=data.get('model', 'all-MiniLM-L6-v2'),
        top_k=data.get('top_k', 5),
        max_hops=data.get('max_hops', 10)
    )
    return jsonify({
        "results": [r.to_dict() for r in results],
        "node_id": api.node_id
    })

@app.route('/kg/health')
def health():
    return jsonify({
        "status": "healthy",
        "node_id": api.node_id,
        "routing_stats": api.router.get_stats() if api.router else {}
    })

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8081)
```

## Best Practices

### Node Specialization

1. **Domain-focused nodes**: Each node should have a clear semantic focus
2. **Centroid quality**: Use representative questions to compute centroids
3. **Topic tagging**: Add descriptive topics for debugging and monitoring

### Routing Configuration

| Parameter | Typical Value | Notes |
|-----------|---------------|-------|
| `alpha` | 2.0 | Higher = prefer closer nodes |
| `max_hops` | 10 | Increase for larger networks |
| `similarity_threshold` | 0.5 | Lower = explore more nodes |
| `path_folding` | true | Improves latency over time |

### Federation Strategy

1. **Start greedy**: Query closest node first (`local_first=True`)
2. **Expand on miss**: If local search fails, federate
3. **Track diversity**: Monitor `diversity_score` for echo detection
4. **Corpus metadata**: Always set `corpus_id` for accurate scoring

## Summary

This chapter covered:

- **Multi-interface nodes**: One system, multiple semantic domains
- **Kleinberg routing**: Greedy forwarding with O(log²n) path length
- **Path folding**: Dynamic shortcuts for improved latency
- **Federated queries**: Parallel execution with diversity-weighted aggregation
- **Prolog integration**: Declarative service definitions

## What's Next

For proper Kleinberg routing with guaranteed O(log²n) path length, see:

- **[Chapter 13: Advanced Routing Algorithms](13_advanced_routing.md)** - Proper small-world networks (k_local + k_long) that enable true Kleinberg routing, HNSW layered routing, and O(log n) scaling
- **[Chapter 14: Scale-Free Networks](14_scale_free_networks.md)** - Power-law interface distribution, capacity-proportional sizing, and hub optimization

## Related Documentation

- [Book 7, Chapter 12d](../book-07-cross-target-glue/12d_kg_topology.md) - Infrastructure details
- [SMALL_WORLD_ROUTING.md](../../docs/proposals/SMALL_WORLD_ROUTING.md) - Theoretical foundation
- [FEDERATED_QUERY_ALGEBRA.md](../../docs/proposals/FEDERATED_QUERY_ALGEBRA.md) - Query algebra
