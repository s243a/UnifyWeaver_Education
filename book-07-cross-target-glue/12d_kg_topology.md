<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 12d: Knowledge Graph Topology

This chapter extends the client-server architecture with **Knowledge Graph (KG) Topology** - a distributed network for semantic search using Kleinberg small-world routing.

## Overview

Building on the service discovery infrastructure from Chapter 12c, KG Topology enables:

- **Multi-interface semantic nodes**: A single expert system can serve multiple semantic domains
- **Kleinberg routing**: Greedy forwarding to the nearest interface by embedding similarity
- **Path folding**: Dynamic shortcuts that improve routing efficiency over time
- **Federated queries**: Parallel queries across nodes with diversity-weighted aggregation

## Core Concepts

### Small-World Networks

KG Topology applies principles from [Kleinberg's small-world research](https://www.cs.cornell.edu/home/kleinber/icm06-swn.pdf):

| Concept | Application |
|---------|-------------|
| Node location (0.0-1.0) | Interface centroid in embedding space |
| Content key | Query embedding |
| Greedy routing | Forward to most similar centroid |
| Long-range links (1/r^α) | Cross-domain bridge questions |
| Path folding | Dynamic shortcut creation after matches |
| HTL (Hops-To-Live) | Maximum hops before giving up |

### Multi-Interface Nodes

One principal node (expert system) can serve multiple logical interfaces:

```
┌─────────────────────────────────────────────────────┐
│            Principal Node (Expert System)           │
│                                                     │
│  ┌─────────────┐ ┌─────────────┐ ┌─────────────┐   │
│  │  Interface  │ │  Interface  │ │  Interface  │   │
│  │   "CSV"     │ │   "JSON"    │ │  "Database" │   │
│  │ centroid:   │ │ centroid:   │ │ centroid:   │   │
│  │ [0.2, 0.4]  │ │ [0.3, 0.5]  │ │ [0.7, 0.1]  │   │
│  └──────┬──────┘ └──────┬──────┘ └──────┬──────┘   │
│         │               │               │           │
│         └───────────────┼───────────────┘           │
│                         │                           │
│              Shared Knowledge Base                  │
└─────────────────────────────────────────────────────┘
                          │
                          ▼
                    To Network
```

Each interface:
- Has its own **semantic centroid** (mean embedding of its Q/A pairs)
- Publishes its centroid in **discovery metadata**
- Can be routed to independently

## Prolog Service Definition

### Kleinberg Routing Options

```prolog
service(csv_expert_node, [
    transport(http('/kg', [host('0.0.0.0'), port(8081)])),

    % Service discovery (Phase 7)
    discovery_enabled(true),
    discovery_backend(consul),
    discovery_tags([kg_node, expert_system]),
    discovery_metadata([
        semantic_centroid("base64_encoded_vector..."),
        embedding_model('all-MiniLM-L6-v2'),
        interface_topics([csv, delimited, tabular])
    ]),

    % Kleinberg routing (Phase 3)
    routing(kleinberg([
        alpha(2.0),            % Link distance exponent
        max_hops(10),          % HTL limit
        similarity_threshold(0.5),
        path_folding(true)
    ]))
], [
    receive(Query),
    handle_kg_query(Query, Response),
    respond(Response)
]).
```

### Validation Predicates

The following predicates validate Kleinberg routing options:

```prolog
% Routing strategies
is_valid_routing_strategy(direct).
is_valid_routing_strategy(round_robin).
is_valid_routing_strategy(kleinberg).
is_valid_routing_strategy(kleinberg(Opts)) :-
    is_list(Opts), maplist(is_valid_kleinberg_option, Opts).

% Kleinberg options
is_valid_kleinberg_option(alpha(A)) :- number(A), A > 0.
is_valid_kleinberg_option(max_hops(N)) :- integer(N), N > 0.
is_valid_kleinberg_option(parallel_paths(N)) :- integer(N), N > 0.
is_valid_kleinberg_option(similarity_threshold(T)) :- number(T), T >= 0, T =< 1.
is_valid_kleinberg_option(path_folding(Bool)) :- (Bool = true ; Bool = false).

% Discovery metadata entries
is_valid_discovery_metadata_entry(semantic_centroid(C)) :- (is_list(C) ; atom(C)).
is_valid_discovery_metadata_entry(interface_topics(T)) :- is_list(T).
is_valid_discovery_metadata_entry(embedding_model(M)) :- atom(M).
```

## Python Implementation

### Discovery Clients

The `discovery_clients.py` module provides service discovery abstraction:

```python
from discovery_clients import create_discovery_client, ServiceInstance

# Create a local discovery client for testing
client = create_discovery_client("local", ttl_seconds=60)

# Register a KG node
instance = ServiceInstance(
    service_id="kg_csv_node_1",
    service_name="kg_node",
    host="192.168.1.10",
    port=8081,
    tags=["kg_node", "csv_expert"],
    metadata={
        "semantic_centroid": "base64_encoded...",
        "embedding_model": "all-MiniLM-L6-v2",
        "interface_topics": '["csv", "delimited"]'
    }
)
client.register(instance)

# Discover KG nodes
nodes = client.discover("kg_node", tags=["kg_node"])
```

Supported backends:
- `local` - In-memory for testing
- `consul` - HashiCorp Consul
- `etcd` - CoreOS etcd (stub)
- `dns` - DNS-based discovery (stub)

### Kleinberg Router

The `KleinbergRouter` class handles semantic routing:

```python
from kleinberg_router import KleinbergRouter, RoutingEnvelope
from discovery_clients import create_discovery_client

# Initialize router
discovery_client = create_discovery_client("consul", host="consul.local")
router = KleinbergRouter(
    discovery_client=discovery_client,
    alpha=2.0,
    max_hops=10,
    similarity_threshold=0.5,
    path_folding_enabled=True
)

# Discover nodes via service discovery
nodes = router.discover_nodes(tags=['kg_node'])

# Create routing envelope
envelope = RoutingEnvelope(
    origin_node="node_a",
    htl=10,
    visited={"node_a"},
    path_folding_enabled=True
)

# Route a query
query_embedding = embed("How do I parse CSV files?")
results = router.route_query(query_embedding, "How do I parse CSV files?", envelope, top_k=5)

# Path folding shortcuts
router.create_shortcut("How do I parse CSV files?", "csv_node", "csv_interface")
cached = router.check_shortcut("How do I parse CSV files?")
```

### Routing Envelope Protocol

The `RoutingEnvelope` tracks query routing state:

```json
{
    "__type": "kg_query",
    "__id": "uuid-123",
    "__routing": {
        "origin_node": "node_a",
        "htl": 8,
        "visited": ["node_a", "node_b"],
        "path_folding_enabled": true
    },
    "__embedding": {
        "model": "all-MiniLM-L6-v2",
        "vector": [0.1, 0.2, ...]
    },
    "payload": {
        "query_text": "How do I parse CSV?",
        "top_k": 5
    }
}
```

### DistributedKGTopologyAPI

The `DistributedKGTopologyAPI` class extends `KGTopologyAPI` with distributed features:

```python
from kg_topology_api import DistributedKGTopologyAPI

# Initialize distributed API
api = DistributedKGTopologyAPI(
    db_path="kg.db",
    embeddings_path="embeddings/",
    discovery_client=create_discovery_client("consul", host="consul.local"),
    enable_distributed=True
)

# Register node with discovery service
api.register_node(
    interface_id="csv_specialist",
    host="0.0.0.0",
    port=8081,
    tags=["kg_node", "expert_system"]
)

# Distributed search across nodes
results = api.distributed_search(
    query_text="How do I parse CSV?",
    model_name="all-MiniLM-L6-v2",
    top_k=5,
    max_hops=10,
    local_first=True
)

# Handle remote queries
@app.route("/kg/query", methods=["POST"])
def handle_query():
    return api.handle_remote_query(request.get_json())
```

## Go Implementation

### KleinbergRouter Struct

```go
// Generated by UnifyWeaver - compile_kleinberg_router_go/2

package main

import (
    "math"
    "sync"
)

type KGNode struct {
    NodeID         string
    Endpoint       string
    Centroid       []float64
    Topics         []string
    EmbeddingModel string
    Similarity     float64
}

type RoutingEnvelope struct {
    OriginNode         string   `json:"origin_node"`
    HTL                int      `json:"htl"`
    Visited            []string `json:"visited"`
    PathFoldingEnabled bool     `json:"path_folding_enabled"`
}

type KleinbergRouter struct {
    discoveryClient     ServiceRegistry
    alpha               float64
    maxHops             int
    parallelPaths       int
    similarityThreshold float64
    pathFoldingEnabled  bool
    nodeCache           map[string]*KGNode
    shortcuts           map[string]string
    mu                  sync.RWMutex
    queriesRouted       int64
    shortcutsUsed       int64
}

func NewKleinbergRouter(client ServiceRegistry, opts ...RouterOption) *KleinbergRouter {
    r := &KleinbergRouter{
        discoveryClient:     client,
        alpha:               2.0,
        maxHops:             10,
        parallelPaths:       1,
        similarityThreshold: 0.5,
        pathFoldingEnabled:  true,
        nodeCache:           make(map[string]*KGNode),
        shortcuts:           make(map[string]string),
    }
    for _, opt := range opts {
        opt(r)
    }
    return r
}

func (r *KleinbergRouter) CosineSimilarity(a, b []float64) float64 {
    if len(a) != len(b) {
        return 0.0
    }
    var dotProduct, normA, normB float64
    for i := 0; i < len(a); i++ {
        dotProduct += a[i] * b[i]
        normA += a[i] * a[i]
        normB += b[i] * b[i]
    }
    if normA == 0 || normB == 0 {
        return 0.0
    }
    return dotProduct / (math.Sqrt(normA) * math.Sqrt(normB))
}

func (r *KleinbergRouter) DiscoverNodes(tags []string) ([]*KGNode, error) {
    instances, err := r.discoveryClient.Discover("kg_node", tags)
    if err != nil {
        return nil, err
    }
    // Convert instances to KGNodes with centroid decoding
    // ...
}

func (r *KleinbergRouter) GetStats() map[string]interface{} {
    r.mu.RLock()
    defer r.mu.RUnlock()
    return map[string]interface{}{
        "queries_routed":  r.queriesRouted,
        "shortcuts_used":  r.shortcutsUsed,
        "cached_nodes":    len(r.nodeCache),
        "active_shortcuts": len(r.shortcuts),
    }
}
```

## Federated Query Algebra

When queries span multiple nodes, results must be aggregated. Phase 4 introduces **federated query algebra** with pluggable aggregation:

### Aggregation Strategies

| Strategy | Description | Use Case |
|----------|-------------|----------|
| `sum` | Sum scores | Independent sources (boost consensus) |
| `max` | Maximum score | Shared corpus (prevent echo) |
| `avg` | Average score | Balanced aggregation |
| `diversity` | Diversity-weighted | Mixed provenance |

### Diversity-Weighted Aggregation

The 3-tier diversity model handles source independence:

```python
def _diversity_merge(self, existing_score, new_score, existing_prov, new_prov):
    """Merge scores based on source diversity."""

    # Tier 1: Different corpus = independent = full boost
    if existing_prov.corpus_id != new_prov.corpus_id:
        return existing_score + new_score

    # Tier 2: Same corpus, disjoint data sources = partial boost
    if not existing_prov.data_sources.intersection(new_prov.data_sources):
        return (existing_score + new_score + max(existing_score, new_score)) / 2

    # Tier 3: Overlapping sources = echo = no boost
    return max(existing_score, new_score)
```

### Federated Query Engine

```python
from federated_query import FederatedQueryEngine, AggregationConfig, AggregationStrategy

# Configure aggregation
config = AggregationConfig(
    strategy=AggregationStrategy.DIVERSITY_WEIGHTED,
    dedup_key="answer_id",
    consensus_threshold=3,
    diversity_field="corpus_id"
)

# Create engine with router
engine = FederatedQueryEngine(router, config)

# Execute federated query
results = engine.federated_query(
    query_text="How do I parse CSV?",
    query_embedding=embedding,
    top_k=5,
    timeout_ms=30000
)
```

### Prolog Federation Options

```prolog
service(federated_kg, [
    % ... transport, discovery ...

    % Federation options
    federation_enabled(true),
    federation_k(5),                    % Query top 5 nodes
    aggregation(diversity),             % Diversity-weighted
    consensus_threshold(3),             % Require 3+ sources
    timeout_ms(30000),                  % 30 second timeout

    % Corpus tracking for diversity
    discovery_metadata([
        corpus_id('my_dataset_v1'),
        data_sources(['docs', 'wiki']),
        last_updated('2025-12-18')
    ])
], [...]).
```

## HTTP Endpoints

Generated services expose these endpoints:

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/kg/query` | POST | Handle distributed query |
| `/kg/register` | POST | Register node with discovery |
| `/kg/health` | GET | Health check |
| `/kg/federated` | POST | Handle federated query (Phase 4) |
| `/kg/federation/stats` | GET | Federation statistics |

### Example: Python Flask

```python
# Generated by UnifyWeaver - generate_kg_query_endpoint/3

@app.route('/kg/query', methods=['POST'])
def handle_kg_query():
    try:
        request_data = request.get_json()
        query_text = request_data.get('query_text', '')
        top_k = request_data.get('top_k', 5)
        envelope = RoutingEnvelope.from_dict(request_data.get('__routing', {}))

        # Check for shortcut first (path folding)
        shortcut = api.router.check_shortcut(query_text)
        if shortcut:
            return jsonify({"redirect": shortcut, "shortcut_hit": True})

        # Local search
        results = api.semantic_search(query_text, top_k=top_k)

        return jsonify({
            "results": [r.to_dict() for r in results],
            "node_id": api.node_id,
            "interface_id": api.interface_id
        })
    except Exception as e:
        return jsonify({"error": str(e)}), 500

@app.route('/kg/health', methods=['GET'])
def kg_health():
    return jsonify({
        "status": "healthy",
        "node_id": api.node_id,
        "routing_stats": api.router.get_stats() if api.router else {}
    })
```

## Complete Example

```prolog
% Multi-interface expert system with Kleinberg routing
service(data_formats_expert, [
    transport(http('/kg', [host('0.0.0.0'), port(8080)])),

    % Discovery
    discovery_enabled(true),
    discovery_backend(consul),
    discovery_tags([kg_node, data_formats]),

    % Multiple interfaces (multi-centroid node)
    interfaces([
        interface(csv_expert, [
            semantic_centroid("..."),
            topics([csv, delimited, tabular])
        ]),
        interface(json_expert, [
            semantic_centroid("..."),
            topics([json, serialization])
        ]),
        interface(xml_expert, [
            semantic_centroid("..."),
            topics([xml, markup, streaming])
        ])
    ]),

    % Kleinberg routing
    routing(kleinberg([
        alpha(2.0),
        max_hops(10),
        path_folding(true)
    ])),

    % Federation
    federation_enabled(true),
    aggregation(diversity)
], [
    receive(Query),
    route_to_interface(Query, Interface),
    handle_with_interface(Interface, Query, Response),
    respond(Response)
]).
```

## Summary

This chapter covered:

**KG Topology Architecture**:
- Multi-interface nodes with semantic centroids
- Discovery metadata for centroid publishing
- Kleinberg routing with greedy forwarding

**Implementation Components**:
- `discovery_clients.py` - Service discovery abstraction
- `kleinberg_router.py` - Semantic routing with path folding
- `kg_topology_api.py` - Distributed KG API

**Federated Queries (Phase 4)**:
- Aggregation strategies (sum, max, diversity)
- 3-tier diversity model for source independence
- FederatedQueryEngine with parallel execution

**Code Generation**:
- Prolog validation predicates
- Python/Go/Rust router generation
- HTTP endpoint generation

## Related Documentation

- [ROADMAP_KG_TOPOLOGY.md](../../docs/proposals/ROADMAP_KG_TOPOLOGY.md) - Master roadmap
- [SMALL_WORLD_ROUTING.md](../../docs/proposals/SMALL_WORLD_ROUTING.md) - Theoretical foundation
- [FEDERATED_QUERY_ALGEBRA.md](../../docs/proposals/FEDERATED_QUERY_ALGEBRA.md) - Query algebra
- [DENSITY_SCORING_PROPOSAL.md](../../docs/proposals/DENSITY_SCORING_PROPOSAL.md) - Confidence scoring

---

## Navigation

**←** [Previous: Chapter 12c: Discovery and Tracing](12c_discovery_tracing) | [📖 Book 7: Cross-Target Glue](./) | [Next: Chapter 13: API Reference →](13_api_reference)
