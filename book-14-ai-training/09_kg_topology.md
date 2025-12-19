<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 9: Knowledge Graph Topology

This chapter extends the LDA training concepts from earlier chapters with **Knowledge Graph (KG) Topology** - a structure that captures relationships between Q/A pairs to enable learning paths, prerequisite discovery, and semantic navigation.

## Motivation

### The Flat Cluster Problem

In chapters 3-4, we organized questions into clusters based on semantic similarity. But clusters alone don't capture:

- **Prerequisites**: What should I learn before this?
- **Extensions**: What can I learn after this?
- **Dependencies**: What concepts does this answer assume?
- **Alternatives**: What other approaches exist?

### From Clusters to Knowledge Graph

KG Topology adds **typed relationships** between answers:

```
                    ┌─────────────────┐
                    │ crypto_signing  │
                    │  (foundational) │
                    └────────┬────────┘
                             │
                             ▼
┌──────────────┐    ┌─────────────────┐    ┌─────────────────┐
│ http_headers │───▶│   jwt_basics    │───▶│  jwt_refresh    │
│(foundational)│    │     (main)      │    │ (compositional) │
└──────────────┘    └────────┬────────┘    └─────────────────┘
                             │
        ┌────────────────────┼────────────────────┐
        │                    │                    │
        ▼                    ▼                    ▼
┌──────────────┐    ┌─────────────────┐    ┌─────────────────┐
│setup_auth_lib│    │secure_endpoints │    │token_revocation │
│(preliminary) │    │ (transitional)  │    │ (transitional)  │
└──────────────┘    └─────────────────┘    └─────────────────┘
```

## Relationship Types

### Learning Flow Relations

| Type | Direction | Description | Example |
|------|-----------|-------------|---------|
| **foundational** | A ← B | B depends on principles from A | JWT auth ← cryptographic signing |
| **preliminary** | A ← B | B requires completing A first | parse CSV ← install pandas |
| **compositional** | A → B | B extends or builds upon A | basic query → query with joins |
| **transitional** | A → B | B is a natural next step after A | create user → assign permissions |

### Scope Relations

| Type | Direction | Description | Example |
|------|-----------|-------------|---------|
| **refined** | A → B | B is a more specific variant of A | read CSV → read CSV with headers |
| **general** | A ← B | A is broader in scope than B | parse delimited data ← parse CSV |

### Abstraction Relations

| Type | Direction | Description | Example |
|------|-----------|-------------|---------|
| **generalization** | A → B | B is an abstract pattern derived from A | JWT refresh → Token refresh pattern |
| **implementation** | A ← B | A is code that realizes pattern B | JWT refresh code ← Token pattern |
| **axiomatization** | A → B | B is abstract theory derived from A | Arithmetic → Ring theory |
| **instance** | A ← B | A is a domain that satisfies theory B | Arithmetic ← Ring theory |
| **example** | A ← B | A illustrates/demonstrates concept B | JWT tutorial ← Token-based auth |

## Database Schema

### Answer Relations Table

```sql
-- Extend the existing answer_relations table
CREATE TABLE answer_relations (
    relation_id INTEGER PRIMARY KEY,
    from_answer_id INTEGER REFERENCES answers(answer_id),
    to_answer_id INTEGER REFERENCES answers(answer_id),
    relation_type TEXT NOT NULL CHECK(relation_type IN (
        -- Learning flow
        'foundational', 'preliminary', 'compositional', 'transitional',
        -- Scope
        'refined', 'general',
        -- Abstraction
        'generalization', 'implementation', 'axiomatization', 'instance', 'example'
    )),
    strength REAL DEFAULT 1.0,
    metadata TEXT,  -- JSON for extra info
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    UNIQUE(from_answer_id, to_answer_id, relation_type)
);

CREATE INDEX idx_relations_from ON answer_relations(from_answer_id);
CREATE INDEX idx_relations_to ON answer_relations(to_answer_id);
CREATE INDEX idx_relations_type ON answer_relations(relation_type);
```

### Seed Level Tracking

```sql
-- Track provenance (how far from original dataset)
CREATE TABLE question_seed_levels (
    question_id INTEGER PRIMARY KEY REFERENCES questions(question_id),
    seed_level INTEGER NOT NULL DEFAULT 0,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- seed(0) = original curated data
-- seed(n) = discovered n expansion steps from seed(0)
```

### Answer Anchors

```sql
-- Link answers to their anchor questions (hash-based)
CREATE TABLE answer_anchors (
    answer_id INTEGER PRIMARY KEY REFERENCES answers(answer_id),
    anchor_question_hash TEXT NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);
```

## API Usage

### Adding Relations

```python
from kg_topology_api import KGTopologyAPI

api = KGTopologyAPI(db_path="kg.db", embeddings_path="embeddings/")

# Create answers
jwt_basics = api.add_answer("playbooks/auth/jwt.md", "JWT authentication example...")
crypto_signing = api.add_answer("playbooks/crypto/signing.md", "Cryptographic signing...")
jwt_refresh = api.add_answer("playbooks/auth/refresh.md", "JWT refresh tokens...")

# Add conceptual relations
api.add_kg_relation(crypto_signing, jwt_basics, 'foundational')  # signing is foundational to JWT
api.add_kg_relation(jwt_basics, jwt_refresh, 'compositional')    # refresh extends JWT
```

### Querying Relations

```python
# Get prerequisites for an answer
foundational = api.get_foundational(jwt_basics)
prerequisites = api.get_prerequisites(jwt_basics)

# Get next steps
extensions = api.get_extensions(jwt_basics)
next_steps = api.get_next_steps(jwt_basics)

# Get complete learning path
path = api.get_learning_path(jwt_basics, include_foundational=True, include_prerequisites=True)
```

### Search with Context

```python
# Search returns graph context
results = api.search_with_context("How do I add JWT auth?", projection_id)

# Result includes relations:
{
    "answer_id": 42,
    "score": 0.92,
    "text": "To implement JWT authentication...",
    "foundational": [
        {"answer_id": 10, "name": "Cryptographic Signing"},
        {"answer_id": 11, "name": "HTTP Headers Basics"}
    ],
    "prerequisites": [
        {"answer_id": 20, "name": "Install JWT Library"}
    ],
    "extensions": [
        {"answer_id": 50, "name": "JWT Refresh Tokens"}
    ],
    "next_steps": [
        {"answer_id": 60, "name": "Secure API Endpoints"}
    ]
}
```

## Seed Question Topology

### What is seed(n)?

The **seed level** tracks how far a question is from the original curated dataset:

- **seed(0)**: Original Q/A pairs - the foundation
- **seed(1)**: Questions discovered by exploring from seed(0)
- **seed(2)**: Questions discovered by exploring from seed(1)
- And so on...

This is **provenance tracking**, not a relationship chain.

### Clarification: What seed(0) is NOT

- NOT a single "anchor" question per answer
- NOT weighted higher than other seeds (all questions are equal for training)
- NOT the "primary key" for KG relations

### Training Data Organization

```
training_data/
├── seed_0/           # Original curated data (highest priority)
│   ├── cluster_001/
│   └── cluster_002/
├── seed_1/           # First expansion
│   └── ...
└── seed_n/           # Nth expansion (lowest priority for pruning)
```

Pruning priority: Delete seed(n) first, preserve seed(0).

## Integration with Distributed Search

KG Topology integrates with the distributed infrastructure from Chapter 6 (Book 13):

### Multi-Interface Routing

Each interface can have its own KG topology:

```python
from kg_topology_api import DistributedKGTopologyAPI

api = DistributedKGTopologyAPI(db_path="kg.db", embeddings_path="embeddings/")

# Register node
api.register_node(
    interface_id="auth_expert",
    host="0.0.0.0",
    port=8081,
    tags=["kg_node", "auth"]
)

# Distributed search with context
results = api.distributed_search_with_context(
    query_text="How do I implement JWT?",
    model_name="all-MiniLM-L6-v2",
    top_k=5,
    include_relations=True
)
```

### Federated Relation Traversal

Relations can span nodes in a federated network:

```python
# Get foundational concepts (may be on other nodes)
foundational = api.get_foundational_federated(answer_id, max_hops=3)

# Returns results from multiple nodes with provenance
for concept in foundational:
    print(f"Concept: {concept.text}")
    print(f"Source node: {concept.source_node}")
    print(f"Relation: {concept.relation_type}")
```

## Knowledge Gap Detection

Find areas that need more content:

```sql
-- Find answers with no outgoing relations (dead ends)
SELECT a.answer_id, a.source_file
FROM answers a
LEFT JOIN answer_relations ar ON a.answer_id = ar.from_answer_id
WHERE ar.relation_id IS NULL;

-- Find answers with no foundational relations (orphaned concepts)
SELECT a.answer_id, a.source_file
FROM answers a
LEFT JOIN answer_relations ar ON
    a.answer_id = ar.to_answer_id AND
    ar.relation_type = 'foundational'
WHERE ar.relation_id IS NULL;
```

```python
def find_knowledge_gaps(api):
    """Identify content gaps for prioritized creation."""
    return {
        'orphaned': api.query_answers_without('foundational', 'incoming'),
        'dead_ends': api.query_answers_without('transitional', 'outgoing'),
        'no_extensions': api.query_answers_without('compositional', 'outgoing'),
    }
```

## Prolog Service Definition

```prolog
service(learning_path_expert, [
    transport(http('/kg', [host('0.0.0.0'), port(8082)])),

    % Discovery
    discovery_enabled(true),
    discovery_backend(consul),
    discovery_tags([kg_node, learning_paths]),
    discovery_metadata([
        semantic_centroid("..."),
        embedding_model('all-MiniLM-L6-v2'),
        interface_topics([auth, jwt, security]),
        supported_relations([foundational, preliminary, compositional, transitional])
    ]),

    % Routing
    routing(kleinberg([
        alpha(2.0),
        max_hops(10),
        path_folding(true)
    ]))
], [
    receive(Query),
    search_with_context(Query, Results),
    respond(Results)
]).
```

## Best Practices

### Relation Curation

1. **Start with core concepts**: Identify the foundational answers first
2. **Build incrementally**: Add relations as you discover connections
3. **Use bidirectional thinking**: If A is foundational to B, B is compositional from A
4. **Validate with users**: Learning paths should match how experts think

### Seed Level Management

1. **Protect seed(0)**: Original data is the foundation
2. **Track expansion**: Monitor how the KG grows
3. **Prune carefully**: Remove distant seeds when storage is limited
4. **Quality gradient**: Lower seeds = higher quality (typically)

### Integration with Training

1. **All seeds are equal**: No weighting by seed level for training
2. **Relations inform smoothing**: Related answers should have similar embeddings
3. **Gap detection**: Missing relations indicate content opportunities

## Summary

This chapter covered:

- **11 relation types** for knowledge graph structure
- **Seed level tracking** for provenance
- **Learning path generation** from relations
- **Knowledge gap detection** for content prioritization
- **Integration with distributed search** (federated traversal)
- **Prolog service definitions** for KG topology

## Related Documentation

- [QA_KNOWLEDGE_GRAPH.md](../../docs/proposals/QA_KNOWLEDGE_GRAPH.md) - Full proposal
- [SEED_QUESTION_TOPOLOGY.md](../../docs/proposals/SEED_QUESTION_TOPOLOGY.md) - Seed level details
- [SMALL_WORLD_ROUTING.md](../../docs/proposals/SMALL_WORLD_ROUTING.md) - Routing theory
- [Book 7, Chapter 12d](../book-07-cross-target-glue/12d_kg_topology.md) - Infrastructure
- [Book 13, Chapter 6](../book-13-semantic-search/06_distributed_search.md) - Distributed search
