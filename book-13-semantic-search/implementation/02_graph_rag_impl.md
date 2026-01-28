<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Graph RAG - Implementation Details

This document provides function-level documentation for Graph Retrieval-Augmented Generation.

**Source**: `src/unifyweaver/targets/python_semantic_target.pl`, `src/unifyweaver/runtime/pt_searcher.py`

---

## Overview: Graph RAG vs Standard RAG

| Feature | Standard RAG | Graph RAG |
|---------|--------------|-----------|
| Retrieval | Vector similarity only | Vector + graph traversal |
| Context | Isolated documents | Documents + relationships |
| Missing links | Yes (related docs missed) | No (neighbors included) |
| Use case | Simple Q&A | Complex knowledge bases |

---

## semantic_search/3

Standard vector similarity search.

### Signature

```prolog
semantic_search(+Query, +TopK, -Results)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Query` | `string` | Natural language search query |
| `TopK` | `integer` | Number of results to return |
| `Results` | `list` | List of matching documents |

### Algorithm

1. **Embed query**: Convert query text to vector using embedding model
2. **Vector search**: Find TopK nearest neighbors in embedding space
3. **Return results**: List of document IDs and content

### Generated Python

```python
results = _get_runtime().searcher.semantic_search(query, top_k=TopK)
```

### SQL Execution

```sql
SELECT id, content,
       (1 - cosine_distance(embedding, ?)) as score
FROM embeddings
ORDER BY score DESC
LIMIT ?
```

### Example

```prolog
?- semantic_search("Apollo mission", 5, Results).
% Results = [doc_123, doc_456, doc_789, ...]
```

---

## graph_search/4

Hybrid vector + graph traversal search (Graph RAG).

### Signature

```prolog
graph_search(+Query, +TopK, +Hops, -Results)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Query` | `string` | Natural language search query |
| `TopK` | `integer` | Number of anchor documents |
| `Hops` | `integer` | Graph traversal depth |
| `Results` | `dict` | Structured results with focus, parents, children |

### Algorithm

1. **Anchor**: Use `semantic_search` to find TopK entry nodes
2. **Expand**: For each hop level:
   - Query `links` table for parents (`target_id` where `source_id` matches)
   - Query `links` table for children (`source_id` where `target_id` matches)
3. **Deduplicate**: Remove duplicate nodes across hops
4. **Return**: Structured result with focus nodes and neighbors

### Generated Python

```python
results = _get_runtime().searcher.graph_search(query, top_k=TopK, hops=Hops)
```

### SQL Execution

```sql
-- Step 1: Vector search for anchors
SELECT id FROM embeddings WHERE ... LIMIT ?

-- Step 2: Find parents (1 hop)
SELECT target_id FROM links WHERE source_id IN (anchor_ids)

-- Step 3: Find children (1 hop)
SELECT source_id FROM links WHERE target_id IN (anchor_ids)
```

### Result Structure

```python
{
    "focus": [doc_123, doc_456],      # Anchor documents
    "parents": [doc_001, doc_002],    # Parent nodes
    "children": [doc_789, doc_790]    # Child nodes
}
```

### Example

```prolog
?- graph_search("Hacktivism", 3, 1, Results).
% Results = {
%   focus: ["Anonymous Culture", "Hacktivism Tree"],
%   parents: ["Politics"],
%   children: ["Labomedia", "Tetalab"]
% }
```

---

## PtSearcher Class

Python runtime class implementing search operations.

### Location

`src/unifyweaver/runtime/pt_searcher.py`

### Constructor

```python
class PtSearcher:
    def __init__(self, db_path: str, embedding_model: str = "default"):
        self.db_path = db_path
        self.embedding_model = embedding_model
        self.conn = sqlite3.connect(db_path)
```

### Methods

| Method | Description |
|--------|-------------|
| `semantic_search(query, top_k)` | Vector similarity search |
| `graph_search(query, top_k, hops)` | Graph RAG search |
| `get_document(doc_id)` | Retrieve document by ID |
| `get_neighbors(doc_id, direction)` | Get linked documents |

---

## Links Table Schema

The `links` table stores document relationships.

### Schema

```sql
CREATE TABLE links (
    source_id TEXT,
    target_id TEXT,
    link_type TEXT DEFAULT 'related',
    PRIMARY KEY (source_id, target_id)
);
```

### Relationship Semantics

| source_id | target_id | Meaning |
|-----------|-----------|---------|
| Child | Parent | Child belongs to Parent |
| Page A | Page B | Page A references Page B |

### Index

```sql
CREATE INDEX idx_links_source ON links(source_id);
CREATE INDEX idx_links_target ON links(target_id);
```

---

## Ingestion: PtCrawler

The crawler extracts links during document ingestion.

### Link Extraction

```python
def extract_links(self, doc):
    links = []
    # Extract from RDF attributes
    for attr in doc.find_all(attrs={'rdf:resource': True}):
        target = attr['rdf:resource']
        links.append((doc.id, target))
    return links
```

### Crawl Depth

```prolog
crawler_run(['data.rdf'], Depth).
```

- `Depth=0`: Index only specified files
- `Depth=1`: Index files + their direct links
- `Depth=N`: Recursive up to N levels

---

## Graph Traversal Algorithm

### Pseudocode

```
function graph_search(query, top_k, hops):
    # Phase 1: Anchor
    anchors = semantic_search(query, top_k)

    # Phase 2: Expand
    visited = set(anchors)
    parents = set()
    children = set()

    current = anchors
    for hop in range(hops):
        # Get parents
        for node in current:
            for parent in get_parents(node):
                if parent not in visited:
                    parents.add(parent)
                    visited.add(parent)

        # Get children
        for node in current:
            for child in get_children(node):
                if child not in visited:
                    children.add(child)
                    visited.add(child)

        current = parents | children

    # Phase 3: Return
    return {
        "focus": anchors,
        "parents": list(parents),
        "children": list(children)
    }
```

### Time Complexity

- Vector search: O(n log k) with approximate nearest neighbors
- Graph expansion: O(h × d) where h=hops, d=average degree
- Total: O(n log k + h × d × k)

---

## LLM Integration

### llm_ask/3

Sends context to LLM for summarization.

```prolog
llm_ask(+Prompt, +Context, -Response)
```

### Workflow

```prolog
summarize(Topic, Summary) :-
    graph_search(Topic, 3, 1, Context),
    Prompt = 'Summarize the topic using the provided context.',
    llm_ask(Prompt, Context, Summary).
```

### Context Formatting

The `Context` dict is serialized to JSON and included in the LLM prompt:

```python
full_prompt = f"{prompt}\n\nContext:\n{json.dumps(context, indent=2)}"
response = llm.complete(full_prompt)
```

---

## Example: Hacktivism Query

### Step-by-Step

1. **Query**: "Hacktivism"
2. **Vector Search** (TopK=3):
   - Returns: ["Anonymous Culture", "Hacktivism Tree", "Cyber Activism"]
3. **Graph Expansion** (Hops=1):
   - Parents of "Hacktivism Tree": ["Politics"]
   - Children of "Hacktivism Tree": ["Labomedia", "Tetalab"]
4. **Result**:
   ```json
   {
     "focus": ["Anonymous Culture", "Hacktivism Tree", "Cyber Activism"],
     "parents": ["Politics"],
     "children": ["Labomedia", "Tetalab"]
   }
   ```
5. **LLM Output**: "Hacktivism groups include Labomedia and Tetalab, which are part of the broader Politics category..."

### Why Graph RAG Matters

Without graph traversal, "Labomedia" and "Tetalab" might be missed because:
- They're small pages with minimal text
- Their embeddings don't match "Hacktivism" closely
- But they're **children** of the Hacktivism tree

---

## Configuration

### Database Path

```python
searcher = PtSearcher(db_path="data.db")
```

### Embedding Model

```python
searcher = PtSearcher(db_path="data.db", embedding_model="sentence-transformers/all-MiniLM-L6-v2")
```

### Search Parameters

| Parameter | Recommended | Description |
|-----------|-------------|-------------|
| `top_k` | 3-10 | More anchors = broader context |
| `hops` | 1-2 | More hops = deeper relationships |

---

## Related Documentation

- [Book 13 Chapter 1: Introduction](../01_introduction.md)
- [Book 13 Chapter 3: Semantic Data Pipeline](../03_semantic_data_pipeline.md)
- [Python Semantic Target Source](../../../../src/unifyweaver/targets/python_semantic_target.pl)
