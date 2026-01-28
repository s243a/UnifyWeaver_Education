<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Graph RAG - Questions

Q&A companion for [02_graph_rag_impl.md](./02_graph_rag_impl.md).

---

## Question Index

1. [What is the difference between standard RAG and Graph RAG?](#b13c02-q-rag-difference)
2. [What does semantic_search/3 do?](#b13c02-q-semantic-search)
3. [What does graph_search/4 do?](#b13c02-q-graph-search)
4. [How does graph traversal work in Graph RAG?](#b13c02-q-traversal)
5. [What is the structure of Graph RAG results?](#b13c02-q-result-structure)
6. [How is the links table structured?](#b13c02-q-links-table)
7. [How does the PtCrawler extract links?](#b13c02-q-crawler)
8. [What is the time complexity of graph_search?](#b13c02-q-complexity)
9. [How do I integrate Graph RAG with an LLM?](#b13c02-q-llm-integration)
10. [What parameters should I use for graph_search?](#b13c02-q-parameters)
11. [Why might standard vector search miss relevant documents?](#b13c02-q-missing-context)
12. [How do I configure the embedding model?](#b13c02-q-embedding-model)

---

## Questions and Answers

### <a id="b13c02-q-rag-difference"></a>Q1: What is the difference between standard RAG and Graph RAG?

**Answer**: The key differences are:

| Feature | Standard RAG | Graph RAG |
|---------|--------------|-----------|
| Retrieval | Vector similarity only | Vector + graph traversal |
| Context | Isolated documents | Documents + relationships |
| Missing links | Related docs can be missed | Neighbors automatically included |

Standard RAG finds documents similar to the query. Graph RAG also retrieves documents **connected** to those matches through relationships (parent/child, citations, etc.).

**See**: [Overview: Graph RAG vs Standard RAG](./02_graph_rag_impl.md#overview-graph-rag-vs-standard-rag)

---

### <a id="b13c02-q-semantic-search"></a>Q2: What does semantic_search/3 do?

**Answer**: `semantic_search/3` performs standard vector similarity search:

```prolog
semantic_search(+Query, +TopK, -Results)
```

It:
1. Embeds the query text into a vector
2. Finds the TopK nearest neighbors in embedding space
3. Returns matching document IDs

This is the building block for Graph RAG's anchor phase.

**See**: [semantic_search/3](./02_graph_rag_impl.md#semantic_search3)

---

### <a id="b13c02-q-graph-search"></a>Q3: What does graph_search/4 do?

**Answer**: `graph_search/4` implements Graph RAG:

```prolog
graph_search(+Query, +TopK, +Hops, -Results)
```

It:
1. **Anchors**: Uses `semantic_search` to find TopK entry nodes
2. **Traverses**: Expands N hops through the graph to find parents and children
3. **Returns**: Structured result with focus nodes and neighbors

**See**: [graph_search/4](./02_graph_rag_impl.md#graph_search4)

---

### <a id="b13c02-q-traversal"></a>Q4: How does graph traversal work in Graph RAG?

**Answer**: The traversal algorithm:

1. Start with anchor documents from vector search
2. For each hop level:
   - Query `links` table for parents (`SELECT target_id WHERE source_id IN anchors`)
   - Query `links` table for children (`SELECT source_id WHERE target_id IN anchors`)
3. Deduplicate nodes across hops
4. Return all discovered nodes

The `hops` parameter controls depth: `hops=1` means direct neighbors, `hops=2` means neighbors of neighbors.

**See**: [Graph Traversal Algorithm](./02_graph_rag_impl.md#graph-traversal-algorithm)

---

### <a id="b13c02-q-result-structure"></a>Q5: What is the structure of Graph RAG results?

**Answer**: `graph_search/4` returns a dict with three keys:

```python
{
    "focus": [doc_123, doc_456],      # Anchor documents (vector search results)
    "parents": [doc_001, doc_002],    # Parent nodes (linked as targets)
    "children": [doc_789, doc_790]    # Child nodes (linked as sources)
}
```

This structure allows the LLM to understand document relationships.

**See**: [Result Structure](./02_graph_rag_impl.md#result-structure)

---

### <a id="b13c02-q-links-table"></a>Q6: How is the links table structured?

**Answer**: The `links` table stores document relationships:

```sql
CREATE TABLE links (
    source_id TEXT,
    target_id TEXT,
    link_type TEXT DEFAULT 'related',
    PRIMARY KEY (source_id, target_id)
);
```

Semantics:
- `source_id` = Child/referencing document
- `target_id` = Parent/referenced document

Example: If "Apollo 11" is a child of "Space Program", then `source_id='apollo11'`, `target_id='space_program'`.

**See**: [Links Table Schema](./02_graph_rag_impl.md#links-table-schema)

---

### <a id="b13c02-q-crawler"></a>Q7: How does the PtCrawler extract links?

**Answer**: The crawler extracts links from RDF attributes during ingestion:

```python
def extract_links(self, doc):
    for attr in doc.find_all(attrs={'rdf:resource': True}):
        target = attr['rdf:resource']
        links.append((doc.id, target))
```

Common sources:
- `rdf:resource` attributes in RDF/XML
- `href` attributes in HTML
- Explicit link definitions in data files

**See**: [Ingestion: PtCrawler](./02_graph_rag_impl.md#ingestion-ptcrawler)

---

### <a id="b13c02-q-complexity"></a>Q8: What is the time complexity of graph_search?

**Answer**: The complexity is:

- **Vector search**: O(n log k) with approximate nearest neighbors
- **Graph expansion**: O(h × d) where h=hops, d=average degree
- **Total**: O(n log k + h × d × k)

For typical parameters (top_k=5, hops=1, average_degree=10):
- Vector search dominates for large datasets
- Graph expansion is fast due to indexed lookups

**See**: [Time Complexity](./02_graph_rag_impl.md#time-complexity)

---

### <a id="b13c02-q-llm-integration"></a>Q9: How do I integrate Graph RAG with an LLM?

**Answer**: Use `llm_ask/3` with the graph search results:

```prolog
summarize(Topic, Summary) :-
    graph_search(Topic, 3, 1, Context),
    Prompt = 'Summarize the topic using the provided context.',
    llm_ask(Prompt, Context, Summary).
```

The Context dict is serialized to JSON and included in the LLM prompt, giving it access to focus documents, parents, and children.

**See**: [LLM Integration](./02_graph_rag_impl.md#llm-integration)

---

### <a id="b13c02-q-parameters"></a>Q10: What parameters should I use for graph_search?

**Answer**: Recommended values:

| Parameter | Recommended | Description |
|-----------|-------------|-------------|
| `top_k` | 3-10 | More anchors = broader context |
| `hops` | 1-2 | More hops = deeper relationships |

Start with `top_k=3, hops=1` and increase if:
- Results are too narrow
- Important related documents are missing
- Knowledge graph has deep hierarchies

**See**: [Search Parameters](./02_graph_rag_impl.md#search-parameters)

---

### <a id="b13c02-q-missing-context"></a>Q11: Why might standard vector search miss relevant documents?

**Answer**: Vector search can miss relevant documents when:

1. **Low textual similarity**: A "Budget Report 1968" about Apollo doesn't mention "Apollo" often
2. **Sparse content**: Child pages with just URLs or titles
3. **Indirect relationships**: Documents related through structure, not content

Graph RAG solves this by following **links** regardless of textual similarity. A budget report linked as a child of "Apollo Project" will be retrieved even if its embeddings don't match.

**See**: [Example: Hacktivism Query](./02_graph_rag_impl.md#example-hacktivism-query)

---

### <a id="b13c02-q-embedding-model"></a>Q12: How do I configure the embedding model?

**Answer**: Pass the model name to the PtSearcher constructor:

```python
searcher = PtSearcher(
    db_path="data.db",
    embedding_model="sentence-transformers/all-MiniLM-L6-v2"
)
```

The default model is optimized for general-purpose semantic search. For domain-specific applications, consider fine-tuned models.

**See**: [Configuration](./02_graph_rag_impl.md#configuration)

---

## Summary

Graph RAG extends standard vector search with relationship traversal:
- `semantic_search/3` for vector-only retrieval
- `graph_search/4` for vector + graph traversal
- Links table stores parent/child relationships
- LLM integration via `llm_ask/3`
