# Chapter 5: Graph Visualization

Use WASM string support to build an interactive graph visualization with Cytoscape.js.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                      Prolog (Source)                        │
│   parent(tom, bob).  ancestor(X, Y) :- parent(X, Y).       │
└─────────────────────┬───────────────────────────────────────┘
                      │ compile_wasm_string_module/3
                      ▼
┌─────────────────────────────────────────────────────────────┐
│                     LLVM IR                                 │
│   @edges, @addEdge, @getEdge, @alloc                       │
└─────────────────────┬───────────────────────────────────────┘
                      │ llc -march=wasm32 + wasm-ld
                      ▼
┌─────────────────────────────────────────────────────────────┐
│                   WASM Module                               │
│   family_graph.wasm                                        │
└─────────────────────┬───────────────────────────────────────┘
                      │ GraphWasm.load()
                      ▼
┌─────────────────────────────────────────────────────────────┐
│              TypeScript + Cytoscape.js                      │
│   Interactive directed graph                               │
└─────────────────────────────────────────────────────────────┘
```

## Edge Storage API

WASM exports these functions for graph data:

```typescript
// Add an edge
graph.addEdge('tom', 'bob');

// Get all edges
graph.getEdges(); // [['tom', 'bob'], ['bob', 'alice']]

// Count edges
graph.getEdgeCount(); // 2
```

## Cytoscape.js Integration

```typescript
import cytoscape from 'cytoscape';

const graph = await GraphWasm.load('family_graph.wasm');

// Add family tree
graph.addEdge('tom', 'bob');
graph.addEdge('bob', 'alice');
graph.addEdge('alice', 'eve');

// Convert to Cytoscape format
const edges = graph.getEdges();
const nodes = new Set<string>();
edges.forEach(([from, to]) => { nodes.add(from); nodes.add(to); });

// Render
const cy = cytoscape({
  container: document.getElementById('graph'),
  elements: [
    ...Array.from(nodes).map(id => ({ data: { id } })),
    ...edges.map(([from, to], i) => ({
      data: { id: `e${i}`, source: from, target: to }
    }))
  ],
  style: [
    { selector: 'node', style: { 'label': 'data(id)' } },
    { selector: 'edge', style: { 
      'target-arrow-shape': 'triangle',
      'curve-style': 'bezier'
    }}
  ],
  layout: { name: 'cose', animate: true }
});
```

## Running the Demo

```bash
cd examples/wasm-graph
npx serve .
# Open http://localhost:3000
```

Click **"Load Sample Data"** to see the family tree graph.

## Use Cases

| Use Case | Description |
|----------|-------------|
| **Knowledge Graphs** | Visualize Prolog facts as networks |
| **Transitive Closure** | Show all reachable nodes from a query |
| **Debugging** | Visualize rule evaluation paths |
| **Education** | Interactive Prolog graph exploration |

---

**Next**: [Chapter 6: Cross-Target Integration](06_cross_target.md)
