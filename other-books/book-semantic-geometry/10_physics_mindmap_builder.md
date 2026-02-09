<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 UnifyWeaver Contributors

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 10: The Physics Mindmap Builder

## From Theory to Tool

Chapters 1-9 developed the geometric foundations: embedding spaces, density manifolds, learned distances, tree algorithms, Fisher information, projection modes, hierarchical structure, and consensus blending. This chapter puts all of them into a single interactive application — the **Physics Mindmap Builder**.

The app exists in two versions: an **open-source version** using cosine distance on Nomic embeddings (reproducible by anyone), and an **enhanced version** using the trained Wikipedia Physics distance model with L² blending (Chapter 4, 9). The open-source version is published at `examples/physics-mindmap/` in the main repository. The enhanced version is not currently published — it lives in the project's private `.local/` directory and is described here for completeness. Both share the same browser UI and tree algorithms, but differ in the distance geometry that drives hierarchy recovery.

This chapter also introduces one concept not covered in earlier chapters: an **entropy-based upward penalty** that suppresses parent-category dominance in the tree view. This mechanism is motivated by the constraints of a list-based mindmap interface, where hub nodes are far more disruptive than in a 2D scatter plot.

## The Open-Source Version

The self-contained version lives at `examples/physics-mindmap/` and requires no project dependencies beyond standard Python libraries. The data generation pipeline:

```bash
pip install datasets sentence-transformers numpy scipy
cd examples/physics-mindmap
python generate_data.py          # generates data/physics_bundle.json
python -m http.server 8080       # serve the app
```

The `generate_data.py` script runs a ten-step pipeline:

1. **Download** — Streams up to 100K articles from `Supabase/wikipedia-en-embeddings` on HuggingFace
2. **Filter** — Two-stage physics selection: keyword matching (exact terms + compound phrases), then semantic similarity to a seed centroid
3. **Embed** — Re-embeds ~300 article titles with `nomic-embed-text-v1.5` (768D), since the HuggingFace dataset uses MiniLM (384D)
4. **Distances** — Pairwise cosine distance matrix from L2-normalized embeddings
5. **Breadth** — k-NN density scores (k=30), normalized to [0, 1] (Chapter 3)
6. **MST** — Minimum spanning tree via scipy, root selected by highest degree (Chapter 5)
7. **Projection** — 2D coordinates via SVD on raw 768D embeddings (Chapter 7)
8. **Density grid** — 100x100 KDE with Scott's bandwidth (Chapter 3)
9. **Peaks** — Local maxima of the density surface
10. **Bundle** — Everything saved to `data/physics_bundle.json` (~6.5 MB)

### Browser Distance Metrics

The app stores the full 768D embeddings in the bundle, enabling on-the-fly metric switching:

| Metric | Formula | Range | Character |
|--------|---------|-------|-----------|
| Cosine | 1 - cos θ | [0, 2] | Standard for embeddings. Pre-computed in bundle. |
| Chord | √(2(1 - cos θ)) | [0, 2] | L2 on unit sphere. Compresses large angles. |
| Angular | arccos(cos θ) | [0, π] | Arc length on unit sphere. Linear with angle. |

Switching metrics recomputes the full N×N distance matrix in the browser from stored embeddings, then rebuilds the tree on the new geometry. Cosine uses the pre-computed bundle matrix; chord and angular trigger browser-side recomputation.

## The Enhanced Version (Not Published)

The enhanced version (`.local/apps/physics-mindmap/`, not published) uses the trained Wikipedia Physics distance model (Chapter 4) to produce richer hierarchy:

```
distance = sqrt(0.5 * d_cosine_norm² + 0.5 * d_wiki_norm²)
```

This is the consensus distance from Chapter 9 restricted to two spaces (cosine + wiki model) with n=2 and equal weights. The pre-computed distance matrix captures both semantic similarity and learned organizational structure.

| Feature | Open-source | Enhanced |
|---------|-------------|----------|
| Distance metric | Cosine only | L² blend (cosine + wiki model) |
| Projections | 768D raw embeddings | 768D + 64D model hidden layer |
| Data source | HuggingFace download | Local NPZ dataset |
| Model dependency | None | `wikipedia_physics_distance.pt` |
| Root emergence | By MST degree (often "Physics") | Consistently "Physics" |
| Requirements | `datasets`, `sentence-transformers` | Full project + PyTorch |

The enhanced version also includes 64D projections from the model's `query_proj` hidden layer (Chapter 7), which encode organizational depth as a spatial gradient (Chapter 4, r=0.85 correlation).

> **Note:** The enhanced version is not currently published. Its `precompute.py` script depends on trained models and internal datasets (`density_core.py`, `wikipedia_physics_distance.pt`) that are not included in the open-source release. The data in the enhanced version's `physics_bundle.json` was generated using the Wikipedia Physics distance model described in Chapter 4, but scripts for reproducing this data generation have not yet been provided.

## Tree Algorithms in the App

Both versions implement the two tree algorithms from Chapter 5 as interactive browser operations:

**MST (global):** The MST edges are pre-computed in the bundle. Re-rooting is done by BFS from the user-selected node through the same edge set — the root changes but the edges are fixed. When `maxBranching` is enforced, children exceeding the limit are orphaned, then re-attached to the closest visited node with available capacity using the distance matrix as fallback. This pushes overflow nodes deeper into the tree rather than discarding them.

**J-guided (local):** Density-ordered greedy insertion, computed on-the-fly from the current distance matrix. Each node is processed in order of k-NN density (densest first) and attaches to its nearest already-placed node. Dense hubs become natural tree centers. This algorithm is recomputed whenever the root, distance metric, or penalty settings change.

Both support interactive controls: `maxDepth` (slider 1-10) limits tree depth, `maxBranching` (slider 2-20 or ∞) limits children per node. Clicking any node in the tree list selects it as the new root, clearing collapsed state and rebuilding the tree.

## Upward Penalty: Suppressing Parent Categories

### The Problem

This is the key algorithmic contribution unique to the mindmap builder. In the density explorer's 2D scatter plot, parent categories appearing in the tree overlay are tolerable — spatial layout provides context, and a long edge to "Physics" is visible as a geometric relationship. But in the mindmap builder's **indented list view**, a parent category appearing as a child is catastrophic for usability.

Consider rooting the tree at "Quantum theory." Without any penalty, "Physics" (breadth=1.0, the densest node in the collection) is processed first in J-guided. At that moment, the root is the only placed node, so "Physics" attaches directly to "Quantum theory" as its first child — regardless of penalty strength. It then brings 7+ sub-children with it, swamping the actual quantum subtopics. The list format demands hierarchy that reflects the selected root's perspective, not the global structure.

### Breadth Scores

The penalty system uses **breadth scores** — a proxy for how "hub-like" each node is:

```
breadth(i) = normalize_01( 1 / mean_k-NN_distance(i, k=30) )
```

This is the inverse of the mean distance to the k nearest neighbors, normalized to [0, 1]. Nodes with many close neighbors (hubs) score high; specific topics with fewer close neighbors score low.

| Article | Breadth | Role |
|---------|---------|------|
| Physics | 1.000 | Broadest hub |
| Energy | 0.839 | Major hub |
| Electron | 0.802 | Major hub |
| Quantum theory | 0.650 | Moderate hub |
| Tyndall effect | 0.000 | Most specific |

The breadth score captures the same signal as Fisher information (Chapter 6) — hub nodes sit at convex boundaries with high geometric influence — but uses a simpler k-NN proxy that can be computed efficiently in the browser.

### The Three-Part Mechanism

Three components work together, controlled by the `↑ Penalty` slider (range 1.0-5.0):

**1. Processing order re-ordering.** When penalty > 1, the J-guided insertion order changes. Instead of pure density order, nodes are split into two groups: specific topics (breadth ≤ root's breadth) go first in density order, then hub topics (breadth > root's breadth) go last. This prevents hubs from monopolizing the root's limited branching slots when only the root is available as a parent.

**2. Edge-level distance inflation.** For each candidate parent-child connection, the distance is multiplied by a penalty factor based on the *worse* of the two endpoints:

```
excess = max(parentExcess, nodeExcess)
penalty = 1 + (upwardPenalty - 1) * excess / entropyRange
```

where `parentExcess = max(0, breadth[parent] - breadth[root])`. This dual check penalizes both connecting *to* a hub parent and placing a hub *as* a child. Nodes at or below the root's breadth pass through unpenalized.

**3. Taint propagation.** Children of high-breadth parents inherit a compounding taint multiplier: `taint[child] = taint[parent] * 1.5` when the parent is broader than the root. Otherwise taint passes through unchanged. This creates exponentially increasing resistance along chains that pass through hub nodes, preventing hubs from forming bridges to large subtrees.

### Effect on Tree Shape

The penalty slider transforms the tree character:

- **Penalty = 1.0** (off): "Quantum theory" as root shows "Physics" as an immediate child with 7 sub-children dominating the list. The tree reflects global structure, not the root's local neighborhood.

- **Penalty = 2.0**: "Physics" is completely absent. "Quantum theory" directly connects to Old quantum theory, String theory, Quantum fluctuation, Wave-particle duality, Classical field theory — its actual subtopics. The tree reflects the root's perspective.

This connects back to Chapter 6: Fisher information identifies hub nodes at convex boundaries. The upward penalty operationalizes this insight, letting users control whether those boundary nodes dominate the tree structure.

## Export Formats

The app exports the current tree in six standard formats:

| Format | Extension | Use Case |
|--------|-----------|----------|
| FreeMind | `.mm` | Open-source mindmap editor, alternating left/right layout |
| SimpleMind | `.smmx` | Mobile/desktop mindmap app, radial layout with depth coloring |
| OPML | `.opml` | Outline processors, RSS readers |
| VUE | `.vue` | Tufts VUE concept mapping tool, 2D positioned nodes |
| Mermaid | `.md` | GitHub/GitLab markdown rendering, flowchart TD |
| JSON | `.json` | Programmatic consumption, nested tree with metadata |

VUE and SimpleMind exports use the 2D SVD coordinates from the bundle to position nodes spatially, preserving the geometric relationships from the density manifold (Chapter 7). All formats include Wikipedia article URLs as node links.

## Geometry in Practice

The two versions of the app demonstrate the book's central thesis at different fidelity levels:

The **open-source version**, using only cosine distance on Nomic embeddings, produces reasonable trees. "Physics" often emerges as the MST root by degree, but not always — cosine distance captures semantic similarity but misses organizational depth. The hierarchy quality varies with the chosen metric.

The **enhanced version**, using the L² blend of cosine and trained model distances, consistently selects "Physics" as root and produces trees that closely match Wikipedia's actual category structure. This confirms the result from Chapter 8: hierarchy is implicit in the learned metric geometry.

The **upward penalty** adds a dimension that neither version has by default: user-controllable perspective. The same geometric data can produce different hierarchical views depending on the selected root and penalty strength. This is useful precisely because knowledge structures are not trees — they are DAGs with multiple valid hierarchical decompositions (Chapter 8). The penalty lets users choose which decomposition to explore.

The export formats close the loop from theory to practice. The geometric hierarchy recovered by algorithms from Chapters 4-5 becomes a FreeMind file, an OPML outline, or a Mermaid diagram — formats that integrate into existing knowledge management workflows.

---

**Previous**: [Chapter 9: Consensus Distances](09_consensus_distances.md)
