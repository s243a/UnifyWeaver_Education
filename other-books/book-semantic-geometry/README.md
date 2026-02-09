<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 UnifyWeaver Contributors

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Semantic Geometry

**Status:** Initial

How metric geometry shapes semantic search — from embedding spaces to learned hierarchies.

## Overview

This book explores the geometric foundations of semantic search, using tools built in the UnifyWeaver project as running examples. It covers how different distance metrics, density estimations, and tree algorithms interact to reveal hierarchical structure in knowledge bases.

The central insight: **hierarchy is implicit in metric geometry**. A generic algorithm like minimum spanning tree can recover Wikipedia's category structure purely from learned pairwise distances — no explicit hierarchy is passed in.

## Prerequisites

- Basic linear algebra (vectors, matrices, SVD)
- Familiarity with embeddings (word2vec, BERT, or similar)
- Python (numpy, torch) for code examples
- Optional: UnifyWeaver density explorer for interactive exploration

## Contents

| # | Chapter | Topics |
|---|---------|--------|
| 1 | [Introduction](01_introduction.md) | Why geometry matters, tool overview |
| 2 | [Embedding Spaces](02_embedding_spaces.md) | Fixed vs learned geometries, Nomic 768D |
| 3 | [Density Manifolds](03_density_manifolds.md) | KDE, bandwidth, convex regions |
| 4 | [Learned Distances](04_learned_distances.md) | Training distance models, organizational depth |
| 5 | [Tree Algorithms](05_tree_algorithms.md) | MST, J-guided, root emergence |
| 6 | [Fisher Information](06_fisher_information.md) | Sensitivity, curvature, hub geometry |
| 7 | [Projection Modes](07_projection_modes.md) | SVD, hidden layers, weight space |
| 8 | [Hierarchical Structure](08_hierarchical_structure.md) | How hierarchy emerges from geometry |
| 9 | [Consensus Distances](09_consensus_distances.md) | Multi-space blending, product metrics, long branches |
| 10 | [Physics Mindmap Builder](10_physics_mindmap_builder.md) | End-to-end app, upward penalty, export |

## Key Features

- Theory and implementation side by side
- Screenshots from real tools (density explorer, organizational depth plots)
- Reproducible examples with Python code
- Interactive HTML visualizations

## Tools Referenced

- **Density Manifold Explorer** — Browser-based 2D visualization of embedding density with tree overlays
- **Wikipedia Physics Distance Model** — Neural distance function trained on hierarchical relationships
- **Organizational Depth Scatter** — Plotly visualization of hierarchy depth in projection space
- **Physics Mindmap Builder** — Self-contained browser app for building exportable mindmaps from embedding geometry (`examples/physics-mindmap/`)

## See Also

- [Book 13: Semantic Search](../../book-13-semantic-search/README.md) — Practical semantic search pipelines
- [Book 14: AI Training](../../book-14-ai-training/README.md) — Model training techniques
