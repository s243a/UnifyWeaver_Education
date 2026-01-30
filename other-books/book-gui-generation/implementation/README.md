<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book GUI Generation - Implementation Documentation

Technical deep-dive documentation for multi-framework app generation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 2: App Generation | [02_app_generation_impl.md](./02_app_generation_impl.md) | [02_app_generation_questions.md](./02_app_generation_questions.md) |
| Chapter 9: Visualization Modules | [09_visualization_modules_impl.md](./09_visualization_modules_impl.md) | [09_visualization_modules_questions.md](./09_visualization_modules_questions.md) |

## Question Count

- Chapter 2: 10 questions
- Chapter 9: 10 questions
- **Total: 20 questions**

## Topics Covered

### App Generation (Chapter 2)
- `generate_complete_project/4` API
- Vue 3 + Vite generation
- React Native + Expo generation
- Flutter + Dart generation
- SwiftUI generation
- Navigation specification (tabs, stack, drawer)
- Full-stack project generation
- Config file generation (package.json, pubspec.yaml)

### Visualization Modules (Chapter 9)
- Self-contained module pattern
- `generate_all/0` entry point
- HTML generation with `format/2`
- Prolog to JSON conversion
- Cytoscape.js graph visualization
- Chart.js curve plotting
- Pyodide/NumPy matrix operations
- WASM generation from LLVM IR

## Source Files

- `src/unifyweaver/glue/app_generator.pl`
- `examples/wasm-graph/graph_module.pl`
- `examples/curve-plot/curve_module.pl`
- `examples/pyodide-matrix/matrix_module.pl`
