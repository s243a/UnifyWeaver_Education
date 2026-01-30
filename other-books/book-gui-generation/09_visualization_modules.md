<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 9: Self-Contained Visualization Modules

## Overview

In addition to the multi-target app generator (`app_generator.pl`), UnifyWeaver supports a simpler pattern for creating browser-based visualizations: **self-contained Prolog modules** that generate complete HTML applications.

This pattern is ideal for:
- Data visualization dashboards
- Interactive graph explorers
- Mathematical plotting tools
- Demos and prototypes

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│              Self-Contained Prolog Module                   │
│  - Data definitions (facts)                                 │
│  - Configuration options                                    │
│  - HTML/CSS/JS generation predicates                        │
│  - generate_all/0 entry point                               │
└─────────────────────┬───────────────────────────────────────┘
                      │
                      ▼
┌─────────────────────────────────────────────────────────────┐
│                    Output Files                              │
│  - index.html (complete web app)                            │
│  - Optional: .ts bindings, .py libraries, .ll LLVM IR       │
└─────────────────────────────────────────────────────────────┘
```

## Pattern Components

### 1. Module Declaration

```prolog
:- module(graph_module, [
    % Data definitions
    node/2,                    % node(+Id, +Properties)
    edge/3,                    % edge(+From, +To, +Properties)
    graph_config/1,            % graph_config(-Config)

    % Code generation
    generate_html/1,           % generate_html(-HTMLCode)
    generate_ts_bindings/1,    % generate_ts_bindings(-TSCode)
    generate_all/0             % Entry point - writes all files
]).
```

### 2. Data Definitions (Facts)

```prolog
% Configuration
graph_config([
    title("Family Tree"),
    layout(cose),
    theme(dark),
    node_color('#7c3aed')
]).

% Data as Prolog facts
node(abraham, [label("Abraham"), type(person)]).
node(isaac, [label("Isaac"), type(person)]).
edge(abraham, isaac, [relation(parent)]).
```

### 3. HTML Generation Predicate

```prolog
generate_html(HTML) :-
    graph_config(Config),
    member(title(Title), Config),
    all_nodes(Nodes),
    all_edges(Edges),
    nodes_to_json(Nodes, NodesJSON),
    edges_to_json(Edges, EdgesJSON),
    format(string(HTML), '<!DOCTYPE html>
<html>
<head>
    <title>~w</title>
    <script src="https://unpkg.com/cytoscape/dist/cytoscape.min.js"></script>
    <style>
        #cy { width: 100%; height: 100vh; }
    </style>
</head>
<body>
    <div id="cy"></div>
    <script>
        const cy = cytoscape({
            container: document.getElementById("cy"),
            elements: [
~w,
~w
            ]
        });
    </script>
</body>
</html>', [Title, NodesJSON, EdgesJSON]).
```

### 4. Entry Point

```prolog
generate_all :-
    generate_html(HTML),
    open('index.html', write, S),
    write(S, HTML),
    close(S),
    format('Generated index.html~n').
```

## Real Examples

UnifyWeaver includes several fully-working visualization modules:

### Graph Visualization (Cytoscape.js)

**Path:** `examples/wasm-graph/graph_module.pl`

```prolog
% Define nodes and edges as Prolog facts
node(abraham, [label("Abraham"), generation(1)]).
node(isaac, [label("Isaac"), generation(2)]).
edge(abraham, isaac, [relation(parent)]).

% Generate complete HTML app
?- generate_all.
% Creates: index.html with Cytoscape.js graph
```

**Features:**
- Layout buttons (Force, Circle, Grid, Tree)
- Add/remove edges dynamically
- Click nodes for details
- Dark theme styling

**Run:**
```bash
cd examples/wasm-graph
swipl -g "consult('graph_module.pl'), graph_module:generate_all" -t halt
# Open index.html in browser
```

### Curve Plotting (Chart.js + WASM)

**Path:** `examples/curve-plot/curve_module.pl`

```prolog
% Define mathematical functions
curve_function(sin_wave, X, Y) :- Y is sin(X).
curve_function(cos_wave, X, Y) :- Y is cos(X).

% Generate with WASM acceleration
?- generate_all.
% Creates: index.html, curve_plot.ll, curve_wasm.ts
```

**Features:**
- Multiple curve types (sine, cosine, polynomial)
- WASM-accelerated computation with JS fallback
- Interactive Chart.js visualization
- Range controls (min, max, step)

**Run:**
```bash
cd examples/curve-plot
swipl -g "consult('curve_module.pl'), curve_module:generate_all" -t halt
```

### Matrix Operations (Pyodide + NumPy)

**Path:** `examples/pyodide-matrix/matrix_module.pl`

```prolog
% Define matrix operations
matrix_operation(multiply, A, B, Result) :-
    Result = numpy_matmul(A, B).
matrix_operation(inverse, A, _, Result) :-
    Result = numpy_inv(A).

% Generate with Python/NumPy via Pyodide
?- generate_all.
% Creates: index.html, matrix_lib.py, matrix_wasm.ts
```

**Features:**
- Matrix input grid
- Preset matrices (identity, rotation, etc.)
- NumPy operations via Pyodide (browser Python)
- Chart.js visualization of results

## Visualization Libraries

| Library | Use Case | CDN |
|---------|----------|-----|
| Cytoscape.js | Graphs, networks | `unpkg.com/cytoscape` |
| Chart.js | Charts, plots | `cdn.jsdelivr.net/npm/chart.js` |
| D3.js | Custom visualizations | `d3js.org` |
| Plotly.js | 3D plots, scientific | `cdn.plot.ly/plotly-latest.min.js` |
| Pyodide | Python in browser | `cdn.jsdelivr.net/pyodide` |

## Generation Pattern

### Helper Predicates

```prolog
%% Convert Prolog data to JSON
nodes_to_json(Nodes, JSON) :-
    maplist(node_to_json, Nodes, JSONList),
    atomic_list_concat(JSONList, ',\n', JSON).

node_to_json(node(Id, Props), JSON) :-
    member(label(Label), Props),
    format(string(JSON),
           '{ data: { id: "~w", label: "~w" } }',
           [Id, Label]).
```

### CSS Generation

```prolog
generate_css(CSS) :-
    graph_config(Config),
    member(theme(Theme), Config),
    theme_colors(Theme, BG, FG),
    format(string(CSS), '
body {
    background: ~w;
    color: ~w;
    margin: 0;
}
#cy {
    width: 100%%;
    height: 100vh;
}', [BG, FG]).

theme_colors(dark, '#1a1a2e', '#eee').
theme_colors(light, '#ffffff', '#333').
```

### JavaScript Generation

```prolog
generate_javascript(JS) :-
    all_nodes(Nodes),
    all_edges(Edges),
    nodes_to_cytoscape(Nodes, NodeElements),
    edges_to_cytoscape(Edges, EdgeElements),
    format(string(JS), '
const cy = cytoscape({
    container: document.getElementById("cy"),
    elements: [~w, ~w],
    style: [
        { selector: "node", style: { label: "data(label)" } }
    ],
    layout: { name: "cose" }
});', [NodeElements, EdgeElements]).
```

## Comparison: App Generator vs Visualization Modules

| Aspect | App Generator | Visualization Modules |
|--------|---------------|----------------------|
| **Output** | Multi-file project | Single HTML file |
| **Targets** | Vue, React, Flutter, SwiftUI | Browser only |
| **Complexity** | Full app scaffolding | Single-purpose tool |
| **Dependencies** | npm, build tools | CDN libraries |
| **Use Case** | Production apps | Dashboards, demos |
| **Generator** | `app_generator.pl` | Self-contained module |

## Current Status

| App | Generator | Status |
|-----|-----------|--------|
| wasm-graph | `graph_module.pl` | ✅ Fully generated |
| curve-plot | `curve_module.pl` | ✅ Fully generated |
| pyodide-matrix | `matrix_module.pl` | ✅ Fully generated |
| storybook-react | `generate.pl` | ✅ Fully generated |
| flutter-cli | `generate.pl` | ✅ Fully generated |
| tui-cli | `tui_generator.pl` | ✅ Fully generated |
| tui-dialog-cli | `tui_generator.pl` | ✅ Fully generated |

See `docs/GUI_GENERATION_STATUS.md` in the main project for the complete list.

## Creating Your Own Visualization Module

### Step 1: Create Module Structure

```prolog
:- module(my_viz, [
    data_point/2,
    viz_config/1,
    generate_html/1,
    generate_all/0
]).
```

### Step 2: Define Data

```prolog
viz_config([title("My Visualization"), theme(dark)]).

data_point(1, 10).
data_point(2, 25).
data_point(3, 15).
```

### Step 3: Implement HTML Generation

```prolog
generate_html(HTML) :-
    viz_config(Config),
    member(title(Title), Config),
    findall([X,Y], data_point(X, Y), Data),
    data_to_json(Data, DataJSON),
    format(string(HTML), '<!DOCTYPE html>...~w...', [Title, DataJSON]).
```

### Step 4: Write Entry Point

```prolog
generate_all :-
    generate_html(HTML),
    open('index.html', write, S),
    write(S, HTML),
    close(S),
    format('Generated index.html~n').
```

### Step 5: Run

```bash
swipl -g "consult('my_viz.pl'), my_viz:generate_all" -t halt
```

---

**Previous**: [Chapter 8: Theming](08_theming.md)
