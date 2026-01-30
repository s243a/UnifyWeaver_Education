<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 9: Visualization Modules - Implementation Details

Technical deep-dive for self-contained HTML visualization generation.

## Module Pattern Overview

Self-contained visualization modules follow a consistent structure:

```prolog
:- module(module_name, [
    % Data predicates
    data_definition/N,
    config/1,

    % Generation predicates
    generate_html/1,
    generate_all/0
]).
```

## generate_all/0

### Signature

```prolog
generate_all
```

### Purpose

Entry point that generates all output files (HTML, TypeScript, Python, etc.).

### Algorithm

1. **Call Generators**: Invoke each `generate_X/1` predicate
2. **Write Files**: Open file handles and write generated content
3. **Report Progress**: Print status messages

### Example Implementation

```prolog
generate_all :-
    format('Generating visualization files...~n~n'),

    % Generate HTML
    generate_html(HTMLCode),
    open('index.html', write, S1),
    write(S1, HTMLCode),
    close(S1),
    format('  ✓ index.html~n'),

    % Generate TypeScript bindings (optional)
    generate_ts_bindings(TSCode),
    open('bindings.ts', write, S2),
    write(S2, TSCode),
    close(S2),
    format('  ✓ bindings.ts~n'),

    format('~nDone!~n').
```

## generate_html/1

### Signature

```prolog
generate_html(-HTMLCode)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `HTMLCode` | `string` | Complete HTML document |

### Algorithm

1. **Read Config**: Get title, theme, colors from `config/1`
2. **Collect Data**: Use `findall/3` to gather all data facts
3. **Convert to JSON**: Transform Prolog terms to JavaScript literals
4. **Format HTML**: Use `format/2` to build the HTML string

### HTML Structure

```prolog
generate_html(HTML) :-
    config(Config),
    member(title(Title), Config),
    generate_css(CSS),
    generate_body(Body),
    generate_javascript(JS),
    format(string(HTML), '<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>~w</title>
    <style>~w</style>
</head>
<body>
~w
<script>~w</script>
</body>
</html>', [Title, CSS, Body, JS]).
```

## Data to JSON Conversion

### node_to_json/2

```prolog
node_to_json(node(Id, Props), JSON) :-
    atom_string(Id, IdStr),
    (member(label(Label), Props) -> true ; Label = IdStr),
    format(string(JSON),
           '{ data: { id: "~w", label: "~w" } }',
           [IdStr, Label]).
```

### edge_to_json/3

```prolog
edge_to_json(edge(From, To, _Props), Idx, JSON) :-
    atom_string(From, FromStr),
    atom_string(To, ToStr),
    format(string(JSON),
           '{ data: { id: "e~w", source: "~w", target: "~w" } }',
           [Idx, FromStr, ToStr]).
```

### List Conversion

```prolog
nodes_to_json(Nodes, JSON) :-
    maplist(node_to_json, Nodes, JSONList),
    atomic_list_concat(JSONList, ',\n', JSON).
```

## Graph Module (graph_module.pl)

### Exported Predicates

| Predicate | Description |
|-----------|-------------|
| `node/2` | Define graph node with properties |
| `edge/3` | Define edge between nodes |
| `graph_config/1` | Graph configuration options |
| `generate_html/1` | Generate complete HTML |
| `generate_ts_bindings/1` | Generate TypeScript bindings |
| `generate_all/0` | Write all files |

### Configuration Options

```prolog
graph_config([
    title("Family Tree"),
    description("Biblical family relationships"),
    layout(cose),           % cose, circle, grid, breadthfirst
    theme(dark),            % dark, light
    node_color('#7c3aed'),
    edge_color('#00d4ff'),
    selected_color('#ffd700')
]).
```

### Generated Features

- Layout buttons (Force, Circle, Grid, Tree)
- Add/remove edges via UI
- Node click details panel
- Sample data loading
- Responsive design

## Curve Module (curve_module.pl)

### Exported Predicates

| Predicate | Description |
|-----------|-------------|
| `curve_function/3` | Define mathematical function |
| `curve_config/1` | Plot configuration |
| `generate_llvm_ir/1` | Generate LLVM IR for WASM |
| `generate_html/1` | Generate HTML with Chart.js |
| `generate_ts_bindings/1` | TypeScript WASM loader |
| `generate_all/0` | Write all files |

### Function Definition

```prolog
curve_function(sin_wave, X, Y) :- Y is sin(X).
curve_function(cos_wave, X, Y) :- Y is cos(X).
curve_function(quadratic, X, Y) :- Y is X * X.
```

### WASM Generation

```prolog
generate_llvm_ir(IR) :-
    format(string(IR), '
define double @compute_sin(double %x) {
    %result = call double @llvm.sin.f64(double %x)
    ret double %result
}
declare double @llvm.sin.f64(double)
', []).
```

## Matrix Module (matrix_module.pl)

### Exported Predicates

| Predicate | Description |
|-----------|-------------|
| `matrix_operation/4` | Define matrix operation |
| `preset_matrix/2` | Preset matrix definitions |
| `generate_python/1` | Generate NumPy Python code |
| `generate_html/1` | Generate HTML with Pyodide |
| `generate_ts_bindings/1` | TypeScript Pyodide loader |
| `generate_all/0` | Write all files |

### Python Generation

```prolog
generate_python(Python) :-
    format(string(Python), '
import numpy as np

def multiply(a, b):
    return np.matmul(a, b)

def inverse(a):
    return np.linalg.inv(a)

def determinant(a):
    return np.linalg.det(a)
', []).
```

## CSS Generation Helper

```prolog
generate_css(CSS) :-
    config(Config),
    member(theme(Theme), Config),
    theme_colors(Theme, BG, FG, Accent),
    format(string(CSS), '
* { box-sizing: border-box; margin: 0; padding: 0; }
body {
    font-family: system-ui, sans-serif;
    background: ~w;
    color: ~w;
}
button {
    background: ~w;
    color: white;
    border: none;
    padding: 8px 16px;
    cursor: pointer;
}
button:hover { opacity: 0.8; }
', [BG, FG, Accent]).

theme_colors(dark, '#1a1a2e', '#e0e0e0', '#7c3aed').
theme_colors(light, '#ffffff', '#333333', '#3b82f6').
```

## JavaScript Generation Helper

```prolog
generate_cytoscape_init(JS) :-
    all_nodes(Nodes),
    all_edges(Edges),
    nodes_to_json(Nodes, NodesJSON),
    edges_to_json(Edges, EdgesJSON),
    format(string(JS), '
const cy = cytoscape({
    container: document.getElementById("cy"),
    elements: [
~w,
~w
    ],
    style: [
        { selector: "node", style: {
            "background-color": "#7c3aed",
            "label": "data(label)"
        }},
        { selector: "edge", style: {
            "line-color": "#00d4ff",
            "curve-style": "bezier"
        }}
    ],
    layout: { name: "cose" }
});
', [NodesJSON, EdgesJSON]).
```

## Source Files

| Module | Location |
|--------|----------|
| Graph | `examples/wasm-graph/graph_module.pl` |
| Curve | `examples/curve-plot/curve_module.pl` |
| Matrix | `examples/pyodide-matrix/matrix_module.pl` |
