<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 9: Visualization Modules - Questions

Q&A companion for self-contained HTML visualization generation.

## Question Index

1. [What is the visualization module pattern?](#bgui09-q-pattern)
2. [How does generate_all/0 work?](#bgui09-q-generate-all)
3. [How is HTML generated from Prolog?](#bgui09-q-html-generation)
4. [How do I convert Prolog data to JSON?](#bgui09-q-json-conversion)
5. [What visualization libraries are supported?](#bgui09-q-libraries)
6. [How does graph_module.pl work?](#bgui09-q-graph-module)
7. [How does curve_module.pl generate WASM?](#bgui09-q-curve-wasm)
8. [How does matrix_module.pl use Pyodide?](#bgui09-q-matrix-pyodide)
9. [How do I run a visualization module?](#bgui09-q-running)
10. [How do I create my own visualization module?](#bgui09-q-custom)

---

<a id="bgui09-q-pattern"></a>
## Q1: What is the visualization module pattern?

**Question:** What is the self-contained visualization module pattern?

**Answer:** A self-contained Prolog module that defines data as facts and generates complete HTML applications:

```prolog
:- module(my_viz, [
    data_point/2,        % Data definitions
    viz_config/1,        % Configuration
    generate_html/1,     % HTML generation
    generate_all/0       % Entry point
]).
```

Unlike `app_generator.pl` which scaffolds multi-file projects, this pattern generates single HTML files with embedded CSS/JS.

**See:** [09_visualization_modules_impl.md#module-pattern-overview](09_visualization_modules_impl.md#module-pattern-overview)

---

<a id="bgui09-q-generate-all"></a>
## Q2: How does generate_all/0 work?

**Question:** What does the generate_all/0 predicate do?

**Answer:** It's the entry point that generates and writes all output files:

```prolog
generate_all :-
    generate_html(HTML),
    open('index.html', write, S),
    write(S, HTML),
    close(S),
    format('Generated index.html~n').
```

Run with:
```bash
swipl -g "consult('module.pl'), module:generate_all" -t halt
```

**See:** [09_visualization_modules_impl.md#generate_all0](09_visualization_modules_impl.md#generate_all0)

---

<a id="bgui09-q-html-generation"></a>
## Q3: How is HTML generated from Prolog?

**Question:** How does generate_html/1 build the HTML document?

**Answer:** Using `format/2` with string interpolation:

```prolog
generate_html(HTML) :-
    config(Config),
    member(title(Title), Config),
    generate_css(CSS),
    generate_javascript(JS),
    format(string(HTML), '<!DOCTYPE html>
<html>
<head><title>~w</title><style>~w</style></head>
<body><script>~w</script></body>
</html>', [Title, CSS, JS]).
```

The `~w` placeholders are replaced with values from the argument list.

**See:** [09_visualization_modules_impl.md#generate_html1](09_visualization_modules_impl.md#generate_html1)

---

<a id="bgui09-q-json-conversion"></a>
## Q4: How do I convert Prolog data to JSON?

**Question:** How do I transform Prolog facts into JavaScript object literals?

**Answer:** Use format/2 to build JSON strings:

```prolog
node_to_json(node(Id, Props), JSON) :-
    atom_string(Id, IdStr),
    member(label(Label), Props),
    format(string(JSON),
           '{ data: { id: "~w", label: "~w" } }',
           [IdStr, Label]).

% Convert list
nodes_to_json(Nodes, JSON) :-
    maplist(node_to_json, Nodes, JSONList),
    atomic_list_concat(JSONList, ',\n', JSON).
```

**See:** [09_visualization_modules_impl.md#data-to-json-conversion](09_visualization_modules_impl.md#data-to-json-conversion)

---

<a id="bgui09-q-libraries"></a>
## Q5: What visualization libraries are supported?

**Question:** Which JavaScript visualization libraries can I use?

**Answer:** Common libraries with CDN links:

| Library | Use Case | CDN |
|---------|----------|-----|
| Cytoscape.js | Graphs, networks | `unpkg.com/cytoscape` |
| Chart.js | Charts, plots | `cdn.jsdelivr.net/npm/chart.js` |
| D3.js | Custom visualizations | `d3js.org` |
| Plotly.js | 3D plots | `cdn.plot.ly/plotly-latest.min.js` |
| Pyodide | Python in browser | `cdn.jsdelivr.net/pyodide` |

Include via `<script src="...">` in generated HTML.

**See:** [09_visualization_modules_impl.md](09_visualization_modules_impl.md)

---

<a id="bgui09-q-graph-module"></a>
## Q6: How does graph_module.pl work?

**Question:** How does the graph visualization module work?

**Answer:** Define nodes and edges as Prolog facts:

```prolog
node(abraham, [label("Abraham"), generation(1)]).
node(isaac, [label("Isaac"), generation(2)]).
edge(abraham, isaac, [relation(parent)]).
```

Then `generate_all/0` creates an HTML file with Cytoscape.js that renders the graph with:
- Layout buttons (Force, Circle, Grid, Tree)
- Node click details
- Add/remove edges UI

**See:** [09_visualization_modules_impl.md#graph-module-graph_modulepl](09_visualization_modules_impl.md#graph-module-graph_modulepl)

---

<a id="bgui09-q-curve-wasm"></a>
## Q7: How does curve_module.pl generate WASM?

**Question:** How does curve plotting use WebAssembly?

**Answer:** The module generates LLVM IR that compiles to WASM:

```prolog
generate_llvm_ir(IR) :-
    format(string(IR), '
define double @compute_sin(double %x) {
    %result = call double @llvm.sin.f64(double %x)
    ret double %result
}', []).
```

Build with:
```bash
llc -march=wasm32 curve.ll -o curve.o
wasm-ld --no-entry --export-all curve.o -o curve.wasm
```

The HTML includes JS fallback for browsers without WASM.

**See:** [09_visualization_modules_impl.md#curve-module-curve_modulepl](09_visualization_modules_impl.md#curve-module-curve_modulepl)

---

<a id="bgui09-q-matrix-pyodide"></a>
## Q8: How does matrix_module.pl use Pyodide?

**Question:** How does the matrix module run Python in the browser?

**Answer:** It generates Python code and loads Pyodide:

```prolog
generate_python(Python) :-
    format(string(Python), '
import numpy as np

def multiply(a, b):
    return np.matmul(a, b)
', []).
```

The HTML loads Pyodide from CDN and runs the Python code:
```javascript
const pyodide = await loadPyodide();
await pyodide.runPythonAsync(pythonCode);
```

**See:** [09_visualization_modules_impl.md#matrix-module-matrix_modulepl](09_visualization_modules_impl.md#matrix-module-matrix_modulepl)

---

<a id="bgui09-q-running"></a>
## Q9: How do I run a visualization module?

**Question:** How do I execute a visualization module and view the result?

**Answer:** Run with SWI-Prolog:

```bash
cd examples/wasm-graph
swipl -g "consult('graph_module.pl'), graph_module:generate_all" -t halt
```

Then open `index.html` in a browser:
```bash
# Simple HTTP server (for modules that need it)
python -m http.server 8000
# Open http://localhost:8000
```

**See:** [09_visualization_modules_impl.md#generate_all0](09_visualization_modules_impl.md#generate_all0)

---

<a id="bgui09-q-custom"></a>
## Q10: How do I create my own visualization module?

**Question:** What are the steps to create a custom visualization module?

**Answer:** Four steps:

1. **Define module exports:**
```prolog
:- module(my_viz, [data/2, config/1, generate_html/1, generate_all/0]).
```

2. **Define data as facts:**
```prolog
config([title("My Viz"), theme(dark)]).
data(1, 10).
data(2, 25).
```

3. **Implement generate_html/1:**
```prolog
generate_html(HTML) :-
    findall([X,Y], data(X,Y), Data),
    format(string(HTML), '<!DOCTYPE html>...~w...', [Data]).
```

4. **Implement generate_all/0:**
```prolog
generate_all :-
    generate_html(H), open('index.html',write,S), write(S,H), close(S).
```

**See:** [09_visualization_modules_impl.md](09_visualization_modules_impl.md)
