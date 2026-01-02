<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 23: Declarative Visualization Glue

**Generating React Components and Python Plots from Prolog Specifications**

This chapter covers the declarative glue modules that generate visualization code:

**Core Visualization Modules:**
- **graph_generator.pl** - Cytoscape.js graph visualization with React/TypeScript
- **curve_plot_generator.pl** - Chart.js curve plotting with React/TypeScript
- **matplotlib_generator.pl** - Python matplotlib code generation
- **heatmap_generator.pl** - Heatmap visualization with React and seaborn
- **treemap_generator.pl** - Hierarchical treemap visualization with React and Plotly
- **plot3d_generator.pl** - 3D surface, scatter, and line plots with Plotly.js

**Layout & Controls:**
- **layout_generator.pl** - Declarative CSS Grid/Flexbox layouts and controls
- **responsive_generator.pl** - Responsive breakpoints and mobile support
- **accessibility_generator.pl** - ARIA attributes, keyboard navigation, color blindness
- **animation_generator.pl** - CSS keyframes, sequences, and transitions
- **interaction_generator.pl** - Pan/zoom, brush selection, tooltips, drill-down

**Export & Preview:**
- **export_generator.pl** - SVG, PNG, PDF, JSON, CSV export functionality
- **live_preview_generator.pl** - Vite dev server, hot-reload, state synchronization
- **data_binding_generator.pl** - Reactive data binding, computed properties, WebSocket sync

**Theme & Templates:**
- **theme_generator.pl** - Centralized themes with CSS custom properties
- **animation_presets.pl** - Curated animation library (entry, exit, attention, chart)
- **template_library.pl** - Pre-built dashboard and report templates

**Performance:**
- **lazy_loading_generator.pl** - Pagination, infinite scroll, chunked loading
- **virtual_scroll_generator.pl** - Efficient rendering for large lists/tables/grids
- **webworker_generator.pl** - Background data processing and chart calculations

## Overview

The visualization glue modules follow the UnifyWeaver pattern of declarative specifications that generate target-language code. Instead of manually writing React components or Python plotting scripts, you define your data and configuration in Prolog, then generate the complete implementation.

```
┌─────────────────────────────────────────────────────────────────┐
│                    Prolog Specifications                        │
│  graph_node/2, graph_edge/3, curve/2, matplotlib_curve/2, etc. │
└───────────────────────────┬─────────────────────────────────────┘
                            │
            ┌───────────────┼───────────────┐
            ▼               ▼               ▼
    ┌───────────────┐ ┌───────────────┐ ┌───────────────┐
    │ React/TS +    │ │ React/TS +    │ │ Python +      │
    │ Cytoscape.js  │ │ Chart.js      │ │ Matplotlib    │
    └───────────────┘ └───────────────┘ └───────────────┘
```

## Graph Generator

The graph generator creates interactive network visualizations using Cytoscape.js within React components.

### Defining Nodes and Edges

```prolog
:- use_module('src/unifyweaver/glue/graph_generator').

% Define nodes with properties
graph_node(abraham, [label("Abraham"), type(person), generation(1)]).
graph_node(isaac, [label("Isaac"), type(person), generation(2)]).
graph_node(jacob, [label("Jacob"), type(person), generation(3)]).

% Define edges with relationships
graph_edge(abraham, isaac, [relation(parent), via(sarah)]).
graph_edge(isaac, jacob, [relation(parent), via(rebekah)]).
```

### Graph Specifications

```prolog
% Define a complete graph specification
graph_spec(family_tree, [
    title("Family Tree"),
    layout(cose),              % Force-directed layout
    theme(dark),
    nodes([abraham, sarah, isaac, rebekah, jacob, esau])
]).
```

### Code Generation

```prolog
% Generate React/TypeScript component
?- generate_graph_component(family_tree, ComponentCode).

% Generate Cytoscape.js configuration
?- generate_cytoscape_config(family_tree, Config).

% Generate CSS module
?- generate_graph_styles(family_tree, CssCode).

% Generate graph data as JavaScript
?- generate_graph_data(family_tree, DataCode).
```

### Generated Component Structure

The generator produces a complete React component with:

- TypeScript interfaces for type safety
- Cytoscape.js initialization and configuration
- CSS modules for styling
- Responsive container layout
- Interactive features (pan, zoom, select)

```typescript
// Generated: FamilyTreeGraph.tsx
import React, { useEffect, useRef } from 'react';
import cytoscape from 'cytoscape';
import styles from './FamilyTreeGraph.module.css';

interface GraphProps {
  onNodeClick?: (nodeId: string) => void;
}

export const FamilyTreeGraph: React.FC<GraphProps> = ({ onNodeClick }) => {
  const containerRef = useRef<HTMLDivElement>(null);
  // ... component implementation
};
```

### Available Layouts

| Layout | Description |
|--------|-------------|
| `cose` | Force-directed layout (default) |
| `breadthfirst` | Hierarchical tree layout |
| `circle` | Circular arrangement |
| `grid` | Regular grid layout |
| `dagre` | Directed acyclic graph (requires plugin) |

## Curve Plot Generator

The curve plot generator creates interactive mathematical curve visualizations using Chart.js.

### Defining Curves

Curves can be defined in three ways: mathematical expressions, explicit data points (MATLAB-style), or predefined types.

#### Method 1: Mathematical Expressions (Recommended)

Use `expr()` to define curves with arbitrary mathematical expressions:

```prolog
:- use_module('src/unifyweaver/glue/curve_plot_generator').

% Gaussian curve
curve(gaussian, [
    expr(exp(-(x^2) / 2)),
    color('#10b981'),
    label("Gaussian")
]).

% Damped oscillation
curve(damped_sine, [
    expr(exp(-abs(x) / 3) * sin(x * 2)),
    color('#6366f1'),
    label("Damped sine")
]).

% Rational function (Cauchy distribution)
curve(rational, [
    expr(1 / (1 + x^2)),
    color('#14b8a6'),
    label("1/(1+x²)")
]).

% Higher-order polynomial
curve(polynomial, [
    expr(x^4 - 2*x^2 + 0.5),
    color('#ec4899'),
    label("x⁴ - 2x² + 0.5")
]).
```

#### Method 2: Explicit Data Points (MATLAB-Style)

For measured or sampled data:

```prolog
curve(sampled_data, [
    data([-2, -1, 0, 1, 2, 3, 4],    % X values
         [4, 1, 0, 1, 4, 9, 16]),     % Y values
    color('#f97316'),
    label("Measured Data")
]).

curve(experimental, [
    data([0, 0.5, 1.0, 1.5, 2.0],
         [0.0, 0.48, 0.84, 1.0, 0.91]),
    color('#8b5cf6'),
    label("Experimental")
]).
```

#### Method 3: Predefined Types

For common curve types with parameters:

```prolog
% Trigonometric curves
curve(sine_wave, [
    type(sine),
    amplitude(1),
    frequency(1),
    phase(0),
    color('#00d4ff'),
    label("sin(x)")
]).

curve(cosine_wave, [
    type(cosine),
    amplitude(1),
    frequency(1),
    phase(0),
    color('#ff6b6b'),
    label("cos(x)")
]).

% Polynomial curves
curve(parabola, [
    type(quadratic),
    a(1), b(0), c(0),
    color('#22c55e'),
    label("y = x²")
]).
```

### Curve Evaluation

The module includes runtime curve evaluation:

```prolog
% Evaluate a curve at a specific x value
?- evaluate_curve(sine_wave, 0, Y).
Y = 0.0.

?- evaluate_curve(sine_wave, 1.5708, Y).  % π/2
Y = 1.0.

?- evaluate_curve(parabola, 3, Y).
Y = 9.0.
```

### Plot Specifications

```prolog
plot_spec(trig_demo, [
    title("Trigonometric Functions"),
    curves([sine_wave, cosine_wave]),
    x_range(-6.28, 6.28),    % -2π to 2π
    y_range(-1.5, 1.5),
    theme(dark),
    points(200)              % Number of sample points
]).
```

### Code Generation

```prolog
% Generate React/TypeScript component
?- generate_curve_component(trig_demo, ComponentCode).

% Generate Chart.js configuration
?- generate_chartjs_config(trig_demo, Config).

% Generate CSS module
?- generate_curve_styles(trig_demo, CssCode).
```

### Supported Curve Types

| Type | Formula | Parameters |
|------|---------|------------|
| `linear` | y = mx + b | m, b |
| `quadratic` | y = ax² + bx + c | a, b, c |
| `cubic` | y = ax³ + bx² + cx + d | a, b, c, d |
| `sine` | y = A·sin(ωx + φ) | amplitude, frequency, phase |
| `cosine` | y = A·cos(ωx + φ) | amplitude, frequency, phase |
| `exponential` | y = base·e^(scale·x) | base, scale |
| `logarithm` | y = log_base(x) | base |
| `absolute` | y = \|x\| | (none) |

## Matplotlib Generator

The matplotlib generator produces Python scripts for publication-quality plots.

### Defining Curves

```prolog
:- use_module('src/unifyweaver/glue/matplotlib_generator').

matplotlib_curve(sine, [
    type(sine),
    amplitude(1),
    frequency(1),
    phase(0),
    color('blue'),
    linestyle('-'),
    label("sin(x)")
]).

matplotlib_curve(gaussian, [
    type(gaussian),
    mu(0),
    sigma(1),
    color('navy'),
    linestyle('-'),
    label("Gaussian")
]).
```

### Plot Specifications

```prolog
matplotlib_plot(trig_functions, [
    title("Trigonometric Functions"),
    curves([sine, cosine]),
    x_range(-6.28318, 6.28318),
    y_range(-1.5, 1.5),
    style(seaborn),
    figsize(10, 6),
    grid(true),
    legend(true),
    xlabel("x"),
    ylabel("y"),
    output(show)              % or: png("output.png"), pdf("output.pdf")
]).
```

### Code Generation

```prolog
% Generate Python code
?- generate_matplotlib_code(trig_functions, PythonCode).

% Generate complete runnable script
?- generate_matplotlib_script(trig_functions, Script).
```

### Generated Python Code

```python
#!/usr/bin/env python3
# Generated by UnifyWeaver matplotlib_generator

import numpy as np
import matplotlib.pyplot as plt

def plot_trig_functions():
    """Trigonometric Functions"""
    plt.style.use('seaborn')
    fig, ax = plt.subplots(figsize=(10, 6))

    x = np.linspace(-6.28318, 6.28318, 200)

    # sine curve
    y_sine = np.sin(x)
    ax.plot(x, y_sine, color='blue', linestyle='-', label='sin(x)')

    # cosine curve
    y_cosine = np.cos(x)
    ax.plot(x, y_cosine, color='orange', linestyle='-', label='cos(x)')

    ax.set_xlabel('x')
    ax.set_ylabel('y')
    ax.set_title('Trigonometric Functions')
    ax.set_ylim(-1.5, 1.5)
    ax.grid(True)
    ax.legend()

    plt.show()

if __name__ == '__main__':
    plot_trig_functions()
```

### NumPy Expression Generation

The generator converts curve types to NumPy expressions:

```prolog
% Get NumPy expression for a curve type
?- matplotlib_curve(sine, Props),
   generate_numpy_expression(sine, Props, Expr).
Expr = 'np.sin(x)'.

?- matplotlib_curve(gaussian, Props),
   generate_numpy_expression(gaussian, Props, Expr).
Expr = 'np.exp(-((x - 0) ** 2) / (2 * 1 ** 2))'.
```

### Output Formats

| Format | Example | Description |
|--------|---------|-------------|
| `show` | `output(show)` | Display interactive window |
| `png(File)` | `output(png("plot.png"))` | Save as PNG image |
| `pdf(File)` | `output(pdf("plot.pdf"))` | Save as PDF document |
| `svg(File)` | `output(svg("plot.svg"))` | Save as SVG vector graphic |

## Integration with Other Glue Modules

The visualization modules integrate with other UnifyWeaver glue modules:

### With Express Generator

```prolog
% Define an API endpoint that returns graph data
api_endpoint('/api/graph/:name', get, [
    handler(graph_data_handler),
    response_type(json)
]).

% Handler generates graph data from Prolog
graph_data_handler(Request, Response) :-
    member(name=Name, Request.params),
    generate_graph_data(Name, Data),
    Response = json(Data).
```

### With React Generator

```prolog
% Include graph component in a React page
react_page(dashboard, [
    components([
        graph_component(family_tree, [interactive(true)]),
        curve_component(trig_demo, [responsive(true)])
    ])
]).
```

## Layout System

The visualization modules integrate with a declarative layout system for composing UIs.

### Outer Layouts (Container Positioning)

```prolog
:- use_module('src/unifyweaver/glue/layout_generator').

% Define a sidebar + content layout
layout(my_dashboard, grid, [
    areas([["sidebar", "main"]]),
    columns(["320px", "1fr"]),
    gap("1rem")
]).

% Place components in regions
place(my_dashboard, sidebar, [controls]).
place(my_dashboard, main, [chart]).

% Generate with layout
?- generate_graph_with_layout(family_tree, sidebar_content, Code).
?- generate_curve_with_layout(trig_demo, dashboard, Code).
```

### Subplot Layouts (Internal Component Grids)

For composite components with multiple charts/graphs:

```prolog
% Define a 2x2 subplot grid
subplot_layout(comparison_demo, grid, [rows(2), cols(2)]).

% Place content in cells
subplot_content(comparison_demo, pos(1,1), [curve(sine), title("Sine")]).
subplot_content(comparison_demo, pos(1,2), [curve(cosine), title("Cosine")]).
subplot_content(comparison_demo, pos(2,1), [curve(quadratic), title("Quadratic")]).
subplot_content(comparison_demo, pos(2,2), [curve(exponential), title("Exp")]).

% Generate - target-aware!
?- generate_subplot_css(comparison_demo, CSS).      % Web: nested CSS grid
?- generate_subplot_matplotlib(comparison_demo, Code). % Python: native subplots
```

The layout system is target-aware:
- **Web targets**: Synthesizes nested CSS grids with multiple chart instances
- **Matplotlib**: Uses native `plt.subplots()` for efficient multi-plot figures

## Control System

The control system provides declarative UI controls that integrate with visualizations.

### Defining Controls

```prolog
:- use_module('src/unifyweaver/glue/layout_generator').

% Slider control for numeric values
control(amplitude, slider, [
    min(0), max(5), step(0.1),
    default(1),
    label("Amplitude")
]).

% Dropdown select
control(curve_type, select, [
    options([sine, cosine, quadratic, cubic, exponential]),
    default(sine),
    label("Curve Type")
]).

% Checkbox for boolean values
control(show_grid, checkbox, [
    default(true),
    label("Show Grid")
]).

% Color picker
control(line_color, color_picker, [
    default('#00d4ff'),
    label("Line Color")
]).
```

### Control Panels

Group related controls into panels:

```prolog
% Control panel for curve parameters
control_panel(curve_controls, [amplitude, frequency, phase, curve_type]).

% Control panel for display settings
control_panel(display_controls, [show_grid, show_legend, line_color, line_width]).
```

### Generating Control JSX

```prolog
% Generate individual control
?- generate_control_jsx(amplitude, JSX).
% Produces slider input with label and onChange handler

% Generate entire control panel
?- generate_control_panel_jsx(curve_controls, PanelJSX).
% Produces panel with all controls grouped

% Generate React useState declarations
?- generate_control_state(curve_controls, StateCode).
% const [amplitude, setAmplitude] = useState(1);
% const [frequency, setFrequency] = useState(1);
% ...
```

### Control Types

| Type | Description | Generated HTML |
|------|-------------|----------------|
| `slider` | Numeric range input | `<input type="range">` |
| `select` | Dropdown selection | `<select><option>...</option></select>` |
| `checkbox` | Boolean toggle | `<input type="checkbox">` |
| `color_picker` | Color selection | `<input type="color">` |
| `number_input` | Numeric text input | `<input type="number">` |
| `text_input` | Text input | `<input type="text">` |

### Wired Components

Generate complete components with controls wired to visualization:

```prolog
% Generate a wired component with sidebar layout
?- generate_wired_component(my_demo, [
       panel(curve_controls),
       component(curve),
       layout(sidebar_content)
   ], Code).
```

This produces:
- React component with useState hooks for each control
- Control panel in sidebar region
- Visualization receiving props from control state
- TypeScript interface for props

### TypeScript Interface Generation

```prolog
?- generate_prop_types(curve_controls, TypesCode).
% interface ChartProps {
%   amplitude: number;
%   frequency: number;
%   phase: number;
%   curveType: string;
% }
```

## Heatmap Generator

The heatmap generator creates grid-based visualizations for correlation matrices, activity data, and more.

### Defining Heatmap Data

```prolog
:- use_module('src/unifyweaver/glue/heatmap_generator').

% Define heatmap configuration
heatmap_spec(correlation_demo, [
    title("Correlation Matrix"),
    x_labels(["Variable A", "Variable B", "Variable C"]),
    y_labels(["Variable A", "Variable B", "Variable C"]),
    color_scale(diverging),
    show_values(true)
]).

% Define cell values (x, y, value)
heatmap_cell(correlation_demo, 0, 0, 1.0).
heatmap_cell(correlation_demo, 0, 1, 0.8).
heatmap_cell(correlation_demo, 1, 0, 0.8).
heatmap_cell(correlation_demo, 1, 1, 1.0).

% Or use rows for efficiency
heatmap_row(activity_demo, 0, [0.1, 0.2, 0.15, 0.18]).
heatmap_row(activity_demo, 1, [0.5, 0.6, 0.55, 0.58]).
```

### Color Scales

| Scale | Description |
|-------|-------------|
| `sequential` | White to blue gradient |
| `diverging` | Blue (-1) to white (0) to red (+1) |
| `viridis` | Perceptually uniform color map |
| `heat` | Black to red to yellow to white |

### Code Generation

```prolog
% Generate React component
?- generate_heatmap_component(correlation_demo, Code).

% Generate Python/seaborn code
?- generate_heatmap_matplotlib(correlation_demo, PyCode).
```

## Treemap Generator

The treemap generator creates hierarchical visualizations using nested rectangles.

### Defining Treemap Data

```prolog
:- use_module('src/unifyweaver/glue/treemap_generator').

% Define treemap configuration
treemap_spec(filesystem_demo, [
    title("Project File Sizes"),
    root(project_root),
    color_by(category),
    show_labels(true)
]).

% Define nodes: id, parent, label, value
treemap_node(project_root, null, "Project", 0).
treemap_node(src, project_root, "src", 0).
treemap_node(main_ts, src, "main.ts", 150).
treemap_node(utils_ts, src, "utils.ts", 80).
```

### Color Modes

| Mode | Description |
|------|-------------|
| `depth` | Color by tree depth level |
| `value` | Color by node value |
| `category` | Color by hash of node ID |

### Code Generation

```prolog
% Generate React component
?- generate_treemap_component(filesystem_demo, Code).

% Generate Python/Plotly code
?- generate_treemap_plotly(filesystem_demo, PyCode).
```

## 3D Plot Generator

The 3D plot generator creates interactive 3D visualizations including surfaces, scatter plots, and lines.

### Defining 3D Surfaces

Surfaces can be defined in three ways: mathematical expressions, explicit data points (MATLAB-style), or predefined functions.

#### Method 1: Mathematical Expressions (Recommended)

Use `expr()` to define surfaces with arbitrary Prolog mathematical expressions that are translated to JavaScript (React/Plotly) or Python (NumPy/matplotlib):

```prolog
:- use_module('src/unifyweaver/glue/plot3d_generator').

% Define a surface using a mathematical expression
surface3d(wave_surface, [
    title("3D Wave Surface"),
    expr(sin(x) * cos(y)),      % z = sin(x) * cos(y)
    x_range(-pi, pi),
    y_range(-pi, pi),
    resolution(50),
    colorscale(viridis)
]).

% More complex expressions
surface3d(gaussian_surface, [
    title("Gaussian"),
    expr(exp(-(x^2 + y^2) / 2)),
    x_range(-3, 3),
    y_range(-3, 3)
]).

% Complex ripple pattern
surface3d(ripple_surface, [
    title("Ripple"),
    expr(sin(sqrt(x^2 + y^2) * 3) / (sqrt(x^2 + y^2) + 0.1)),
    x_range(-5, 5),
    y_range(-5, 5)
]).
```

#### Supported Math Functions

The expression translator supports:

| Category | Functions |
|----------|-----------|
| Trigonometric | `sin`, `cos`, `tan`, `asin`, `acos`, `atan`, `atan2` |
| Hyperbolic | `sinh`, `cosh`, `tanh` |
| Exponential | `exp`, `log`, `log10`, `log2` |
| Power/Roots | `^`, `**`, `sqrt`, `cbrt` |
| Rounding | `floor`, `ceil`, `round`, `abs` |
| Comparison | `min`, `max`, `sign` |
| Constants | `pi`, `e` |
| Variables | `x`, `y`, `z`, `t`, `r` |

#### Method 2: Explicit Data Points (MATLAB-Style)

For measured data or pre-computed surfaces, use `data()` with explicit x, y vectors and z matrix:

```prolog
surface3d(measured_data, [
    title("Measured Data Surface"),
    data([0, 1, 2, 3],                    % X vector
         [0, 1, 2, 3],                    % Y vector
         [[0, 1, 4, 9],                   % Z matrix (row-major)
          [1, 2, 5, 10],
          [4, 5, 8, 13],
          [9, 10, 13, 18]]),
    colorscale(viridis)
]).
```

#### Method 3: Predefined Functions (Legacy)

For backward compatibility, predefined functions are still supported:

| Function | Formula |
|----------|---------|
| `sin_cos` | z = sin(x) * cos(y) |
| `paraboloid` | z = x² + y² |
| `saddle` | z = x² - y² |
| `ripple` | z = sin(r*3) / (r+0.1) |
| `gaussian` | z = e^(-(x²+y²)/2) |

### Defining 3D Scatter Plots

```prolog
scatter3d_spec(cluster_demo, [
    title("3D Cluster Visualization"),
    marker_size(8),
    colorscale(portland)
]).

scatter3d_point(cluster_demo, 1.0, 1.0, 1.0, [label("A1"), cluster(1)]).
scatter3d_point(cluster_demo, -1.0, -1.0, 1.0, [label("B1"), cluster(2)]).
```

### Defining 3D Lines

```prolog
line3d_spec(helix, [
    title("3D Helix"),
    line_width(3),
    color('#00d4ff')
]).

% Points are ordered by index
line3d_point(helix, 0, 1.0, 0.0, 0.0).
line3d_point(helix, 1, 0.809, 0.588, 0.2).
line3d_point(helix, 2, 0.309, 0.951, 0.4).
```

### Code Generation

```prolog
% Generate React/Plotly.js component
?- generate_plot3d_component(wave_surface, Code).

% Generate Python/matplotlib code
?- generate_plot3d_matplotlib(wave_surface, PyCode).

% Generate Python/Plotly code
?- generate_plot3d_plotly(wave_surface, PyCode).
```

## Export System

The export system provides declarative specifications for exporting visualizations to various formats including SVG, PNG, PDF, JSON, and CSV.

### Defining Export Configurations

```prolog
:- use_module('src/unifyweaver/glue/export_generator').

% Define export configuration for a chart type
export_config(my_chart, [
    formats([svg, png, pdf]),
    filename_template("chart-{date}-{title}"),
    default_size(800, 600),
    scale(2),
    background(white),
    include_styles(true)
]).

% Chart-specific export configs are predefined
% line_chart, bar_chart, scatter_plot, pie_chart, heatmap, network_graph, plot3d
```

### Supported Export Formats

| Format | Extension | Description | Use Case |
|--------|-----------|-------------|----------|
| `svg` | `.svg` | Scalable Vector Graphics | Web embedding, printing |
| `png` | `.png` | PNG Image (rasterized) | Screenshots, social media |
| `pdf` | `.pdf` | PDF Document | Reports, presentations |
| `json` | `.json` | JSON Data | Data interchange, APIs |
| `csv` | `.csv` | CSV Data | Spreadsheets, analysis |

### Code Generation

```prolog
% Generate complete export component
?- generate_export_component(my_chart, Component).

% Generate export hook for React
?- generate_export_hook(my_chart, Hook).

% Generate individual format exports
?- generate_svg_export(my_chart, SVGCode).
?- generate_png_export(my_chart, PNGCode).
?- generate_pdf_export(my_chart, PDFCode).

% Generate export menu with buttons
?- generate_export_menu(my_chart, MenuJSX).

% Generate export CSS
?- generate_export_css(CSS).
```

### Generated Export Hook

```typescript
// Generated: useExport.ts
export const useExport = (
  chartRef: RefObject<HTMLDivElement | SVGSVGElement>,
  data?: unknown[],
  title?: string
) => {
  const [isExporting, setIsExporting] = useState(false);

  const exportToSVG = async () => {
    // SVG serialization and download
  };

  const exportToPNG = async () => {
    // Canvas rendering with scale factor
  };

  const exportToPDF = async () => {
    // jsPDF document generation
  };

  const exportToJSON = async () => {
    // JSON serialization with formatting
  };

  const exportToCSV = async () => {
    // CSV generation from data array
  };

  return { exportToSVG, exportToPNG, exportToPDF, exportToJSON, exportToCSV, isExporting };
};
```

### Export Menu Component

```typescript
// Generated: ExportControls.tsx
export const ExportControls: React.FC<ExportProps> = ({ chartRef, data, title }) => {
  const { exportToSVG, exportToPNG, exportToPDF, isExporting } = useExport(chartRef, data, title);

  return (
    <div className="export-menu">
      <button onClick={exportToSVG} disabled={isExporting}>Export SVG</button>
      <button onClick={exportToPNG} disabled={isExporting}>Export PNG</button>
      <button onClick={exportToPDF} disabled={isExporting}>Export PDF</button>
    </div>
  );
};
```

### PNG Export with Scaling

PNG exports support scale factors for high-DPI displays:

```prolog
export_config(high_res_chart, [
    formats([png]),
    default_size(800, 600),
    scale(4),              % 4x scale for 3200x2400 output
    background(white)
]).
```

### PDF Export with jsPDF

PDF generation uses jsPDF library:

```prolog
export_format(pdf, [
    mime_type('application/pdf'),
    extension('.pdf'),
    description("PDF Document"),
    vector(true),
    library(jspdf)
]).
```

## Live Preview System

The live preview system provides a development environment with hot-reload capabilities for real-time visualization prototyping.

### Dev Server Configuration

```prolog
:- use_module('src/unifyweaver/glue/live_preview_generator').

% Configure development server
dev_server_config(my_project, [
    port(3000),
    host('localhost'),
    hot_reload(true),
    open_browser(true),
    watch_paths(['src/**/*.pl', 'src/**/*.ts', 'src/**/*.tsx']),
    ignore_paths(['node_modules', 'dist', '.git']),
    debounce_ms(100),
    https(false)
]).

% Visualization preview configuration
dev_server_config(visualization_preview, [
    port(3001),
    hot_reload(true),
    watch_paths([
        'src/unifyweaver/glue/**/*.pl',
        'src/**/*.ts',
        'src/**/*.tsx'
    ]),
    proxy([
        rule('/api', 'http://localhost:5000')
    ])
]).
```

### Preview Layout Configurations

```prolog
% Split layout with code editor
preview_config(chart_preview, [
    layout(split),
    editor_position(left),
    editor_width('35%'),
    preview_width('65%'),
    show_console(true),
    show_props_panel(true),
    show_data_panel(true),
    theme(dark),
    auto_refresh(true),
    controls([
        data_editor,
        props_inspector,
        export_options
    ])
]).

% Graph preview with bottom editor
preview_config(graph_preview, [
    layout(split),
    editor_position(bottom),
    editor_height('30%'),
    preview_height('70%'),
    show_console(true),
    controls([
        node_inspector,
        edge_editor,
        layout_selector
    ])
]).
```

### Code Generation

```prolog
% Generate Node.js dev server
?- generate_dev_server(my_project, ServerCode).

% Generate Vite configuration
?- generate_vite_config(my_project, ViteConfig).

% Generate preview application
?- generate_preview_app(my_chart, AppCode).

% Generate hot-reload React hook
?- generate_hot_reload_hook(HookCode).

% Generate state synchronization hook
?- generate_state_sync_hook(SyncHook).

% Generate preview wrapper component
?- generate_preview_wrapper(my_chart, WrapperCode).

% Generate code editor component
?- generate_code_editor(my_chart, EditorCode).

% Generate preview CSS with themes
?- generate_preview_css(CSS).
```

### Hot Reload Hook

```typescript
// Generated: useHotReload.ts
export const useHotReload = (onReload?: () => void) => {
  const [lastUpdate, setLastUpdate] = useState<Date | null>(null);
  const [isConnected, setIsConnected] = useState(false);

  useEffect(() => {
    const ws = new WebSocket(`ws://${window.location.host}/__hmr`);

    ws.onopen = () => setIsConnected(true);
    ws.onclose = () => setIsConnected(false);

    ws.onmessage = (event) => {
      const message = JSON.parse(event.data);
      if (message.type === 'reload') {
        setLastUpdate(new Date());
        onReload?.();
      }
    };

    return () => ws.close();
  }, [onReload]);

  return { lastUpdate, isConnected };
};
```

### State Synchronization Hook

```typescript
// Generated: useStateSync.ts
export const useStateSync = <T>(key: string, initialState: T) => {
  const [state, setState] = useState<T>(() => {
    const saved = sessionStorage.getItem(key);
    return saved ? JSON.parse(saved) : initialState;
  });

  useEffect(() => {
    sessionStorage.setItem(key, JSON.stringify(state));
  }, [key, state]);

  return [state, setState] as const;
};
```

### Preview App Structure

```
┌─────────────────────────────────────────────────────────────┐
│  Preview App                                      [Theme] ◐ │
├──────────────────────┬──────────────────────────────────────┤
│                      │                                      │
│   Code Editor        │      Live Preview                    │
│                      │                                      │
│   ┌──────────────┐   │   ┌────────────────────────────┐    │
│   │ Prolog spec  │   │   │                            │    │
│   │ ------------ │   │   │     [Visualization]        │    │
│   │ curve(...)   │   │   │                            │    │
│   │ plot_spec(.) │   │   │                            │    │
│   └──────────────┘   │   └────────────────────────────┘    │
│                      │                                      │
├──────────────────────┴──────────────────────────────────────┤
│  Props Panel │ Console │ Data Editor        [Export ▼]      │
└─────────────────────────────────────────────────────────────┘
```

### Preview Themes

The preview system supports dark and light themes:

```prolog
preview_config(my_preview, [
    theme(dark),   % or theme(light)
    ...
]).
```

Generated CSS includes both themes:

```css
/* Dark theme */
.preview-container[data-theme="dark"] {
  --bg-primary: #1a1a2e;
  --bg-secondary: #16213e;
  --text-primary: #e8e8e8;
  --border-color: #2d3748;
}

/* Light theme */
.preview-container[data-theme="light"] {
  --bg-primary: #ffffff;
  --bg-secondary: #f8fafc;
  --text-primary: #1a202c;
  --border-color: #e2e8f0;
}
```

### WebSocket Hot-Reload Architecture

```
┌────────────────┐         ┌─────────────────┐         ┌──────────────┐
│  File Watcher  │ ──────▶ │   Dev Server    │ ──────▶ │   Browser    │
│   (Chokidar)   │ change  │   (Express +    │   WS    │  (Preview    │
│                │         │   WebSocket)    │ reload  │   App)       │
└────────────────┘         └─────────────────┘         └──────────────┘
       │                          │                          │
       ▼                          ▼                          ▼
  .pl, .ts, .tsx            Vite middleware           React re-render
  file changes              + HMR endpoint            with new props
```

## Theme System

The theme generator provides centralized theme management with CSS custom properties and theme inheritance.

### Defining Themes

```prolog
:- use_module('src/unifyweaver/glue/theme_generator').

% Define a complete theme
theme(my_app, [
    name("My App Theme"),
    colors([
        primary('#3b82f6'),
        secondary('#10b981'),
        background('#ffffff'),
        surface('#f8fafc'),
        text_primary('#1a202c'),
        text_secondary('#64748b'),
        border('#e2e8f0')
    ]),
    typography([
        font_family("Inter, sans-serif"),
        font_size_base("16px"),
        line_height_base(1.5)
    ]),
    spacing([
        xs("0.25rem"),
        sm("0.5rem"),
        md("1rem"),
        lg("1.5rem"),
        xl("2rem")
    ]),
    borders([
        radius_sm("0.25rem"),
        radius_md("0.5rem"),
        radius_lg("1rem")
    ])
]).
```

### Theme Inheritance

Create theme variants by extending base themes:

```prolog
% Dark theme extends light theme
theme(my_app_dark, [
    extends(my_app),
    colors([
        background('#1a1a2e'),
        surface('#16213e'),
        text_primary('#e8e8e8'),
        text_secondary('#a0aec0'),
        border('#2d3748')
    ])
]).
```

### Code Generation

```prolog
% Generate CSS custom properties
?- generate_theme_css(my_app, CSS).

% Generate theme toggle component
?- generate_theme_toggle([light, dark], ToggleCode).

% Generate theme provider React component
?- generate_theme_provider(my_app, ProviderCode).

% Generate useTheme hook
?- generate_theme_hook(HookCode).
```

### Generated Theme CSS

```css
/* Generated: theme.css */
:root {
  --color-primary: #3b82f6;
  --color-secondary: #10b981;
  --color-background: #ffffff;
  --color-surface: #f8fafc;
  --color-text-primary: #1a202c;
  --color-text-secondary: #64748b;
  --color-border: #e2e8f0;

  --font-family: Inter, sans-serif;
  --font-size-base: 16px;
  --line-height-base: 1.5;

  --spacing-xs: 0.25rem;
  --spacing-sm: 0.5rem;
  --spacing-md: 1rem;
  --spacing-lg: 1.5rem;
  --spacing-xl: 2rem;

  --border-radius-sm: 0.25rem;
  --border-radius-md: 0.5rem;
  --border-radius-lg: 1rem;
}

[data-theme="dark"] {
  --color-background: #1a1a2e;
  --color-surface: #16213e;
  --color-text-primary: #e8e8e8;
  --color-text-secondary: #a0aec0;
  --color-border: #2d3748;
}
```

## Animation Presets

The animation presets module provides a curated library of CSS animations for entry, exit, and attention effects.

### Using Animation Presets

```prolog
:- use_module('src/unifyweaver/glue/animation_presets').

% Query available presets
?- animation_preset(Name, Category, _).
Name = fade_in, Category = entry ;
Name = fade_out, Category = exit ;
Name = pulse, Category = attention ;
...

% Get specific preset
?- animation_preset(bounce_in, entry, Props).
Props = [keyframes([...]), duration('0.6s'), timing('ease-out')].
```

### Available Preset Categories

**Entry Animations:**
- `fade_in`, `fade_in_up`, `fade_in_down`, `fade_in_left`, `fade_in_right`
- `slide_in_up`, `slide_in_down`, `slide_in_left`, `slide_in_right`
- `scale_in`, `scale_in_center`, `zoom_in`, `bounce_in`
- `flip_in_x`, `flip_in_y`

**Exit Animations:**
- `fade_out`, `fade_out_up`, `fade_out_down`
- `scale_out`, `zoom_out`

**Attention Animations:**
- `pulse`, `bounce`, `shake`, `wiggle`, `flash`, `heartbeat`, `jello`

**Chart-Specific Animations:**
- `chart_draw` - SVG path drawing effect
- `bar_grow` - Bar chart entry animation
- `pie_reveal` - Pie chart reveal effect
- `data_point_pop` - Scatter plot point animation
- `tooltip_appear` - Tooltip fade-in

### Code Generation

```prolog
% Generate CSS for specific preset
?- generate_preset_css(fade_in_up, CSS).

% Generate CSS for all presets
?- generate_all_presets_css(CSS).

% Generate useAnimation React hook
?- generate_preset_hook(HookCode).

% Get preset timing function
?- preset_timing(bounce_in, Timing).
Timing = 'cubic-bezier(0.68, -0.55, 0.265, 1.55)'.
```

### Generated Animation CSS

```css
/* Generated: animations.css */
@keyframes fade_in {
  from { opacity: 0; }
  to { opacity: 1; }
}

@keyframes fade_in_up {
  from { opacity: 0; transform: translateY(20px); }
  to { opacity: 1; transform: translateY(0); }
}

@keyframes bounce_in {
  0% { opacity: 0; transform: scale(0.3); }
  50% { opacity: 1; transform: scale(1.05); }
  70% { transform: scale(0.9); }
  100% { transform: scale(1); }
}

.animate-fade_in { animation: fade_in 0.3s ease-out forwards; }
.animate-fade_in_up { animation: fade_in_up 0.4s ease-out forwards; }
.animate-bounce_in { animation: bounce_in 0.6s cubic-bezier(0.68, -0.55, 0.265, 1.55) forwards; }
```

## Template Library

The template library provides pre-built dashboard and report templates that can be customized.

### Available Templates

```prolog
:- use_module('src/unifyweaver/glue/template_library').

% Query available templates
?- template(Name, Category, _).
Name = analytics_dashboard, Category = dashboard ;
Name = sales_report, Category = report ;
Name = data_explorer, Category = explorer ;
Name = presentation_slides, Category = presentation ;
...
```

### Template Categories

| Category | Templates | Description |
|----------|-----------|-------------|
| `dashboard` | `analytics_dashboard`, `executive_dashboard`, `ops_dashboard` | Real-time monitoring layouts |
| `report` | `sales_report`, `financial_report`, `quarterly_summary` | Print-ready report layouts |
| `explorer` | `data_explorer`, `log_viewer`, `metrics_explorer` | Interactive data browsing |
| `presentation` | `presentation_slides`, `chart_gallery` | Slide-based presentations |

### Defining Dashboard Templates

```prolog
% Define dashboard template
template(my_dashboard, dashboard, [
    title("My Analytics Dashboard"),
    layout(grid),
    regions([
        region(header, [row(1), col_span(3)]),
        region(sidebar, [row(2), col(1)]),
        region(main, [row(2), col_span(2)]),
        region(footer, [row(3), col_span(3)])
    ]),
    components([
        place(header, [nav_bar, theme_toggle]),
        place(sidebar, [filters, legend]),
        place(main, [chart, data_table]),
        place(footer, [export_controls, status])
    ])
]).
```

### Code Generation

```prolog
% Generate complete template
?- generate_template(analytics_dashboard, Code).

% Generate template layout CSS
?- generate_template_css(analytics_dashboard, CSS).

% Generate template component
?- generate_template_component(analytics_dashboard, Component).

% Customize template
?- customize_template(analytics_dashboard, [
       title("Sales Dashboard"),
       primary_color('#10b981')
   ], CustomCode).
```

### Generated Dashboard Component

```typescript
// Generated: AnalyticsDashboard.tsx
import React from 'react';
import styles from './AnalyticsDashboard.module.css';

interface DashboardProps {
  title?: string;
  data?: unknown;
}

export const AnalyticsDashboard: React.FC<DashboardProps> = ({
  title = "Analytics Dashboard",
  data
}) => {
  return (
    <div className={styles.dashboard}>
      <header className={styles.header}>
        <h1>{title}</h1>
        <ThemeToggle />
      </header>
      <aside className={styles.sidebar}>
        <FilterPanel />
        <Legend />
      </aside>
      <main className={styles.main}>
        <Chart data={data} />
        <DataTable data={data} />
      </main>
      <footer className={styles.footer}>
        <ExportControls />
        <Status />
      </footer>
    </div>
  );
};
```

## Performance Generators

The performance modules provide tools for handling large datasets efficiently.

### Lazy Loading Generator

Provides pagination, infinite scroll, and chunked loading patterns.

```prolog
:- use_module('src/unifyweaver/glue/lazy_loading_generator').

% Generate pagination hook
?- generate_pagination_hook([page_size(20)], Hook).

% Generate infinite scroll component
?- generate_infinite_scroll([threshold(200), batch_size(50)], Component).

% Generate lazy loader for images/components
?- generate_lazy_loader([placeholder(skeleton)], Loader).

% Generate chunked data fetcher
?- generate_chunked_fetcher([chunk_size(1000)], Fetcher).
```

### Generated Pagination Hook

```typescript
// Generated: usePagination.ts
export const usePagination = <T>(
  items: T[],
  pageSize: number = 20
) => {
  const [currentPage, setCurrentPage] = useState(1);

  const totalPages = Math.ceil(items.length / pageSize);
  const startIndex = (currentPage - 1) * pageSize;
  const endIndex = startIndex + pageSize;
  const currentItems = items.slice(startIndex, endIndex);

  const goToPage = (page: number) => {
    setCurrentPage(Math.max(1, Math.min(page, totalPages)));
  };

  return {
    currentItems,
    currentPage,
    totalPages,
    goToPage,
    goToNext: () => goToPage(currentPage + 1),
    goToPrev: () => goToPage(currentPage - 1),
    hasNext: currentPage < totalPages,
    hasPrev: currentPage > 1,
  };
};
```

### Virtual Scroll Generator

Provides efficient rendering for large lists, tables, and grids.

```prolog
:- use_module('src/unifyweaver/glue/virtual_scroll_generator').

% Generate virtual scroll hook
?- generate_virtual_scroll_hook([item_height(40), overscan(5)], Hook).

% Generate virtual list component
?- generate_virtual_list([height(400), item_height(40)], List).

% Generate virtual table with columns
?- generate_virtual_table([
       columns([name, email, role, status]),
       row_height(48)
   ], Table).

% Generate virtual grid
?- generate_virtual_grid([
       cell_width(200),
       cell_height(200),
       gap(16)
   ], Grid).
```

### Generated Virtual List

```typescript
// Generated: VirtualList.tsx
export const VirtualList = <T,>({
  items,
  height,
  itemHeight,
  renderItem,
  overscan = 5,
}: VirtualListProps<T>) => {
  const containerRef = useRef<HTMLDivElement>(null);
  const [scrollTop, setScrollTop] = useState(0);

  const totalHeight = items.length * itemHeight;
  const startIndex = Math.max(0, Math.floor(scrollTop / itemHeight) - overscan);
  const endIndex = Math.min(
    items.length - 1,
    Math.ceil((scrollTop + height) / itemHeight) + overscan
  );

  const visibleItems = items.slice(startIndex, endIndex + 1);
  const offsetY = startIndex * itemHeight;

  return (
    <div
      ref={containerRef}
      style={{ height, overflow: 'auto' }}
      onScroll={(e) => setScrollTop(e.currentTarget.scrollTop)}
    >
      <div style={{ height: totalHeight, position: 'relative' }}>
        <div style={{ transform: `translateY(${offsetY}px)` }}>
          {visibleItems.map((item, i) => (
            <div key={startIndex + i} style={{ height: itemHeight }}>
              {renderItem(item, startIndex + i)}
            </div>
          ))}
        </div>
      </div>
    </div>
  );
};
```

### WebWorker Generator

Generates WebWorker scripts for background data processing.

```prolog
:- use_module('src/unifyweaver/glue/webworker_generator').

% Generate data processing worker
?- generate_worker(data_processor, [
       operations([sort, filter, aggregate, transform])
   ], Worker).

% Generate chart calculation worker
?- generate_worker(chart_calculator, [
       operations([interpolate, smooth, downsample, statistics])
   ], Worker).

% Generate worker hook for React
?- generate_worker_hook(data_processor, Hook).

% Generate worker pool for parallel processing
?- generate_worker_pool([pool_size(4)], Pool).
```

### Generated Worker

```typescript
// Generated: dataProcessor.worker.ts
self.onmessage = (event: MessageEvent) => {
  const { type, data, options } = event.data;

  switch (type) {
    case 'sort':
      const sorted = [...data].sort((a, b) => {
        const aVal = a[options.key];
        const bVal = b[options.key];
        return options.desc ? bVal - aVal : aVal - bVal;
      });
      self.postMessage({ type: 'sorted', data: sorted });
      break;

    case 'filter':
      const filtered = data.filter((item: any) =>
        options.predicate(item)
      );
      self.postMessage({ type: 'filtered', data: filtered });
      break;

    case 'aggregate':
      const aggregated = aggregate(data, options);
      self.postMessage({ type: 'aggregated', data: aggregated });
      break;

    case 'statistics':
      const stats = calculateStatistics(data, options.field);
      self.postMessage({ type: 'statistics', data: stats });
      break;
  }
};
```

### Generated Worker Hook

```typescript
// Generated: useDataProcessor.ts
export const useDataProcessor = () => {
  const workerRef = useRef<Worker | null>(null);
  const [isProcessing, setIsProcessing] = useState(false);
  const [result, setResult] = useState<unknown>(null);

  useEffect(() => {
    workerRef.current = new Worker(
      new URL('./dataProcessor.worker.ts', import.meta.url)
    );

    workerRef.current.onmessage = (event) => {
      setResult(event.data.data);
      setIsProcessing(false);
    };

    return () => workerRef.current?.terminate();
  }, []);

  const process = useCallback((type: string, data: unknown[], options?: unknown) => {
    if (workerRef.current) {
      setIsProcessing(true);
      workerRef.current.postMessage({ type, data, options });
    }
  }, []);

  return { process, result, isProcessing };
};
```

## Testing

The integration tests verify all visualization glue modules:

```bash
# Run visualization glue tests
swipl -g "run_tests" -t halt tests/integration/glue/test_visualization_glue.pl

# Expected output:
# Results: 478/478 tests passed
# All tests passed!
```

### Test Coverage

| Module | Tests | Coverage |
|--------|-------|----------|
| graph_generator | 16 | Node/edge queries, component generation, CSS, config |
| curve_plot_generator | 17 | Curve queries, evaluation, component generation |
| matplotlib_generator | 16 | Curve queries, code generation, NumPy expressions |
| layout_generator | 8 | Default layouts, themes, CSS/JSX generation |
| layout_integration | 8 | Graph/curve with layout patterns |
| subplot_layout | 10 | Subplot CSS, JSX, matplotlib generation |
| control_system | 14 | Control definitions, JSX generation, state, CSS |
| wiring_system | 10 | Wiring specs, props, types, wired components |
| heatmap_generator | 9 | Specs, cells, dimensions, React/matplotlib |
| treemap_generator | 9 | Specs, nodes, hierarchy, React/Plotly |
| plot3d_generator | 11 | Surfaces, scatter, lines, React/matplotlib |
| responsive_generator | 15 | Breakpoints, hooks, containers, touch |
| accessibility_generator | 16 | ARIA, keyboard nav, color blindness |
| animation_generator | 13 | Keyframes, sequences, transitions |
| interaction_generator | 25 | Pan/zoom, brush, tooltips, drill-down |
| export_generator | 22 | SVG/PNG/PDF/JSON/CSV export, hooks, menus |
| live_preview_generator | 23 | Dev server, hot-reload, state sync, preview app |
| data_binding_generator | 29 | Reactive bindings, computed properties, WebSocket |
| theme_generator | 25 | Theme definitions, CSS variables, inheritance |
| animation_presets | 22 | Entry/exit/attention/chart animation presets |
| template_library | 24 | Dashboard/report/explorer templates |
| lazy_loading_generator | 26 | Pagination, infinite scroll, chunked loading |
| virtual_scroll_generator | 28 | Virtual list, table, grid components |
| webworker_generator | 25 | Data processing, chart calculation workers |

## Best Practices

### 1. Use Meaningful Names

```prolog
% Good: descriptive names
graph_node(user_alice, [label("Alice"), role(admin)]).
curve(revenue_growth, [type(exponential), label("Revenue")]).

% Avoid: generic names
graph_node(n1, [label("Node 1")]).
curve(c1, [type(linear)]).
```

### 2. Configure Themes Consistently

```prolog
% Define a shared theme
visualization_theme(corporate, [
    primary_color('#1e40af'),
    secondary_color('#3b82f6'),
    background('#f8fafc'),
    font('Inter, sans-serif')
]).

% Apply to visualizations
graph_spec(org_chart, [theme(corporate), ...]).
plot_spec(sales_chart, [theme(corporate), ...]).
```

### 3. Separate Data from Presentation

```prolog
% Data definition (reusable)
graph_node(dept_engineering, [label("Engineering"), headcount(42)]).
graph_node(dept_sales, [label("Sales"), headcount(28)]).

% Presentation specification (view-specific)
graph_spec(org_overview, [nodes([dept_engineering, dept_sales]), layout(breadthfirst)]).
graph_spec(dept_detail, [nodes([dept_engineering]), layout(cose), show_members(true)]).
```

## Summary

The visualization glue modules provide:

- **Declarative definitions** - Define graphs and plots in Prolog
- **Multi-target generation** - React/TypeScript for web, Python for data science
- **Layout system** - Declarative CSS Grid/Flexbox layouts with subplot support
- **Target-aware subplots** - Native matplotlib subplots or synthesized CSS grids
- **Control system** - Declarative UI controls (sliders, selects, checkboxes, etc.)
- **Wired components** - Controls automatically connected to visualization state
- **Runtime evaluation** - Evaluate curves programmatically
- **Responsive design** - Breakpoint-based layouts, mobile/tablet/desktop support
- **Accessibility** - ARIA attributes, keyboard navigation, screen reader support
- **Animation system** - CSS keyframes, sequences, transition orchestration
- **Interactions** - Pan/zoom, brush selection, tooltips, drill-down navigation
- **Export capabilities** - SVG, PNG, PDF, JSON, CSV with configurable options
- **Live preview** - Vite dev server with WebSocket hot-reload
- **Theme system** - Centralized themes with CSS custom properties and inheritance
- **Animation presets** - Curated library of entry, exit, attention, and chart animations
- **Template library** - Pre-built dashboard, report, explorer, and presentation templates
- **Lazy loading** - Pagination, infinite scroll, chunked loading for large datasets
- **Virtual scrolling** - Efficient rendering for lists, tables, and grids with thousands of items
- **WebWorkers** - Background data processing and chart calculations
- **Consistent patterns** - Same workflow as other UnifyWeaver glue modules
- **Full test coverage** - 478 integration tests across all modules

## What's Next?

- Explore the generated components in your React application
- Use matplotlib scripts for data analysis workflows
- Combine with RPyC bridges for remote visualization
- Extend with custom curve types or graph layouts
- Set up live preview for rapid visualization prototyping
- Export visualizations for reports and presentations
