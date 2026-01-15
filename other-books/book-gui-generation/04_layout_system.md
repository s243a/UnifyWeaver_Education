<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 4: Layout System

## Overview

The layout generator provides declarative specifications for CSS Grid, Flexbox, and absolute positioning. Define layouts in Prolog and generate CSS for web targets or equivalent layouts for mobile frameworks.

## Loading the Module

```prolog
:- use_module('src/unifyweaver/glue/layout_generator').
```

## Layout Strategies

| Strategy | Use Case |
|----------|----------|
| `grid` | Complex 2D layouts with named areas |
| `flex` | 1D layouts (rows or columns) |
| `absolute` | Overlapping elements, precise positioning |

## Grid Layouts

### Defining a Grid Layout

```prolog
layout(Name, grid, Options)
```

**Options:**
- `areas(List)` - 2D list of named regions
- `columns(List)` - Column sizes
- `rows(List)` - Row sizes
- `gap(Value)` - Gap between cells

**Example:**

```prolog
layout(dashboard, grid, [
    areas([
        ["header", "header"],
        ["sidebar", "content"],
        ["sidebar", "footer"]
    ]),
    columns(["280px", "1fr"]),
    rows(["60px", "1fr", "40px"]),
    gap("0")
]).
```

### Generating CSS

```prolog
?- generate_layout_css(dashboard, CSS).
```

**Generated:**

```css
.dashboard {
    display: grid;
    grid-template-areas: "header header"
                         "sidebar content"
                         "sidebar footer";
    grid-template-columns: 280px 1fr;
    grid-template-rows: 60px 1fr 40px;
    gap: 0;
}

.dashboard__header {
    grid-area: header;
}

.dashboard__sidebar {
    grid-area: sidebar;
}

.dashboard__content {
    grid-area: content;
}

.dashboard__footer {
    grid-area: footer;
}
```

### Pre-defined Layouts

Several common layout patterns are built in:

| Pattern | Description |
|---------|-------------|
| `single` | Single content area |
| `sidebar_content` | Left sidebar, main content |
| `content_sidebar` | Main content, right sidebar |
| `header_content` | Top header, main content |
| `header_content_footer` | Header, content, footer |
| `dashboard` | Header, sidebar, content, footer |
| `holy_grail` | Header, 3-column, footer |

**Usage:**

```prolog
?- default_layout(dashboard, Strategy, Options),
   layout(my_dashboard, Strategy, Options).
```

## Flexbox Layouts

### Defining a Flex Layout

```prolog
layout(Name, flex, Options)
```

**Options:**
- `direction(Dir)` - `row`, `column`, `row_reverse`, `column_reverse`
- `wrap(Wrap)` - `wrap`, `nowrap`, `wrap_reverse`
- `justify(Just)` - `start`, `end`, `center`, `space_between`, `space_around`, `space_evenly`
- `align(Align)` - `start`, `end`, `center`, `stretch`, `baseline`
- `gap(Value)` - Gap between items

**Example:**

```prolog
layout(toolbar, flex, [
    direction(row),
    justify(space_between),
    align(center),
    gap("1rem")
]).
```

**Generated CSS:**

```css
.toolbar {
    display: flex;
    flex-direction: row;
    flex-wrap: nowrap;
    justify-content: space-between;
    align-items: center;
    gap: 1rem;
}
```

## Absolute Positioning

### Defining Absolute Layouts

```prolog
layout(Name, absolute, Options)
```

**Options:**
List of `region(Name, Positioning)` where Positioning includes:
- `top(Value)`, `right(Value)`, `bottom(Value)`, `left(Value)`
- `width(Value)`, `height(Value)`
- `transform(Value)`
- `z_index(Number)`

**Example:**

```prolog
layout(overlay, absolute, [
    region(modal, [
        top('50%'),
        left('50%'),
        transform('translate(-50%, -50%)'),
        width('400px'),
        z_index(100)
    ]),
    region(backdrop, [
        top('0'),
        left('0'),
        right('0'),
        bottom('0'),
        z_index(99)
    ])
]).
```

## Placement

Associate components with layout regions:

```prolog
place(Layout, Region, Components)
```

**Example:**

```prolog
place(dashboard, header, [navbar, search_bar]).
place(dashboard, sidebar, [menu, user_info]).
place(dashboard, content, [main_view]).
place(dashboard, footer, [copyright, links]).
```

## Component Styling

### Basic Styles

```prolog
style(Component, Properties)
```

**Example:**

```prolog
style(card, [
    background('#ffffff'),
    border_radius('8px'),
    padding('16px'),
    box_shadow('0 2px 4px rgba(0,0,0,0.1)')
]).
```

### Selector-based Styles

```prolog
style(Component, Selector, Properties)
```

**Example:**

```prolog
style(button, ':hover', [
    background('#0056b3'),
    transform('scale(1.02)')
]).

style(input, ':focus', [
    border_color('#3b82f6'),
    outline('none')
]).
```

### Generating Component CSS

```prolog
?- generate_component_styles(card, CSS).
```

## Themes

### Defining Themes

```prolog
theme(Name, Properties)
```

**Built-in Themes:**

```prolog
theme(dark, [
    background('#1a1a2e'),
    surface('#16213e'),
    text('#e0e0e0'),
    text_secondary('#888888'),
    accent('#00d4ff'),
    accent_secondary('#7c3aed'),
    border('rgba(255,255,255,0.1)'),
    shadow('rgba(0,0,0,0.3)'),
    success('#22c55e'),
    warning('#f59e0b'),
    error('#ef4444')
]).

theme(light, [
    background('#f8fafc'),
    surface('#ffffff'),
    text('#1e293b'),
    text_secondary('#64748b'),
    accent('#7c3aed'),
    accent_secondary('#00d4ff'),
    border('#e2e8f0'),
    shadow('rgba(0,0,0,0.1)'),
    success('#22c55e'),
    warning('#f59e0b'),
    error('#ef4444')
]).
```

### Applying Themes

```prolog
component_theme(Component, ThemeName)
```

### Generating Theme CSS

```prolog
?- generate_theme_css(dark, CSS).
```

## Control Panels

Define interactive controls for data visualization:

### Defining Controls

```prolog
control(Name, Type, Options)
```

**Types:** `slider`, `checkbox`, `select`, `radio`, `number`, `text`

**Example:**

```prolog
control(amplitude, slider, [
    min(0), max(10), step(0.1), default(1),
    label('Amplitude')
]).

control(frequency, slider, [
    min(0.1), max(5), step(0.1), default(1),
    label('Frequency')
]).

control(show_grid, checkbox, [
    default(true),
    label('Show Grid')
]).
```

### Control Panels

```prolog
control_panel(Name, Controls)
```

**Example:**

```prolog
control_panel(wave_controls, [amplitude, frequency, show_grid]).
```

### Generating Control UI

```prolog
?- generate_control_panel_jsx(wave_controls, JSX).
?- generate_control_css(wave_controls, CSS).
?- generate_control_state(wave_controls, StateCode).
?- generate_control_handlers(wave_controls, HandlersCode).
```

## Control Wiring

Connect controls to visualization components:

```prolog
wiring_spec(Name, Mappings)
```

**Example:**

```prolog
wiring_spec(wave_viz, [
    amplitude -> 'waveAmplitude',
    frequency -> 'waveFrequency',
    show_grid -> 'showGrid'
]).
```

## HTML/JSX Generation

### Generate Layout HTML

```prolog
?- generate_layout_html(dashboard, HTML).
```

### Generate Layout JSX

```prolog
?- generate_layout_jsx(dashboard, JSX).
```

## Complete Example

```prolog
:- use_module('src/unifyweaver/glue/layout_generator').

% Define application layout
:- declare_layout(app_layout, grid, [
    areas([
        ["header", "header"],
        ["nav", "main"],
        ["footer", "footer"]
    ]),
    columns(["250px", "1fr"]),
    rows(["64px", "1fr", "48px"]),
    gap("0")
]).

% Style components
:- declare_style(header, [
    background('#1e293b'),
    color('#ffffff'),
    padding('0 24px'),
    display('flex'),
    align_items('center')
]).

:- declare_style(nav, [
    background('#f1f5f9'),
    padding('16px'),
    border_right('1px solid #e2e8f0')
]).

:- declare_style(main, [
    padding('24px'),
    background('#ffffff'),
    overflow('auto')
]).

:- declare_style(footer, [
    background('#f8fafc'),
    border_top('1px solid #e2e8f0'),
    padding('0 24px'),
    display('flex'),
    align_items('center'),
    justify_content('space-between')
]).

% Generate all styles
generate_app_styles(CSS) :-
    generate_layout_css(app_layout, LayoutCSS),
    generate_component_styles(header, HeaderCSS),
    generate_component_styles(nav, NavCSS),
    generate_component_styles(main, MainCSS),
    generate_component_styles(footer, FooterCSS),
    atomic_list_concat([
        LayoutCSS, HeaderCSS, NavCSS, MainCSS, FooterCSS
    ], '\n\n', CSS).
```

## Management Predicates

```prolog
% Add layout dynamically
declare_layout(Name, Strategy, Options)

% Add style dynamically
declare_style(Component, Properties)

% Add theme dynamically
declare_theme(Name, Properties)

% Clear all layouts
clear_layouts
```

---

**Previous**: [Chapter 3: Component Library](03_component_library.md) | **Next**: [Chapter 5: Data Binding](05_data_binding.md)
