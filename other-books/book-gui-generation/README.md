<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Book: GUI Generation

A guide to generating complete frontend applications from declarative Prolog specifications using UnifyWeaver's GUI generation system.

## Status: Complete

## Contents

1. [Introduction](01_introduction.md) - Overview and supported targets
2. [App Generation](02_app_generation.md) - Full project scaffolding (Vue, React Native, Flutter, SwiftUI)
3. [Component Library](03_component_library.md) - Pre-built UI patterns
4. [Layout System](04_layout_system.md) - Grid, Flexbox, placement
5. [Data Binding](05_data_binding.md) - State, reactivity, WebSocket sync
6. [Accessibility](06_accessibility.md) - ARIA, keyboard navigation
7. [Responsive Design](07_responsive_design.md) - Breakpoints, adaptive layouts
8. [Theming](08_theming.md) - Colors, typography, dark mode
9. [Visualization Modules](09_visualization_modules.md) - Self-contained HTML generation (Cytoscape.js, Chart.js, Pyodide)

## Prerequisites

- SWI-Prolog 8.0+
- Node.js 18+ (for Vue/React)
- Flutter SDK (for Flutter target)
- Xcode (for SwiftUI target)

## Quick Start

```prolog
?- use_module('src/unifyweaver/glue/app_generator').
?- generate_complete_project(
       app(myapp, [
           navigation(tabs, [
               screen(home, 'HomeScreen', []),
               screen(settings, 'SettingsScreen', [])
           ], []),
           theme(default, [
               colors([primary('#3b82f6')])
           ])
       ]),
       [frontend-vue],
       '/tmp/myapp',
       Result
   ).
```

## Key Features

- **Multi-Target**: Generate Vue, React Native, Flutter, or SwiftUI from the same spec
- **Component Library**: 20+ pre-built components (modals, toasts, cards, etc.)
- **Declarative Layouts**: CSS Grid and Flexbox from Prolog
- **Reactive Data Binding**: Connect Prolog facts to UI state
- **Accessibility Built-in**: ARIA attributes and keyboard navigation
- **Responsive**: Breakpoint-based adaptive layouts
- **Visualization Modules**: Self-contained Prolog modules that generate complete HTML apps with Cytoscape.js, Chart.js, or Pyodide

## See Also

- [Cross-Target Glue](../../book-07-cross-target-glue/) - Multi-language pipeline composition
- [TypeScript Target](../book-typescript-target/) - TypeScript code generation
- [WASM Target](../book-wasm-target/) - WebAssembly with browser visualization
