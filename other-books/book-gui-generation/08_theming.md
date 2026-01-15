<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 8: Theming

## Overview

The theme generator provides centralized theme definitions including colors, typography, spacing, borders, shadows, and transitions. Define themes declaratively and generate CSS custom properties, React theme providers, and theme switching functionality.

## Loading the Module

```prolog
:- use_module('src/unifyweaver/glue/theme_generator').
```

## Theme Definitions

### Defining a Theme

```prolog
theme(Name, Options)
```

**Options:**
- `colors(List)` - Color definitions
- `typography(List)` - Font settings
- `spacing(List)` - Spacing scale
- `borders(List)` - Border radii and widths
- `shadows(List)` - Box shadow definitions
- `transitions(List)` - Animation timings

### Built-in Themes

**Light Theme:**

```prolog
theme(light, [
    colors([
        primary('#3b82f6'),
        secondary('#64748b'),
        accent('#8b5cf6'),
        success('#10b981'),
        warning('#f59e0b'),
        error('#ef4444'),
        background('#ffffff'),
        surface('#f8fafc'),
        text_primary('#0f172a'),
        text_secondary('#475569'),
        text_muted('#94a3b8'),
        border('#e2e8f0'),
        shadow('rgba(0, 0, 0, 0.1)')
    ]),
    typography([
        font_family('"Inter", -apple-system, sans-serif'),
        font_mono('"JetBrains Mono", monospace'),
        base_size(16),
        line_height(1.5),
        heading_weight(600),
        body_weight(400)
    ]),
    spacing([
        unit(4),
        xs(4), sm(8), md(16), lg(24), xl(32), xxl(48)
    ]),
    borders([
        radius_sm(4), radius_md(8), radius_lg(12), radius_full(9999),
        width(1)
    ]),
    shadows([
        sm('0 1px 2px 0 rgba(0, 0, 0, 0.05)'),
        md('0 4px 6px -1px rgba(0, 0, 0, 0.1)'),
        lg('0 10px 15px -3px rgba(0, 0, 0, 0.1)'),
        xl('0 20px 25px -5px rgba(0, 0, 0, 0.1)')
    ]),
    transitions([
        duration_fast(150),
        duration_normal(300),
        duration_slow(500),
        easing('cubic-bezier(0.4, 0, 0.2, 1)')
    ])
]).
```

**Dark Theme:**

```prolog
theme(dark, [
    colors([
        primary('#60a5fa'),
        secondary('#94a3b8'),
        accent('#a78bfa'),
        success('#34d399'),
        warning('#fbbf24'),
        error('#f87171'),
        background('#0f172a'),
        surface('#1e293b'),
        text_primary('#f8fafc'),
        text_secondary('#cbd5e1'),
        text_muted('#64748b'),
        border('#334155'),
        shadow('rgba(0, 0, 0, 0.3)')
    ]),
    % ... same typography, spacing, borders
]).
```

## Color Palettes

### Built-in Palettes

Tailwind-style color scales:

```prolog
color_palette(slate, [
    c50('#f8fafc'), c100('#f1f5f9'), c200('#e2e8f0'),
    c300('#cbd5e1'), c400('#94a3b8'), c500('#64748b'),
    c600('#475569'), c700('#334155'), c800('#1e293b'),
    c900('#0f172a'), c950('#020617')
]).

color_palette(blue, [
    c50('#eff6ff'), c100('#dbeafe'), c200('#bfdbfe'),
    c300('#93c5fd'), c400('#60a5fa'), c500('#3b82f6'),
    c600('#2563eb'), c700('#1d4ed8'), c800('#1e40af'),
    c900('#1e3a8a'), c950('#172554')
]).

color_palette(emerald, [...]).
color_palette(red, [...]).
color_palette(amber, [...]).
color_palette(violet, [...]).
```

### Semantic Colors

Map palette colors to semantic meanings:

```prolog
semantic_colors(my_theme, [
    primary -> blue-c500,
    primary_hover -> blue-c600,
    success -> emerald-c500,
    error -> red-c500,
    warning -> amber-c500
]).
```

## Typography

### Defining Typography

```prolog
typography(Name, Options)
```

**Options:**
- `font_family(String)` - Font stack
- `font_mono(String)` - Monospace font stack
- `base_size(Pixels)` - Base font size
- `line_height(Number)` - Line height ratio
- `heading_weight(Number)` - Heading font weight
- `body_weight(Number)` - Body font weight

### Font Scale

Define a modular type scale:

```prolog
font_scale(modern, [
    xs(0.75),      % 12px at base 16
    sm(0.875),     % 14px
    base(1),       % 16px
    lg(1.125),     % 18px
    xl(1.25),      % 20px
    h4(1.5),       % 24px
    h3(1.875),     % 30px
    h2(2.25),      % 36px
    h1(3)          % 48px
]).
```

## Spacing

### Spacing Scale

```prolog
spacing_scale(default, [
    px(1),    % 1px
    0.5(2),   % 2px
    1(4),     % 4px
    2(8),     % 8px
    3(12),    % 12px
    4(16),    % 16px
    5(20),    % 20px
    6(24),    % 24px
    8(32),    % 32px
    10(40),   % 40px
    12(48),   % 48px
    16(64),   % 64px
    20(80),   % 80px
    24(96)    % 96px
]).
```

## Theme Inheritance

Create themes that extend others:

```prolog
theme_extends(high_contrast, light).

theme(high_contrast, [
    colors([
        text_primary('#000000'),
        text_secondary('#1f2937'),
        border('#000000')
    ])
]).
```

The child theme inherits all properties from the parent, with overrides applied.

## Code Generation

### Generate Theme CSS

```prolog
?- generate_theme_css(light, CSS).
```

**Output:**

```css
:root {
    --color-primary: #3b82f6;
    --color-secondary: #64748b;
    --color-accent: #8b5cf6;
    --color-success: #10b981;
    --color-warning: #f59e0b;
    --color-error: #ef4444;
    --color-background: #ffffff;
    --color-surface: #f8fafc;
    --color-text-primary: #0f172a;
    --color-text-secondary: #475569;
    --color-text-muted: #94a3b8;
    --color-border: #e2e8f0;

    --font-family: "Inter", -apple-system, sans-serif;
    --font-mono: "JetBrains Mono", monospace;
    --font-size-base: 16px;
    --line-height: 1.5;

    --spacing-xs: 4px;
    --spacing-sm: 8px;
    --spacing-md: 16px;
    --spacing-lg: 24px;
    --spacing-xl: 32px;

    --radius-sm: 4px;
    --radius-md: 8px;
    --radius-lg: 12px;

    --shadow-sm: 0 1px 2px 0 rgba(0, 0, 0, 0.05);
    --shadow-md: 0 4px 6px -1px rgba(0, 0, 0, 0.1);
    --shadow-lg: 0 10px 15px -3px rgba(0, 0, 0, 0.1);

    --transition-fast: 150ms;
    --transition-normal: 300ms;
    --transition-easing: cubic-bezier(0.4, 0, 0.2, 1);
}
```

### Generate Theme Variables

```prolog
?- generate_theme_variables(light, Vars).
```

Returns just the CSS custom property declarations.

### Generate Theme Provider

```prolog
?- generate_theme_provider([light, dark], JSX).
```

Generates a React Context provider for theme management.

### Generate Theme Hook

```prolog
?- generate_theme_hook(Hook).
```

Generates a `useTheme()` hook:

```typescript
export const useTheme = () => {
  const context = useContext(ThemeContext);
  if (!context) {
    throw new Error('useTheme must be used within ThemeProvider');
  }
  return context;
};
```

### Generate Theme Toggle

```prolog
?- generate_theme_toggle([light, dark], Toggle).
```

Generates a theme toggle component with system preference detection.

### Generate Theme Types

```prolog
?- generate_theme_types([light, dark], Types).
```

Generates TypeScript types for the theme object.

## Utility Predicates

```prolog
% Get colors from a theme
?- get_theme_colors(light, Colors).

% Get typography from a theme
?- get_theme_typography(light, Typography).

% Get spacing from a theme
?- get_theme_spacing(light, Spacing).

% Resolve theme with inheritance
?- resolve_theme(high_contrast, ResolvedOptions).
```

## Management Predicates

```prolog
% Add theme dynamically
declare_theme(Name, Options)

% Clear all themes
clear_themes
```

## Complete Example

```prolog
:- use_module('src/unifyweaver/glue/theme_generator').

% Define corporate theme
:- declare_theme(corporate, [
    colors([
        primary('#1e40af'),
        secondary('#3b82f6'),
        accent('#0ea5e9'),
        success('#22c55e'),
        warning('#eab308'),
        error('#dc2626'),
        background('#f8fafc'),
        surface('#ffffff'),
        text_primary('#1e293b'),
        text_secondary('#64748b'),
        border('#e2e8f0')
    ]),
    typography([
        font_family('"IBM Plex Sans", sans-serif'),
        font_mono('"IBM Plex Mono", monospace'),
        base_size(16),
        line_height(1.6),
        heading_weight(700)
    ]),
    spacing([
        xs(4), sm(8), md(16), lg(24), xl(32), xxl(48)
    ]),
    borders([
        radius_sm(2), radius_md(4), radius_lg(8)
    ])
]).

% Dark variant
:- theme_extends(corporate_dark, corporate).
:- declare_theme(corporate_dark, [
    colors([
        primary('#60a5fa'),
        background('#0f172a'),
        surface('#1e293b'),
        text_primary('#f1f5f9'),
        text_secondary('#94a3b8'),
        border('#334155')
    ])
]).

% Generate all theme assets
generate_corporate_themes :-
    generate_theme_css(corporate, LightCSS),
    generate_theme_css(corporate_dark, DarkCSS),
    generate_theme_provider([corporate, corporate_dark], Provider),
    generate_theme_hook(Hook),
    generate_theme_toggle([corporate, corporate_dark], Toggle),
    format('/* Light Theme */~n~w~n~n', [LightCSS]),
    format('/* Dark Theme */~n~w~n~n', [DarkCSS]),
    format('// Provider~n~w~n~n', [Provider]),
    format('// Hook~n~w~n~n', [Hook]),
    format('// Toggle~n~w~n', [Toggle]).
```

---

**Previous**: [Chapter 7: Responsive Design](07_responsive_design.md) | [Back to Book Index](README.md)
