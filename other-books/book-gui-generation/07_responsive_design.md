<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 7: Responsive Design

## Overview

The responsive design generator creates CSS media queries and adaptive layouts from declarative specifications. Define breakpoints and layout variants in Prolog, then generate responsive CSS that works across all screen sizes.

## Loading the Module

```prolog
:- use_module('src/unifyweaver/glue/responsive_generator').
```

## Breakpoints

### Defining Breakpoints

```prolog
breakpoint(Name, Condition)
```

**Conditions:**
- `min_width(Pixels)` - Minimum viewport width
- `max_width(Pixels)` - Maximum viewport width
- `range(Min, Max)` - Width range

### Built-in Breakpoints

**Standard Breakpoints (Bootstrap-style):**

```prolog
breakpoint(xs, max_width(575)).    % Extra small
breakpoint(sm, min_width(576)).    % Small
breakpoint(md, min_width(768)).    % Medium
breakpoint(lg, min_width(992)).    % Large
breakpoint(xl, min_width(1200)).   % Extra large
breakpoint(xxl, min_width(1400)).  % Extra extra large
```

**Semantic Breakpoints:**

```prolog
breakpoint(mobile, max_width(767)).
breakpoint(tablet, range(768, 1023)).
breakpoint(desktop, min_width(1024)).
breakpoint(wide, min_width(1440)).
```

**Device-Specific Breakpoints:**

```prolog
breakpoint(phone_portrait, max_width(480)).
breakpoint(phone_landscape, range(481, 767)).
breakpoint(tablet_portrait, range(768, 1024)).
breakpoint(tablet_landscape, range(1025, 1279)).
```

### Custom Breakpoints

```prolog
breakpoint(small_phone, max_width(375)).
breakpoint(phablet, range(376, 599)).
breakpoint(large_desktop, min_width(1920)).
```

## Responsive Layouts

### Defining Responsive Layouts

```prolog
responsive_layout(Name, Variants)
```

**Variants Format:**
```prolog
[
    default(Properties),
    at(Breakpoint, Properties),
    at(AnotherBreakpoint, Properties),
    ...
]
```

### Built-in Responsive Layouts

**Collapsible Sidebar:**

```prolog
responsive_layout(collapsible_sidebar, [
    default([
        strategy(grid),
        areas([["sidebar", "main"]]),
        columns(["280px", "1fr"]),
        gap("1rem")
    ]),
    at(mobile, [
        areas([["main"], ["sidebar"]]),
        columns(["1fr"]),
        sidebar_position(bottom)
    ]),
    at(tablet, [
        columns(["220px", "1fr"])
    ])
]).
```

**Adaptive Stack:**

```prolog
responsive_layout(adaptive_stack, [
    default([
        strategy(flex),
        direction(column),
        gap("1rem")
    ]),
    at(md, [
        direction(row),
        wrap(wrap)
    ]),
    at(lg, [
        direction(row),
        wrap(nowrap)
    ])
]).
```

**Card Grid:**

```prolog
responsive_layout(card_grid, [
    default([
        strategy(grid),
        columns(["1fr"]),
        gap("1rem")
    ]),
    at(sm, [
        columns(["repeat(2, 1fr)"])
    ]),
    at(md, [
        columns(["repeat(3, 1fr)"])
    ]),
    at(lg, [
        columns(["repeat(4, 1fr)"])
    ])
]).
```

**Dashboard:**

```prolog
responsive_layout(dashboard, [
    default([
        strategy(grid),
        areas([["nav"], ["main"], ["aside"]]),
        columns(["1fr"]),
        rows(["auto", "1fr", "auto"])
    ]),
    at(md, [
        areas([["nav", "nav"], ["main", "aside"]]),
        columns(["1fr", "300px"]),
        rows(["auto", "1fr"])
    ]),
    at(lg, [
        areas([["nav", "nav", "nav"], ["sidebar", "main", "aside"]]),
        columns(["240px", "1fr", "300px"]),
        rows(["auto", "1fr"])
    ])
]).
```

### Custom Responsive Layouts

```prolog
responsive_layout(product_grid, [
    default([
        strategy(grid),
        columns(["1fr"]),
        gap("16px"),
        padding("16px")
    ]),
    at(sm, [
        columns(["repeat(2, 1fr)"]),
        gap("20px")
    ]),
    at(md, [
        columns(["repeat(3, 1fr)"]),
        gap("24px"),
        padding("24px")
    ]),
    at(xl, [
        columns(["repeat(4, 1fr)"]),
        gap("32px"),
        max_width("1400px"),
        margin("0 auto")
    ])
]).
```

## Responsive Styles

Apply different styles at different breakpoints:

```prolog
responsive_style(Name, Variants)
```

**Example:**

```prolog
responsive_style(heading, [
    default([
        font_size("1.5rem"),
        line_height("1.2")
    ]),
    at(md, [
        font_size("2rem")
    ]),
    at(lg, [
        font_size("2.5rem"),
        letter_spacing("-0.02em")
    ])
]).

responsive_style(card, [
    default([
        padding("16px"),
        border_radius("8px")
    ]),
    at(md, [
        padding("24px"),
        border_radius("12px")
    ])
]).
```

## Container Queries

Modern CSS container queries for component-level responsiveness:

```prolog
container(Name, Options)
```

**Options:**
- `type(Type)` - `inline-size`, `size`, `normal`
- `name(String)` - Container name

**Example:**

```prolog
container(card_container, [
    type(inline_size),
    name("card")
]).
```

## Code Generation

### Generate Media Query

```prolog
?- generate_media_query(tablet, Query).
% Query = '@media (min-width: 768px) and (max-width: 1023px)'
```

### Generate Responsive CSS

```prolog
?- generate_responsive_css(card_grid, CSS).
```

**Output:**

```css
.card-grid {
    display: grid;
    grid-template-columns: 1fr;
    gap: 1rem;
}

@media (min-width: 576px) {
    .card-grid {
        grid-template-columns: repeat(2, 1fr);
    }
}

@media (min-width: 768px) {
    .card-grid {
        grid-template-columns: repeat(3, 1fr);
    }
}

@media (min-width: 992px) {
    .card-grid {
        grid-template-columns: repeat(4, 1fr);
    }
}
```

### Generate Breakpoint-Specific CSS

```prolog
?- generate_breakpoint_css(mobile, 'nav', [display(none)], CSS).
```

**Output:**

```css
@media (max-width: 767px) {
    .nav {
        display: none;
    }
}
```

### Generate Container CSS

```prolog
?- generate_container_css(card_container, CSS).
```

## Responsive Strategy

Set mobile-first or desktop-first approach:

```prolog
% Mobile-first (default)
?- set_responsive_strategy(mobile_first).

% Desktop-first
?- set_responsive_strategy(desktop_first).

% Check current strategy
?- is_mobile_first.
```

## Management Predicates

```prolog
% Add breakpoint dynamically
declare_breakpoint(Name, Condition)

% Add responsive layout dynamically
declare_responsive_layout(Name, Variants)

% Clear all responsive specs
clear_responsive
```

## Complete Example

```prolog
:- use_module('src/unifyweaver/glue/responsive_generator').

% Define custom breakpoints
:- declare_breakpoint(compact, max_width(599)).
:- declare_breakpoint(regular, range(600, 959)).
:- declare_breakpoint(expanded, min_width(960)).

% Define responsive navigation
:- declare_responsive_layout(nav_layout, [
    default([
        strategy(flex),
        direction(column),
        position(fixed),
        bottom("0"),
        left("0"),
        right("0"),
        height("60px")
    ]),
    at(regular, [
        direction(row),
        position(static),
        height("auto")
    ]),
    at(expanded, [
        direction(column),
        position(fixed),
        left("0"),
        top("0"),
        bottom("0"),
        width("280px")
    ])
]).

% Define responsive content area
:- declare_responsive_layout(content_layout, [
    default([
        padding("16px"),
        margin_bottom("60px")
    ]),
    at(regular, [
        padding("24px"),
        margin_bottom("0")
    ]),
    at(expanded, [
        margin_left("280px"),
        padding("32px")
    ])
]).

% Define responsive typography
:- responsive_style(body_text, [
    default([font_size("14px"), line_height("1.5")]),
    at(regular, [font_size("16px")]),
    at(expanded, [font_size("18px"), line_height("1.6")])
]).

% Generate all responsive CSS
generate_app_responsive_css(CSS) :-
    generate_responsive_css(nav_layout, NavCSS),
    generate_responsive_css(content_layout, ContentCSS),
    atomic_list_concat([NavCSS, ContentCSS], '\n\n', CSS).
```

## Best Practices

1. **Mobile-First**: Start with mobile styles, add complexity for larger screens
2. **Semantic Breakpoints**: Use `mobile`, `tablet`, `desktop` for clarity
3. **Content-Based**: Choose breakpoints based on content, not devices
4. **Test Thoroughly**: Check layouts at all breakpoint boundaries
5. **Limit Breakpoints**: Use 3-4 breakpoints maximum for maintainability

---

**Previous**: [Chapter 6: Accessibility](06_accessibility.md) | **Next**: [Chapter 8: Theming](08_theming.md)
