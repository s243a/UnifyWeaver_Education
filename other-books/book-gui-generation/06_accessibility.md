<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 6: Accessibility

## Overview

The accessibility generator produces ARIA attributes, keyboard navigation handlers, focus management code, and screen reader announcements. Define accessibility requirements declaratively and generate compliant code for all targets.

## Loading the Module

```prolog
:- use_module('src/unifyweaver/glue/accessibility_generator').
```

## ARIA Specifications

### Defining ARIA Attributes

```prolog
aria_spec(Component, Attributes)
```

**Common Attributes:**
- `role(Role)` - ARIA role
- `label(String)` - aria-label
- `describedby(Id)` - aria-describedby
- `labelledby(Id)` - aria-labelledby
- `controls(Id)` - aria-controls
- `expanded(Bool)` - aria-expanded
- `selected(Bool)` - aria-selected
- `hidden(Bool)` - aria-hidden
- `disabled(Bool)` - aria-disabled
- `readonly(Bool)` - aria-readonly
- `multiselectable(Bool)` - aria-multiselectable

**Value Attributes (for sliders, progress bars):**
- `valuemin(Number)` - aria-valuemin
- `valuemax(Number)` - aria-valuemax
- `valuenow(Number)` - aria-valuenow
- `valuetext(String)` - aria-valuetext

**State Attributes:**
- `live(Mode)` - aria-live (`polite`, `assertive`, `off`)
- `atomic(Bool)` - aria-atomic
- `busy(Bool)` - aria-busy
- `current(Value)` - aria-current

**List/Tree Attributes:**
- `level(Number)` - aria-level
- `posinset(Number)` - aria-posinset
- `setsize(Number)` - aria-setsize
- `sort(Order)` - aria-sort

### Built-in ARIA Specs

**Charts:**

```prolog
aria_spec(line_chart, [
    role(img),
    label("Line chart visualization"),
    describedby(chart_description)
]).

aria_spec(bar_chart, [
    role(img),
    label("Bar chart visualization"),
    describedby(chart_description)
]).
```

**Interactive Components:**

```prolog
aria_spec(data_table, [
    role(grid),
    label("Data table"),
    multiselectable(false),
    readonly(true)
]).

aria_spec(slider_control, [
    role(slider),
    valuemin(0),
    valuemax(100),
    valuenow(50)
]).
```

**Navigation:**

```prolog
aria_spec(sidebar, [
    role(navigation),
    label("Sidebar navigation")
]).

aria_spec(main_content, [
    role(main),
    label("Main content area")
]).
```

### Custom ARIA Specs

```prolog
aria_spec(product_list, [
    role(list),
    label("Product catalog"),
    multiselectable(true)
]).

aria_spec(search_results, [
    role(region),
    label("Search results"),
    live(polite),
    atomic(true)
]).
```

## Keyboard Navigation

### Defining Keyboard Handlers

```prolog
keyboard_nav(Component, Handlers)
```

**Handler Format:**
```prolog
key(KeyName, Action)
```

**Common Key Names:**
- Arrow keys: `'ArrowUp'`, `'ArrowDown'`, `'ArrowLeft'`, `'ArrowRight'`
- Navigation: `'Home'`, `'End'`, `'PageUp'`, `'PageDown'`
- Actions: `'Enter'`, `' '` (Space), `'Escape'`, `'Tab'`

### Built-in Keyboard Navigation

**Data Table:**

```prolog
keyboard_nav(data_table, [
    key('ArrowUp', 'moveFocus("up")'),
    key('ArrowDown', 'moveFocus("down")'),
    key('ArrowLeft', 'moveFocus("left")'),
    key('ArrowRight', 'moveFocus("right")'),
    key('Home', 'moveFocus("first")'),
    key('End', 'moveFocus("last")'),
    key('Enter', 'activateCell()'),
    key('Escape', 'exitEditMode()'),
    key('Tab', 'moveFocus("next")')
]).
```

**Interactive Chart:**

```prolog
keyboard_nav(interactive_chart, [
    key('ArrowLeft', 'selectPreviousPoint()'),
    key('ArrowRight', 'selectNextPoint()'),
    key('ArrowUp', 'selectPreviousSeries()'),
    key('ArrowDown', 'selectNextSeries()'),
    key('Enter', 'showPointDetails()'),
    key('Escape', 'clearSelection()'),
    key(' ', 'togglePointSelection()')
]).
```

**Slider:**

```prolog
keyboard_nav(slider, [
    key('ArrowLeft', 'decrementValue(1)'),
    key('ArrowRight', 'incrementValue(1)'),
    key('ArrowUp', 'incrementValue(1)'),
    key('ArrowDown', 'decrementValue(1)'),
    key('PageUp', 'incrementValue(10)'),
    key('PageDown', 'decrementValue(10)'),
    key('Home', 'setMinValue()'),
    key('End', 'setMaxValue()')
]).
```

**Tab List:**

```prolog
keyboard_nav(tablist, [
    key('ArrowLeft', 'selectPreviousTab()'),
    key('ArrowRight', 'selectNextTab()'),
    key('Home', 'selectFirstTab()'),
    key('End', 'selectLastTab()'),
    key('Enter', 'activateTab()'),
    key(' ', 'activateTab()')
]).
```

**Modal:**

```prolog
keyboard_nav(modal, [
    key('Escape', 'closeModal()'),
    key('Tab', 'trapFocus()')
]).
```

## Focus Traps

Contain focus within a component (for modals, dropdowns):

```prolog
focus_trap(Component, Options)
```

**Options:**
- `container_selector(Selector)` - CSS selector for container
- `initial_focus(Selector)` - Element to focus on open
- `return_focus(Bool)` - Return focus to trigger on close
- `escape_closes(Bool)` - Close on Escape key
- `outside_click_closes(Bool)` - Close on outside click

**Examples:**

```prolog
focus_trap(modal_dialog, [
    container_selector('.modal'),
    initial_focus('.modal-close'),
    return_focus(true)
]).

focus_trap(dropdown_menu, [
    container_selector('.dropdown-menu'),
    initial_focus('.dropdown-item:first-child'),
    escape_closes(true)
]).

focus_trap(sidebar_expanded, [
    container_selector('.sidebar'),
    escape_closes(true),
    outside_click_closes(true)
]).
```

## Live Regions

Announce dynamic content changes to screen readers:

```prolog
live_region(Name, Options)
```

**Options:**
- `aria_live(Mode)` - `polite` (wait for pause) or `assertive` (interrupt)
- `aria_atomic(Bool)` - Announce entire region or just changes
- `aria_busy(Bool)` - Region is updating
- `role(Role)` - `status`, `alert`, `log`, `timer`

**Examples:**

```prolog
% Polite updates (charts, status)
live_region(chart_updates, [
    aria_live(polite),
    aria_atomic(true),
    role(status)
]).

% Assertive alerts (errors)
live_region(error_messages, [
    aria_live(assertive),
    aria_atomic(true),
    role(alert)
]).

% Loading indicators
live_region(loading_status, [
    aria_live(polite),
    aria_busy(true),
    role(status)
]).
```

## Skip Links

Allow keyboard users to skip repetitive content:

```prolog
skip_link(Name, Target)
```

**Target:** CSS selector for skip destination

**Examples:**

```prolog
skip_link(main, '#main-content').
skip_link(nav, '#navigation').
skip_link(chart, '#chart-container').
skip_link(controls, '#control-panel').
```

## Code Generation

### Generate ARIA Props

```prolog
?- generate_aria_props(data_table, Props).
```

**Output:**

```javascript
{
  role: "grid",
  "aria-label": "Data table",
  "aria-multiselectable": false,
  "aria-readonly": true
}
```

### Generate ARIA JSX

```prolog
?- generate_aria_jsx(data_table, JSX).
```

**Output:**

```jsx
{...{
  role: "grid",
  "aria-label": "Data table",
  "aria-multiselectable": false,
  "aria-readonly": true
}}
```

### Generate Keyboard Handler

```prolog
?- generate_keyboard_handler(data_table, Handler).
```

**Output:**

```typescript
const handleKeyDown = (event: React.KeyboardEvent) => {
  switch (event.key) {
    case 'ArrowUp':
      moveFocus("up");
      event.preventDefault();
      break;
    case 'ArrowDown':
      moveFocus("down");
      event.preventDefault();
      break;
    // ... more cases
  }
};
```

### Generate Focus Trap

```prolog
?- generate_focus_trap_jsx(modal_dialog, JSX).
```

### Generate Skip Links

```prolog
?- generate_skip_links_jsx(app, JSX).
```

### Generate Live Region

```prolog
?- generate_live_region_jsx(chart_updates, JSX).
```

### Generate Accessibility CSS

```prolog
?- generate_accessibility_css(app, CSS).
```

Generates CSS for:
- Skip link visibility (hidden until focused)
- Focus outlines
- Screen reader-only text
- Reduced motion preferences

## Management Predicates

```prolog
% Add ARIA spec dynamically
declare_aria_spec(Component, Attributes)

% Add keyboard navigation dynamically
declare_keyboard_nav(Component, Handlers)

% Clear all accessibility specs
clear_accessibility
```

## Complete Example

```prolog
:- use_module('src/unifyweaver/glue/accessibility_generator').

% Define accessible dashboard
:- declare_aria_spec(dashboard, [
    role(application),
    label("Analytics Dashboard")
]).

:- declare_aria_spec(metrics_panel, [
    role(region),
    label("Key Metrics"),
    live(polite)
]).

:- declare_aria_spec(chart_view, [
    role(img),
    label("Revenue trend over time"),
    describedby(chart_desc)
]).

:- keyboard_nav(chart_view, [
    key('ArrowLeft', 'selectPreviousDataPoint()'),
    key('ArrowRight', 'selectNextDataPoint()'),
    key('Enter', 'showDataPointDetails()'),
    key('Escape', 'clearSelection()')
]).

:- live_region(data_updates, [
    aria_live(polite),
    aria_atomic(false),
    role(log)
]).

:- skip_link(main_chart, '#main-chart').
:- skip_link(controls, '#chart-controls').

% Generate all accessibility code
generate_dashboard_a11y :-
    generate_aria_props(dashboard, DashProps),
    generate_aria_props(chart_view, ChartProps),
    generate_keyboard_handler(chart_view, KeyHandler),
    generate_skip_links_jsx(app, SkipLinks),
    generate_live_region_jsx(data_updates, LiveRegion),
    generate_accessibility_css(dashboard, CSS),
    format('Dashboard Props: ~w~n', [DashProps]),
    format('Chart Props: ~w~n', [ChartProps]),
    format('Keyboard Handler: ~w~n', [KeyHandler]),
    format('Skip Links: ~w~n', [SkipLinks]),
    format('Live Region: ~w~n', [LiveRegion]),
    format('CSS: ~w~n', [CSS]).
```

---

**Previous**: [Chapter 5: Data Binding](05_data_binding.md) | **Next**: [Chapter 7: Responsive Design](07_responsive_design.md)
