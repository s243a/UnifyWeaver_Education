<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 3: Component Library

## Overview

The component library provides pre-built UI component patterns that compile to all supported targets (Vue, React Native, Flutter, SwiftUI). Define components declaratively in Prolog, then generate framework-specific code.

## Loading the Module

```prolog
:- use_module('src/unifyweaver/components/component_library').
```

## Component Categories

### Modal/Dialog Components

| Predicate | Description |
|-----------|-------------|
| `modal/3` | Generic modal dialog |
| `alert_dialog/3` | Alert with confirm button |
| `bottom_sheet/3` | Slide-up sheet |
| `action_sheet/3` | List of action options |

### Feedback Components

| Predicate | Description |
|-----------|-------------|
| `toast/3` | Brief notification message |
| `snackbar/3` | Notification with action |
| `banner/3` | Persistent banner message |

### Content Components

| Predicate | Description |
|-----------|-------------|
| `card/3` | Container with elevation |
| `list_item/3` | Row in a list |
| `avatar/3` | User image/initials |
| `badge/3` | Status indicator |
| `chip/3` | Selectable tag |
| `tag/3` | Alias for chip |

### Layout Components

| Predicate | Description |
|-----------|-------------|
| `divider/2` | Horizontal/vertical line |
| `spacer/2` | Empty space |
| `skeleton/3` | Loading placeholder |

### Progress Components

| Predicate | Description |
|-----------|-------------|
| `progress_bar/3` | Linear progress |
| `progress_circle/3` | Circular progress |
| `spinner/2` | Loading spinner |

### Input Components

| Predicate | Description |
|-----------|-------------|
| `search_bar/2` | Search input |
| `rating/3` | Star rating |
| `stepper/3` | Numeric +/- control |
| `slider_input/3` | Range slider |

## Component Specifications

Each component predicate creates a specification term that can be passed to `generate_component/3`.

### Modal

```prolog
modal(Type, Options, Spec)
```

**Types:** `alert`, `confirm`, `custom`

**Options:**
- `title(String)` - Modal title
- `message(String)` - Modal body text
- `onClose(Handler)` - Close callback
- `dismissable(Bool)` - Can dismiss by tapping outside

**Example:**

```prolog
?- modal(confirm, [
       title('Delete Item'),
       message('Are you sure you want to delete this item?'),
       dismissable(true)
   ], Spec),
   generate_component(Spec, react_native, Code).
```

### Alert Dialog

```prolog
alert_dialog(Title, Options, Spec)
```

**Options:**
- `message(String)` - Alert message
- `confirmText(String)` - Button text (default: 'OK')
- `onConfirm(Handler)` - Confirm callback

**Example:**

```prolog
?- alert_dialog('Success', [
       message('Your changes have been saved.'),
       confirmText('Got it')
   ], Spec).
```

### Bottom Sheet

```prolog
bottom_sheet(Content, Options, Spec)
```

**Options:**
- `height(Value)` - Height (`auto`, pixels, or percentage)
- `dismissable(Bool)` - Can swipe to dismiss
- `snapPoints(List)` - Snap positions (e.g., `['25%', '50%', '90%']`)

### Action Sheet

```prolog
action_sheet(Actions, Options, Spec)
```

**Actions:** List of `action(Label, Handler)` terms

**Options:**
- `title(String)` - Sheet title
- `cancelText(String)` - Cancel button text

**Example:**

```prolog
?- action_sheet([
       action('Share', share_handler),
       action('Edit', edit_handler),
       action('Delete', delete_handler)
   ], [title('Options')], Spec).
```

### Toast

```prolog
toast(Message, Options, Spec)
```

**Options:**
- `type(Type)` - `info`, `success`, `warning`, `error`
- `duration(Ms)` - Display duration in milliseconds
- `position(Pos)` - `top`, `bottom`, `center`
- `action(Handler)` - Optional action

**Example:**

```prolog
?- toast('Item saved successfully', [
       type(success),
       duration(3000),
       position(bottom)
   ], Spec).
```

### Snackbar

```prolog
snackbar(Message, Options, Spec)
```

**Options:**
- `action(Handler)` - Action callback
- `actionText(String)` - Action button text
- `duration(Ms)` - Display duration

### Banner

```prolog
banner(Message, Options, Spec)
```

**Options:**
- `type(Type)` - `info`, `warning`, `error`
- `dismissable(Bool)` - Show close button
- `icon(IconName)` - Optional icon
- `actions(List)` - Action buttons

### Card

```prolog
card(Content, Options, Spec)
```

**Options:**
- `title(String)` - Card header title
- `subtitle(String)` - Card subtitle
- `image(URL)` - Header image
- `footer(Content)` - Footer content
- `elevated(Bool)` - Show shadow
- `onPress(Handler)` - Tap callback

**Example:**

```prolog
?- card('Card body content', [
       title('Product Name'),
       subtitle('$29.99'),
       image('/images/product.jpg'),
       elevated(true),
       onPress(view_product)
   ], Spec).
```

### List Item

```prolog
list_item(Content, Options, Spec)
```

**Options:**
- `leading(Component)` - Left element (icon, avatar)
- `trailing(Component)` - Right element (chevron, switch)
- `subtitle(String)` - Secondary text
- `onPress(Handler)` - Tap callback
- `divider(Bool)` - Show bottom divider

### Avatar

```prolog
avatar(Source, Options, Spec)
```

**Source:** Image URL or initials string

**Options:**
- `size(Size)` - `small`, `medium`, `large`, or pixels
- `fallback(String)` - Fallback initials
- `badge(BadgeSpec)` - Optional status badge
- `shape(Shape)` - `circle` or `square`

### Badge

```prolog
badge(Content, Options, Spec)
```

**Options:**
- `variant(Variant)` - `default`, `outlined`
- `color(Color)` - `primary`, `secondary`, `error`, etc.
- `size(Size)` - `small`, `medium`, `large`
- `dot(Bool)` - Show as dot only (no content)

### Chip

```prolog
chip(Label, Options, Spec)
```

**Options:**
- `variant(Variant)` - `filled`, `outlined`
- `color(Color)` - Theme color
- `icon(IconName)` - Leading icon
- `onDelete(Handler)` - Delete callback (shows X button)
- `selected(Bool)` - Selection state

### Progress Bar

```prolog
progress_bar(Value, Options, Spec)
```

**Options:**
- `max(Number)` - Maximum value (default: 100)
- `color(Color)` - Bar color
- `showLabel(Bool)` - Show percentage label
- `animated(Bool)` - Animate changes

### Progress Circle

```prolog
progress_circle(Value, Options, Spec)
```

**Options:**
- `max(Number)` - Maximum value
- `size(Pixels)` - Diameter
- `strokeWidth(Pixels)` - Ring thickness
- `color(Color)` - Ring color
- `showValue(Bool)` - Show value in center

### Spinner

```prolog
spinner(Options, Spec)
```

**Options:**
- `size(Size)` - `small`, `medium`, `large`
- `color(Color)` - Spinner color

### Search Bar

```prolog
search_bar(Options, Spec)
```

**Options:**
- `placeholder(String)` - Placeholder text
- `onSearch(Handler)` - Search callback
- `onClear(Handler)` - Clear callback
- `showCancel(Bool)` - Show cancel button
- `autoFocus(Bool)` - Focus on mount

### Rating

```prolog
rating(Value, Options, Spec)
```

**Options:**
- `max(Number)` - Maximum stars (default: 5)
- `allowHalf(Bool)` - Allow half-star ratings
- `readOnly(Bool)` - Disable interaction
- `size(Size)` - Star size
- `onChange(Handler)` - Change callback

### Stepper

```prolog
stepper(Value, Options, Spec)
```

**Options:**
- `min(Number)` - Minimum value
- `max(Number)` - Maximum value
- `step(Number)` - Increment amount
- `onChange(Handler)` - Change callback

### Slider

```prolog
slider_input(Value, Options, Spec)
```

**Options:**
- `min(Number)` - Minimum value
- `max(Number)` - Maximum value
- `step(Number)` - Step size
- `showValue(Bool)` - Display current value
- `onChange(Handler)` - Change callback

## Generating Code

### generate_component/3

```prolog
generate_component(+Spec, +Target, -Code)
```

Generate target-specific code from a component specification.

**Targets:** `react_native`, `vue`, `flutter`, `swiftui`

**Example:**

```prolog
?- card('Welcome to our app!', [
       title('Hello'),
       elevated(true)
   ], Spec),
   generate_component(Spec, vue, Code),
   writeln(Code).
```

### Target-Specific Generators

```prolog
generate_react_native_component(+Spec, -Code)
generate_vue_component(+Spec, -Code)
generate_flutter_component(+Spec, -Code)
generate_swiftui_component(+Spec, -Code)
```

## Component Registry

Register custom components for reuse:

```prolog
% Register a component
?- card('Product details', [title('Widget'), subtitle('$9.99')], Spec),
   register_component(product_card, Spec).

% Retrieve a component
?- get_component(product_card, Spec).

% List all registered components
?- list_components(Names).
```

## Complete Example

```prolog
:- use_module('src/unifyweaver/components/component_library').

% Define a user profile card
user_card(User, Spec) :-
    User = user(Name, Email, AvatarURL),
    avatar(AvatarURL, [size(large), shape(circle)], AvatarSpec),
    card(profile_content, [
        title(Name),
        subtitle(Email),
        leading(AvatarSpec),
        elevated(true),
        onPress(view_profile)
    ], Spec).

% Generate for all targets
generate_user_card :-
    user_card(user('John Doe', 'john@example.com', '/avatar.jpg'), Spec),
    generate_component(Spec, react_native, RNCode),
    generate_component(Spec, vue, VueCode),
    generate_component(Spec, flutter, FlutterCode),
    format('React Native:~n~w~n~n', [RNCode]),
    format('Vue:~n~w~n~n', [VueCode]),
    format('Flutter:~n~w~n', [FlutterCode]).
```

---

**Previous**: [Chapter 2: App Generation](02_app_generation.md) | **Next**: [Chapter 4: Layout System](04_layout_system.md)
