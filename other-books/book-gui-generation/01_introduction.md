<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 1: Introduction

## Overview

UnifyWeaver's GUI generation system enables you to define complete frontend applications declaratively in Prolog and generate production-ready code for multiple target frameworks. Instead of writing framework-specific code by hand, you describe your application's structure, navigation, components, and styling—then generate code for Vue, React Native, Flutter, or SwiftUI.

## Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    App Specification (Prolog)               │
│  app(myapp, [navigation(...), theme(...), screens(...)])    │
└─────────────────────┬───────────────────────────────────────┘
                      │ generate_complete_project/4
                      ▼
┌─────────────────────────────────────────────────────────────┐
│                    App Generator                            │
│  Coordinates: UI patterns, theming, i18n, layout,          │
│               data binding, accessibility                   │
└─────────────────────┬───────────────────────────────────────┘
                      │
        ┌─────────────┼─────────────┬─────────────┐
        ▼             ▼             ▼             ▼
   ┌─────────┐   ┌─────────┐   ┌─────────┐   ┌─────────┐
   │   Vue   │   │ React   │   │ Flutter │   │ SwiftUI │
   │ + Vite  │   │ Native  │   │  + Dart │   │  + iOS  │
   └─────────┘   └─────────┘   └─────────┘   └─────────┘
```

## Supported Targets

| Target | Framework | Platform | Entry Point |
|--------|-----------|----------|-------------|
| `vue` | Vue 3 + Vite | Web | `main.ts` |
| `react_native` | React Native + Expo | iOS/Android | `App.tsx` |
| `flutter` | Flutter + Dart | iOS/Android/Web | `main.dart` |
| `swiftui` | SwiftUI | iOS/macOS | `App.swift` |

## Core Modules

The GUI generation system integrates several UnifyWeaver modules:

| Module | Purpose | Location |
|--------|---------|----------|
| `app_generator` | Main orchestration | `src/unifyweaver/glue/app_generator.pl` |
| `project_generator` | Directory structure, config files | `src/unifyweaver/glue/project_generator.pl` |
| `component_library` | Pre-built UI components | `src/unifyweaver/components/component_library.pl` |
| `layout_generator` | CSS Grid, Flexbox | `src/unifyweaver/glue/layout_generator.pl` |
| `data_binding_generator` | Reactive state | `src/unifyweaver/glue/data_binding_generator.pl` |
| `accessibility_generator` | ARIA, keyboard nav | `src/unifyweaver/glue/accessibility_generator.pl` |
| `responsive_generator` | Breakpoints, media queries | `src/unifyweaver/glue/responsive_generator.pl` |
| `theme_generator` | Colors, typography | `src/unifyweaver/glue/theme_generator.pl` |

## App Specification Format

An app specification is a Prolog term describing the entire application:

```prolog
app(AppName, [
    % Navigation structure
    navigation(Type, Screens, Options),

    % Screen definitions
    screens([
        screen(name, Config),
        ...
    ]),

    % Theme configuration
    theme(ThemeName, ThemeOptions),

    % Localization
    locales([en, es, fr]),
    translations([...]),

    % Data sources
    data([
        source(name, Config),
        ...
    ])
]).
```

## Quick Example

Define a simple two-screen app:

```prolog
:- use_module('src/unifyweaver/glue/app_generator').

my_app(App) :-
    App = app(taskmanager, [
        navigation(tabs, [
            screen(tasks, 'TasksScreen', [icon(list)]),
            screen(settings, 'SettingsScreen', [icon(cog)])
        ], []),
        theme(default, [
            colors([
                primary('#3b82f6'),
                secondary('#64748b'),
                background('#ffffff')
            ])
        ])
    ]).

% Generate Vue project
generate_vue :-
    my_app(App),
    generate_complete_project(App, [frontend-vue], '/tmp/taskmanager', Result),
    format('Generated: ~w~n', [Result]).

% Generate React Native project
generate_rn :-
    my_app(App),
    generate_complete_project(App, [frontend-react_native], '/tmp/taskmanager', Result),
    format('Generated: ~w~n', [Result]).
```

## Generated Output

For a Vue target, the generator produces:

```
/tmp/taskmanager/frontend/
├── index.html
├── package.json
├── vite.config.ts
├── tsconfig.json
├── tsconfig.node.json
└── src/
    ├── main.ts
    ├── App.vue
    ├── navigation/
    │   └── index.ts
    ├── views/
    │   ├── TasksScreen.vue
    │   └── SettingsScreen.vue
    ├── styles/
    │   └── theme.css
    └── api/
        └── client.ts
```

## Running Generated Projects

### Vue

```bash
cd /tmp/taskmanager/frontend
npm install
npm run dev
# Open http://localhost:5173
```

### React Native

```bash
cd /tmp/taskmanager/frontend
npm install
npx expo start
```

### Flutter

```bash
cd /tmp/taskmanager/frontend
flutter pub get
flutter run
```

## What's Next

The following chapters cover each aspect of GUI generation in detail:

- **Chapter 2**: Full project scaffolding and target-specific configuration
- **Chapter 3**: Using pre-built components from the component library
- **Chapter 4**: Declarative layouts with Grid and Flexbox
- **Chapter 5**: Binding Prolog data to reactive UI state
- **Chapter 6**: Building accessible applications
- **Chapter 7**: Responsive design for multiple screen sizes
- **Chapter 8**: Theming and visual customization

---

**Next**: [Chapter 2: App Generation](02_app_generation.md)
