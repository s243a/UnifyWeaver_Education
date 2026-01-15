<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 2: App Generation

## Overview

The app generator produces complete, runnable projects for multiple frontend frameworks from a single Prolog specification. This chapter covers the generation process, target-specific details, and full-stack project scaffolding.

## Core Predicates

### generate_complete_project/4

The main entry point for project generation:

```prolog
generate_complete_project(+AppSpec, +Targets, +OutputDir, -Result)
```

**Parameters:**
- `AppSpec`: The app specification term
- `Targets`: List of targets like `[frontend-vue, backend-fastapi]`
- `OutputDir`: Base output directory
- `Result`: Unified with generation result

**Example:**

```prolog
?- generate_complete_project(
       app(myapp, [
           navigation(tabs, [
               screen(home, 'HomeScreen', []),
               screen(profile, 'ProfileScreen', [])
           ], [])
       ]),
       [frontend-vue, backend-fastapi],
       '/tmp/myapp',
       Result
   ).
```

### generate_frontend_project/4

Generate only the frontend portion:

```prolog
generate_frontend_project(+AppSpec, +Target, +OutputDir, -Files)
```

**Supported Targets:**
- `vue` - Vue 3 with Vite and TypeScript
- `react_native` - React Native with Expo
- `flutter` - Flutter with Dart
- `swiftui` - SwiftUI for iOS/macOS

## Target-Specific Generation

### Vue 3 + Vite

The Vue target generates a modern Vue 3 project with:

- **Vite** for fast development and building
- **TypeScript** for type safety
- **Pinia** for state management
- **Vue Router** for navigation
- **TanStack Query** for data fetching

**Entry Point (main.ts):**

```typescript
import { createApp } from 'vue';
import { createPinia } from 'pinia';
import { VueQueryPlugin } from '@tanstack/vue-query';
import { router } from './navigation';
import App from './App.vue';
import './styles/theme.css';

const app = createApp(App);

app.use(createPinia());
app.use(router);
app.use(VueQueryPlugin);

app.mount('#app');
```

**Directory Structure:**

```
frontend/
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
    │   └── *.vue
    ├── styles/
    │   └── theme.css
    └── api/
        └── client.ts
```

### React Native + Expo

The React Native target generates an Expo-based project with:

- **Expo** for cross-platform development
- **React Navigation** for navigation
- **TanStack Query** for data fetching
- **Theme Context** for styling

**Entry Point (App.tsx):**

```tsx
import React from 'react';
import { NavigationContainer } from '@react-navigation/native';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { ThemeProvider } from './theme';
import { AppNavigator } from './navigation';

const queryClient = new QueryClient({
  defaultOptions: {
    queries: {
      staleTime: 5 * 60 * 1000,
      retry: 2,
    },
  },
});

export default function App() {
  return (
    <QueryClientProvider client={queryClient}>
      <ThemeProvider>
        <NavigationContainer>
          <AppNavigator />
        </NavigationContainer>
      </ThemeProvider>
    </QueryClientProvider>
  );
}
```

**Directory Structure:**

```
frontend/
├── app.json
├── package.json
├── App.tsx
├── src/
│   ├── navigation/
│   │   └── index.tsx
│   ├── screens/
│   │   └── *.tsx
│   ├── theme/
│   │   └── index.tsx
│   └── api/
│       └── client.ts
```

### Flutter + Dart

The Flutter target generates a Dart project with:

- **Riverpod** for state management
- **GoRouter** for navigation
- **Material Design** theming

**Entry Point (main.dart):**

```dart
import 'package:flutter/material.dart';
import 'package:flutter_riverpod/flutter_riverpod.dart';
import 'package:go_router/go_router.dart';
import 'theme/app_theme.dart';
import 'router/app_router.dart';

void main() {
  runApp(
    const ProviderScope(
      child: MyApp(),
    ),
  );
}

class MyApp extends ConsumerWidget {
  const MyApp({super.key});

  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final theme = ref.watch(themeProvider);
    final router = ref.watch(routerProvider);

    return MaterialApp.router(
      title: 'MyApp',
      theme: theme,
      routerConfig: router,
      debugShowCheckedModeBanner: false,
    );
  }
}
```

**Directory Structure:**

```
frontend/
├── pubspec.yaml
├── lib/
│   ├── main.dart
│   ├── theme/
│   │   └── app_theme.dart
│   ├── router/
│   │   └── app_router.dart
│   └── screens/
│       └── *.dart
```

### SwiftUI

The SwiftUI target generates an iOS/macOS project with:

- **SwiftUI** declarative views
- **ObservableObject** for state
- **EnvironmentObject** for dependency injection

**Entry Point (App.swift):**

```swift
import SwiftUI

@main
struct MyAppApp: App {
    @StateObject private var appStore = AppStore()
    @StateObject private var themeManager = ThemeManager()

    var body: some Scene {
        WindowGroup {
            ContentView()
                .environmentObject(appStore)
                .environmentObject(themeManager)
        }
    }
}
```

**Directory Structure:**

```
frontend/
├── MyApp.xcodeproj/
├── Sources/
│   ├── MyAppApp.swift
│   ├── ContentView.swift
│   ├── Theme/
│   │   └── ThemeManager.swift
│   └── Views/
│       └── *.swift
```

## Navigation Generation

Navigation is generated based on the `navigation/3` term in the app spec:

```prolog
navigation(Type, Screens, Options)
```

**Types:**
- `tabs` - Tab-based navigation (bottom tabs on mobile)
- `stack` - Stack-based navigation (push/pop)
- `drawer` - Drawer/sidebar navigation

**Screen Definition:**

```prolog
screen(Name, ComponentName, Options)
```

**Options:**
- `icon(IconName)` - Icon for tab navigation
- `title(Title)` - Screen title
- `initial(true)` - Mark as initial screen

**Example:**

```prolog
navigation(tabs, [
    screen(home, 'HomeScreen', [icon(home), title('Home')]),
    screen(search, 'SearchScreen', [icon(search), title('Search')]),
    screen(profile, 'ProfileScreen', [icon(person), title('Profile')])
], [])
```

**Generated Vue Router:**

```typescript
import { createRouter, createWebHistory } from 'vue-router';

const routes = [
  { path: '/', redirect: '/home' },
  { path: '/home', component: () => import('../views/HomeScreen.vue') },
  { path: '/search', component: () => import('../views/SearchScreen.vue') },
  { path: '/profile', component: () => import('../views/ProfileScreen.vue') },
];

export const router = createRouter({
  history: createWebHistory(),
  routes,
});
```

## Full-Stack Generation

Generate both frontend and backend together:

```prolog
generate_full_stack_project(+AppSpec, +FrontendTarget, +BackendTarget, +OutputDir, -Result)
```

**Backend Targets:**
- `fastapi` - Python FastAPI
- `flask` - Python Flask

**Example:**

```prolog
?- generate_full_stack_project(
       app(taskapp, Patterns, Options),
       vue,        % Frontend
       fastapi,    % Backend
       '/tmp/taskapp',
       Result
   ).
```

**Output Structure:**

```
/tmp/taskapp/
├── frontend/          # Vue project
│   ├── package.json
│   └── src/
└── backend/           # FastAPI project
    ├── requirements.txt
    ├── main.py
    └── routers/
```

## Config File Generation

### package.json (Vue/React Native)

```prolog
generate_package_json(+AppName, +Target, -JSON)
```

Generated with appropriate dependencies for each target.

### pubspec.yaml (Flutter)

```prolog
generate_pubspec_yaml(+AppName, -YAML)
```

Includes Flutter dependencies like Riverpod, GoRouter, etc.

### requirements.txt (Python backends)

```prolog
generate_requirements_txt(+Backend, -Content)
```

## Complete Example

```prolog
:- use_module('src/unifyweaver/glue/app_generator').

% Define a complete app specification
shopping_app(App) :-
    App = app(shopify_clone, [
        navigation(tabs, [
            screen(home, 'HomeScreen', [icon(home)]),
            screen(cart, 'CartScreen', [icon(cart)]),
            screen(orders, 'OrdersScreen', [icon(list)]),
            screen(account, 'AccountScreen', [icon(person)])
        ], []),

        theme(default, [
            colors([
                primary('#10B981'),
                secondary('#3B82F6'),
                background('#F9FAFB')
            ]),
            typography([
                fontFamily('Inter')
            ])
        ]),

        data([
            source(products, [endpoint('/api/products')]),
            source(cart, [endpoint('/api/cart')])
        ])
    ]).

% Generate for different targets
generate_all :-
    shopping_app(App),
    generate_complete_project(App, [frontend-vue], '/tmp/shop-vue', _),
    generate_complete_project(App, [frontend-react_native], '/tmp/shop-rn', _),
    generate_complete_project(App, [frontend-flutter], '/tmp/shop-flutter', _).
```

---

**Previous**: [Chapter 1: Introduction](01_introduction.md) | **Next**: [Chapter 3: Component Library](03_component_library.md)
