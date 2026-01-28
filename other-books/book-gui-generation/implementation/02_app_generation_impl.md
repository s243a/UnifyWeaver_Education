<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: App Generation - Implementation Details

Technical deep-dive for multi-framework app generation from Prolog specifications.

## generate_complete_project/4

### Signature

```prolog
generate_complete_project(+AppSpec, +Targets, +OutputDir, -Result)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `AppSpec` | `term` | App specification (navigation, theme, data) |
| `Targets` | `list` | Target frameworks (e.g., `[frontend-vue]`) |
| `OutputDir` | `string` | Base output directory |
| `Result` | `term` | Generation result |

### Supported Frontend Targets

| Target | Framework | Features |
|--------|-----------|----------|
| `vue` | Vue 3 + Vite | TypeScript, Pinia, Vue Router |
| `react_native` | React Native + Expo | TanStack Query, Navigation |
| `flutter` | Flutter + Dart | Riverpod, GoRouter |
| `swiftui` | SwiftUI | ObservableObject, EnvironmentObject |

### Algorithm

1. **Parse AppSpec**: Extract navigation, theme, and data sources
2. **Select Generator**: Route to target-specific generator
3. **Generate Structure**: Create directory layout
4. **Generate Files**: Emit configuration, entry points, components
5. **Return Result**: File list and status

## App Specification Format

### Structure

```prolog
app(Name, [
    navigation(Type, Screens, Options),
    theme(Name, Properties),
    data(Sources)
])
```

### Navigation Types

| Type | Description | Example |
|------|-------------|---------|
| `tabs` | Tab-based (bottom tabs) | Mobile apps |
| `stack` | Push/pop navigation | Detail views |
| `drawer` | Sidebar navigation | Desktop apps |

### Screen Definition

```prolog
screen(Name, ComponentName, Options)
```

Options:
- `icon(IconName)`: Tab icon
- `title(Title)`: Screen title
- `initial(true)`: Mark as initial screen

## Vue 3 + Vite Generation

### Entry Point (main.ts)

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

### Directory Structure

```
frontend/
├── index.html
├── package.json
├── vite.config.ts
├── tsconfig.json
└── src/
    ├── main.ts
    ├── App.vue
    ├── navigation/index.ts
    ├── views/*.vue
    ├── styles/theme.css
    └── api/client.ts
```

### Router Generation

```typescript
import { createRouter, createWebHistory } from 'vue-router';

const routes = [
  { path: '/', redirect: '/home' },
  { path: '/home', component: () => import('../views/HomeScreen.vue') },
  { path: '/search', component: () => import('../views/SearchScreen.vue') },
];

export const router = createRouter({
  history: createWebHistory(),
  routes,
});
```

## React Native + Expo Generation

### Entry Point (App.tsx)

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

### Directory Structure

```
frontend/
├── app.json
├── package.json
├── App.tsx
└── src/
    ├── navigation/index.tsx
    ├── screens/*.tsx
    ├── theme/index.tsx
    └── api/client.ts
```

## Flutter + Dart Generation

### Entry Point (main.dart)

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

### Directory Structure

```
frontend/
├── pubspec.yaml
└── lib/
    ├── main.dart
    ├── theme/app_theme.dart
    ├── router/app_router.dart
    └── screens/*.dart
```

## SwiftUI Generation

### Entry Point (App.swift)

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

### Directory Structure

```
frontend/
├── MyApp.xcodeproj/
└── Sources/
    ├── MyAppApp.swift
    ├── ContentView.swift
    ├── Theme/ThemeManager.swift
    └── Views/*.swift
```

## Full-Stack Generation

### generate_full_stack_project/5

```prolog
generate_full_stack_project(+AppSpec, +FrontendTarget, +BackendTarget, +OutputDir, -Result)
```

### Backend Targets

| Target | Framework | Features |
|--------|-----------|----------|
| `fastapi` | Python FastAPI | Async, OpenAPI |
| `flask` | Python Flask | Simple, flexible |

### Output Structure

```
/tmp/myapp/
├── frontend/          # Vue/React/Flutter project
│   ├── package.json
│   └── src/
└── backend/           # FastAPI/Flask project
    ├── requirements.txt
    ├── main.py
    └── routers/
```

## Config File Generation

### generate_package_json/3

```prolog
generate_package_json(+AppName, +Target, -JSON)
```

Generates target-specific dependencies:
- Vue: vue, pinia, vue-router, @tanstack/vue-query
- React Native: react-native, @react-navigation/native, @tanstack/react-query

### generate_pubspec_yaml/2

```prolog
generate_pubspec_yaml(+AppName, -YAML)
```

Flutter dependencies:
- flutter_riverpod
- go_router
- dio (HTTP client)

## Source Files

- `src/unifyweaver/glue/app_generator.pl`
