<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: App Generation - Questions

Q&A companion for multi-framework app generation.

## Question Index

1. [What is the API for generating complete projects?](#bgui02-q-api)
2. [What frontend frameworks are supported?](#bgui02-q-targets)
3. [How is navigation specified?](#bgui02-q-navigation)
4. [What is the Vue 3 project structure?](#bgui02-q-vue-structure)
5. [How is React Native navigation generated?](#bgui02-q-rn-navigation)
6. [What state management does Flutter use?](#bgui02-q-flutter-state)
7. [How does SwiftUI dependency injection work?](#bgui02-q-swiftui-di)
8. [How do I generate full-stack projects?](#bgui02-q-fullstack)
9. [What backend targets are supported?](#bgui02-q-backends)
10. [How are config files generated?](#bgui02-q-config)

---

<a id="bgui02-q-api"></a>
## Q1: What is the API for generating complete projects?

**Question:** What is the main predicate for generating app projects?

**Answer:** Use `generate_complete_project/4`:

```prolog
?- generate_complete_project(
       app(myapp, [
           navigation(tabs, [
               screen(home, 'HomeScreen', []),
               screen(profile, 'ProfileScreen', [])
           ], [])
       ]),
       [frontend-vue],
       '/tmp/myapp',
       Result
   ).
```

This generates a complete Vue project with tab navigation.

**See:** [02_app_generation_impl.md#generate_complete_project4](02_app_generation_impl.md#generate_complete_project4)

---

<a id="bgui02-q-targets"></a>
## Q2: What frontend frameworks are supported?

**Question:** Which frontend frameworks can be generated?

**Answer:** Four targets are supported:

| Target | Framework | Features |
|--------|-----------|----------|
| `vue` | Vue 3 + Vite | TypeScript, Pinia, Vue Router |
| `react_native` | React Native + Expo | TanStack Query, Navigation |
| `flutter` | Flutter + Dart | Riverpod, GoRouter |
| `swiftui` | SwiftUI | ObservableObject, EnvironmentObject |

**See:** [02_app_generation_impl.md#supported-frontend-targets](02_app_generation_impl.md#supported-frontend-targets)

---

<a id="bgui02-q-navigation"></a>
## Q3: How is navigation specified?

**Question:** How do I define app navigation in the spec?

**Answer:** Use the `navigation/3` term:

```prolog
navigation(tabs, [
    screen(home, 'HomeScreen', [icon(home), title('Home')]),
    screen(search, 'SearchScreen', [icon(search), title('Search')]),
    screen(profile, 'ProfileScreen', [icon(person), title('Profile')])
], [])
```

Navigation types:
- `tabs`: Bottom tab bar (mobile)
- `stack`: Push/pop navigation
- `drawer`: Sidebar navigation

**See:** [02_app_generation_impl.md#navigation-types](02_app_generation_impl.md#navigation-types)

---

<a id="bgui02-q-vue-structure"></a>
## Q4: What is the Vue 3 project structure?

**Question:** What files are generated for a Vue 3 project?

**Answer:** Standard Vite + Vue 3 structure:

```
frontend/
├── index.html
├── package.json
├── vite.config.ts
├── tsconfig.json
└── src/
    ├── main.ts          # Entry point
    ├── App.vue          # Root component
    ├── navigation/      # Vue Router setup
    ├── views/           # Screen components
    ├── styles/          # CSS/theme
    └── api/             # HTTP client
```

**See:** [02_app_generation_impl.md#vue-3--vite-generation](02_app_generation_impl.md#vue-3--vite-generation)

---

<a id="bgui02-q-rn-navigation"></a>
## Q5: How is React Native navigation generated?

**Question:** How does the generator set up React Navigation?

**Answer:** The entry point wraps the app with providers:

```tsx
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

TanStack Query is configured with staleTime and retry settings.

**See:** [02_app_generation_impl.md#react-native--expo-generation](02_app_generation_impl.md#react-native--expo-generation)

---

<a id="bgui02-q-flutter-state"></a>
## Q6: What state management does Flutter use?

**Question:** What state management pattern is used for Flutter?

**Answer:** Flutter uses Riverpod with `ConsumerWidget`:

```dart
class MyApp extends ConsumerWidget {
  @override
  Widget build(BuildContext context, WidgetRef ref) {
    final theme = ref.watch(themeProvider);
    final router = ref.watch(routerProvider);

    return MaterialApp.router(
      theme: theme,
      routerConfig: router,
    );
  }
}
```

Riverpod provides compile-time safe dependency injection.

**See:** [02_app_generation_impl.md#flutter--dart-generation](02_app_generation_impl.md#flutter--dart-generation)

---

<a id="bgui02-q-swiftui-di"></a>
## Q7: How does SwiftUI dependency injection work?

**Question:** How are dependencies injected in SwiftUI?

**Answer:** Using `@StateObject` and `@EnvironmentObject`:

```swift
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

Child views access via `@EnvironmentObject`.

**See:** [02_app_generation_impl.md#swiftui-generation](02_app_generation_impl.md#swiftui-generation)

---

<a id="bgui02-q-fullstack"></a>
## Q8: How do I generate full-stack projects?

**Question:** How do I generate both frontend and backend together?

**Answer:** Use `generate_full_stack_project/5`:

```prolog
?- generate_full_stack_project(
       app(taskapp, Patterns, Options),
       vue,        % Frontend target
       fastapi,    % Backend target
       '/tmp/taskapp',
       Result
   ).
```

This creates both `frontend/` and `backend/` directories.

**See:** [02_app_generation_impl.md#full-stack-generation](02_app_generation_impl.md#full-stack-generation)

---

<a id="bgui02-q-backends"></a>
## Q9: What backend targets are supported?

**Question:** Which backend frameworks can be generated?

**Answer:** Two Python backends:

| Target | Framework | Features |
|--------|-----------|----------|
| `fastapi` | Python FastAPI | Async, automatic OpenAPI docs |
| `flask` | Python Flask | Simple, flexible routing |

Output includes `requirements.txt`, `main.py`, and `routers/`.

**See:** [02_app_generation_impl.md#backend-targets](02_app_generation_impl.md#backend-targets)

---

<a id="bgui02-q-config"></a>
## Q10: How are config files generated?

**Question:** How does the generator create package.json and pubspec.yaml?

**Answer:** Dedicated predicates for each config type:

```prolog
% JavaScript/TypeScript projects
generate_package_json(+AppName, +Target, -JSON)

% Flutter projects
generate_pubspec_yaml(+AppName, -YAML)

% Python backends
generate_requirements_txt(+Backend, -Content)
```

Dependencies are target-specific (Vue Router for Vue, GoRouter for Flutter, etc.).

**See:** [02_app_generation_impl.md#config-file-generation](02_app_generation_impl.md#config-file-generation)
