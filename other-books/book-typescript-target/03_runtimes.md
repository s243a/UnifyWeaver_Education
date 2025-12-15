# Chapter 3: Runtime Selection

Choose the right JavaScript runtime for your use case.

## The js_glue Module

```prolog
?- use_module('src/unifyweaver/glue/js_glue').
```

## Runtime Selection

```prolog
?- js_runtime_choice([typescript, secure], Runtime).
Runtime = deno.

?- js_runtime_choice([npm], Runtime).
Runtime = node.

?- js_runtime_choice([dom], Runtime).
Runtime = browser.

?- js_runtime_choice([fast, bundled], Runtime).
Runtime = bun.
```

## Runtime Features

### Node.js (Default)

```prolog
node_supports(fs).
node_supports(path).
node_supports(http).
node_supports(crypto).
node_supports(npm).
```

Best for: Server-side, npm ecosystem, existing projects.

### Deno

```prolog
deno_supports(typescript).      % Native TS support
deno_supports(permissions).     % Security sandbox
deno_supports('Deno.serve').    % HTTP server
```

Best for: TypeScript-first, security-conscious apps.

### Bun

```prolog
bun_supports(fast).             % Fast startup
bun_supports(npm_compat).       % npm compatibility
bun_supports('Bun.serve').      % Built-in server
```

Best for: Performance, drop-in Node replacement.

### Browser

```prolog
browser_supports(dom).
browser_supports(fetch).
browser_supports(localStorage).
browser_supports(document).
```

Best for: Web apps, React, Vue.

## Compatibility Checking

```prolog
?- can_use_runtime([fs, path, crypto], node).
true.

?- can_use_runtime([document, localStorage], browser).
true.

?- can_use_runtime([fs], browser).
false.  % fs not available in browsers
```

## Runtime Detection

```prolog
?- detect_node(Version).
Version = "v20.10.0".

?- detect_deno(Version).
Version = "deno 1.38.0".
```

## Pattern: Auto-Select Runtime

```prolog
% Choose runtime based on required features
auto_compile(Pred, Features, Code) :-
    js_runtime_choice(Features, Runtime),
    format('Using runtime: ~w~n', [Runtime]),
    compile_recursion(Pred, [runtime(Runtime)], Code).
```

---

**Previous**: [Chapter 2: Recursion Patterns](02_recursion.md) | [📖 Book: TypeScript Target](./)
