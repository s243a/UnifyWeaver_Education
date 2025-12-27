<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 20: Pyodide and Python Variant Targets

This chapter covers UnifyWeaver's Python variant targets - specialized compilation backends that optimize Python code for different use cases, from browser-based execution to high-performance numerical computing.

## Learning Objectives

By the end of this chapter, you will be able to:

- Choose the right Python variant for your use case
- Generate Pyodide code for secure browser-based computation
- Use Numba for JIT-compiled numerical operations
- Create Cython extensions for C-level performance
- Package applications with Nuitka
- Understand the security benefits of client-side computation

---

## 20.1 Overview of Python Variants

UnifyWeaver supports seven Python compilation variants:

| Variant | License | NumPy | Execution | Best For |
|---------|---------|-------|-----------|----------|
| **python** | - | ✅ | Interpreted | General purpose |
| **numba** | BSD 2-Clause | ✅ | JIT → LLVM | Numerical loops |
| **cython** | Apache 2.0 | ✅ | AOT → C | C extensions |
| **nuitka** | Apache 2.0 | ✅ | AOT → C | Deployment |
| **codon** | BSL 1.1* | ⚠️ | AOT → LLVM | Maximum speed |
| **mypyc** | MIT | ✅ | AOT → C | Type-checked code |
| **pyodide** | MPL 2.0 | ✅ | WASM | Browser/security |

*Codon has commercial restrictions.

### Inheritance Architecture

All variants inherit from the base `python_target`:

```prolog
% Each variant loads the base target
:- use_module(python_target).

init_numba_target :-
    init_python_target.  % Shared bindings and components
```

This means:
- **Bindings** defined for Python work in all variants
- **Components** registered for Python are available everywhere
- Variants add **decorators**, **type annotations**, and **build tooling**

---

## 20.2 Pyodide: Secure Browser-Based Python

Pyodide compiles CPython to WebAssembly, enabling Python (including NumPy, SciPy, pandas) to run entirely in the browser.

### Why Pyodide?

**Security Benefits:**

| Risk | Server-side Python | Pyodide |
|------|-------------------|---------|
| Code injection | ⚠️ Possible with eval() | ✅ Impossible |
| Data exfiltration | ⚠️ Server has access | ✅ Sandboxed |
| Filesystem access | ⚠️ Full access | ✅ None |
| Network access | ⚠️ Full access | ✅ Browser restricted |

**No Server Required:**
- Users don't need to trust your server
- Works offline after initial load
- No backend infrastructure to maintain

### Basic Usage

```prolog
?- use_module('src/unifyweaver/targets/python_pyodide_target').
?- init_pyodide_target.

% Generate a Pyodide-compatible function
?- compile_pyodide_function(matrix_inverse/1, [
       packages([numpy])
   ], Code).

% Generate complete HTML page with Chart.js
?- generate_pyodide_html('Matrix Calculator', [
       packages([numpy]),
       chart(true)
   ], HTML).
```

### Generated JavaScript Wrapper

```javascript
class PyodideRunner {
    async init() {
        this.pyodide = await loadPyodide();
        await this.pyodide.loadPackage(['numpy']);
    }

    async numpy(operation) {
        return await this.runPython(`
            import numpy as np
            result = ${operation}
            result.tolist()
        `);
    }
}
```

### Matrix Visualization Example

See `examples/pyodide-matrix/` for a complete demo:

```bash
cd examples/pyodide-matrix
firefox index.html  # No server needed!
```

Features:
- Matrix inverse, eigenvalues, SVD
- Visual transformation of unit circle
- Eigenvector visualization
- Linear regression

---

## 20.3 Numba: JIT-Compiled Numerical Python

Numba compiles Python functions to machine code at runtime using LLVM.

### When to Use Numba

- **Numerical loops** that are too slow in pure Python
- **NumPy operations** that need extra speed
- **Parallel processing** with automatic parallelization

### Basic Usage

```prolog
?- use_module('src/unifyweaver/targets/python_numba_target').
?- init_numba_target.

% JIT-compiled function with caching
?- compile_numba_function(dot_product/2, [
       nopython(true),
       cache(true),
       parallel(true)
   ], Code).
```

### Generated Code

```python
from numba import njit, prange
import numpy as np

@njit(parallel=True, cache=True)
def dot_product(a, b):
    result = 0.0
    for i in prange(len(a)):
        result += a[i] * b[i]
    return result
```

### Decorator Options

| Option | Values | Effect |
|--------|--------|--------|
| `nopython` | true/false | Use @njit (faster, no Python objects) |
| `cache` | true/false | Cache compiled code to disk |
| `parallel` | true/false | Enable automatic parallelization |
| `fastmath` | true/false | Relax IEEE 754 for speed |

---

## 20.4 Cython: C Extension Modules

Cython compiles Python with optional static typing to C extension modules.

### When to Use Cython

- **C library integration** (wrapping C/C++ code)
- **Maximum performance** with explicit type declarations
- **GIL release** for multi-threaded code

### Basic Usage

```prolog
?- use_module('src/unifyweaver/targets/python_cython_target').
?- init_cython_target.

% Typed function with C-level performance
?- compile_cython_function(fast_sum/1, [
       mode(cpdef),
       types([list(float)]),
       return_type(float),
       nogil(true)
   ], Code).
```

### Generated Code

```cython
# cython: language_level=3
# cython: boundscheck=False
cimport cython

cpdef double fast_sum(double[:] arr) nogil:
    cdef double total = 0.0
    cdef Py_ssize_t i
    for i in range(arr.shape[0]):
        total += arr[i]
    return total
```

### Build Pipeline

```bash
# Generate .pyx file
swipl -g "generate_code" -t halt module.pl > module.pyx

# Compile with setup.py
python setup.py build_ext --inplace
```

---

## 20.5 Nuitka: Standalone Executables

Nuitka compiles entire Python programs to C for distribution.

### When to Use Nuitka

- **Distributing applications** without requiring Python installation
- **Hiding source code** (compiled to machine code)
- **Web applications** with Flask/FastAPI

### Basic Usage

```prolog
?- use_module('src/unifyweaver/targets/python_nuitka_target').

% Generate web application
?- compile_nuitka_webapp(my_api, [
       framework(flask),
       routes([
           route('/', get, index),
           route('/api/data', post, handle_data)
       ])
   ], Code).

% Generate build script
?- generate_nuitka_build_script(my_app, [
       standalone(true),
       onefile(true)
   ], Script).
```

### Build Commands

```bash
# Single executable
nuitka --onefile my_app.py

# With Flask plugin
nuitka --standalone --enable-plugin=flask my_app.py

# Cross-platform Windows build
nuitka --onefile --windows-disable-console my_app.py
```

---

## 20.6 Comparing Variants

### Decision Tree

```
Start
  │
  ├─ Runs in browser? ───────────────────► Pyodide
  │
  ├─ Numerical/scientific computing?
  │     │
  │     ├─ Needs classes? ───────────────► Cython
  │     │
  │     └─ Just functions/loops? ────────► Numba
  │
  ├─ Distributing executable?
  │     │
  │     └─ Need plugins (Flask, Qt)? ────► Nuitka
  │
  ├─ Using mypy for type checking? ──────► mypyc
  │
  └─ Maximum raw performance?
        │
        └─ Limited Python subset OK? ────► Codon
```

### Performance Comparison

| Workload | Python | Numba | Cython | Codon |
|----------|--------|-------|--------|-------|
| Pure Python loop | 1x | 50-100x | 20-50x | 100x |
| NumPy operations | 1x | 2-5x | 2-5x | 5-10x |
| C library calls | 1x | N/A | Native | N/A |

### Startup Time

| Variant | First Run | Subsequent |
|---------|-----------|------------|
| Python | Instant | Instant |
| Numba | 100-500ms | 1ms (cached) |
| Cython | Instant | Instant |
| Nuitka | Instant | Instant |
| Pyodide | 5-10s | 100ms |

---

## 20.7 Custom Components

Each variant has a corresponding component type:

```prolog
% Numba component
declare_component(source, fast_sum, custom_numba, [
    code("    return np.sum(input)"),
    decorator(njit),
    parallel(true)
]).

% Cython component
declare_component(source, c_multiply, custom_cython, [
    code("    return arg0 * arg1"),
    mode(cpdef),
    types([double, double]),
    return_type(double),
    nogil(true)
]).

% Pyodide component (browser-safe)
declare_component(source, matrix_inverse, custom_pyodide, [
    code("    return np.linalg.inv(input)"),
    packages([numpy]),
    js_wrapper(true)
]).
```

---

## 20.8 Exercises

### Exercise 1: Pyodide Matrix Calculator

Create a Pyodide-based calculator that computes:
- Matrix determinant
- Matrix rank
- Condition number

```prolog
% Your code here
compile_pyodide_module(matrix_analysis, [
    packages([numpy]),
    exports([determinant, rank, condition_number])
], Code).
```

### Exercise 2: Numba Performance Benchmark

Compare Python vs Numba for computing the Mandelbrot set:

```prolog
% Generate both versions
compile_predicate_to_python(mandelbrot/4, [], PythonCode).
compile_numba_function(mandelbrot/4, [parallel(true)], NumbaCode).
```

### Exercise 3: Hybrid Pipeline

Create a pipeline that uses:
- Pyodide for browser UI
- Numba for server-side computation
- HTTP communication between them

---

## 20.9 Summary

| Concept | Key Point |
|---------|-----------|
| **Pyodide** | Python in browser via WASM - secure, no server |
| **Numba** | JIT compilation for numerical loops |
| **Cython** | C extensions with static typing |
| **Nuitka** | Compile to standalone executables |
| **Inheritance** | All variants share bindings and components |
| **Security** | Pyodide eliminates server-side injection risks |

---

## Next Steps

- [Chapter 19: WASM Visualization](19_wasm_visualization.md) - LLVM→WASM pipeline
- [API Reference](13_api_reference.md) - Complete predicate documentation
- [docs/PYTHON_VARIANTS.md](../../docs/PYTHON_VARIANTS.md) - Full variant reference

---

*[Previous: Chapter 19 - WASM Visualization](19_wasm_visualization.md)*
