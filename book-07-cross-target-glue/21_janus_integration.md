<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 21: Janus In-Process Integration

In-process Python↔Prolog communication using SWI-Prolog's Janus library.

> **Note:** Janus is **SWI-Prolog specific**. It is not available in other Prolog implementations (GNU Prolog, XSB, etc.). If you need cross-Prolog compatibility, use the pipe transport instead.

## Overview

Janus provides **direct in-process** communication between Prolog and Python:

```
┌─────────────────────────────────────────────┐
│ SWI-Prolog Process                          │
│ ┌─────────────────┐  ┌───────────────────┐  │
│ │ Prolog Code     │←→│ Embedded Python   │  │
│ │                 │  │ (NumPy, etc.)     │  │
│ │ - UnifyWeaver   │  │                   │  │
│ │ - janus_glue.pl │  │ - Shared memory   │  │
│ │ - Your code     │  │ - Zero-copy arrays│  │
│ └─────────────────┘  └───────────────────┘  │
└─────────────────────────────────────────────┘
```

### Key Benefits

- **Zero serialization overhead** for compatible types
- **Direct function calls** without process spawning
- **Shared memory** for NumPy arrays
- **Bidirectional calling** (Prolog→Python and Python→Prolog)

---

## Transport Comparison

| Transport | Overhead | Use Case |
|-----------|----------|----------|
| **Janus** | Minimal (in-process) | Tight integration, NumPy, ML |
| **Pipe** | Medium (serialization) | Process isolation, streaming |
| **HTTP** | High (network) | Distributed, microservices |

### Performance

For 1000 repeated calls:

| Operation | Janus | Pipe |
|-----------|-------|------|
| Simple function | ~0.1s | ~10s |
| NumPy array (1000 elements) | ~0.2s | ~15s |
| Large matrix (1M elements) | ~0.5s | ~60s+ |

**Janus is 50-100x faster** because it avoids:
- Process spawning
- Serialization/deserialization
- Inter-process communication

---

## Requirements

- **SWI-Prolog 9.0+** with Janus support (Janus is SWI-Prolog specific)
- **Python 3.8+**
- **janus-swi** Python package (for bidirectional calling)

> **Prolog Compatibility:** Janus only works with SWI-Prolog. For other Prolog systems, use the standard pipe transport which works across all implementations.

```bash
# Install Python package
pip install janus-swi

# Optional: NumPy for numerical examples
pip install numpy
```

---

## Quick Start

### Basic Python Calls

```prolog
:- use_module(library(janus)).

% Call math.sqrt directly
?- py_call(math:sqrt(16), Result).
Result = 4.0.

% Call json.dumps
?- py_call(json:dumps([1, 2, 3]), JsonStr).
JsonStr = "[1, 2, 3]".

% Import and use module
?- py_call(importlib:import_module(json), Json),
   py_call(Json:dumps([a, b, c]), S).
S = "[\"a\", \"b\", \"c\"]".
```

### Using the Glue Module

```prolog
:- use_module('src/unifyweaver/glue/janus_glue').

% Higher-level interface
?- janus_call_python(math, sqrt, [16], Result).
Result = 4.0.

% Create objects
?- janus_create_object(datetime, datetime, [2025, 12, 26], DT),
   janus_call_method(DT, isoformat, [], IsoStr).
IsoStr = "2025-12-26T00:00:00".
```

---

## NumPy Integration

### Creating Arrays

```prolog
:- use_module('src/unifyweaver/glue/janus_glue').

% Check NumPy availability
?- janus_numpy_available.
true.

% Create NumPy array from Prolog list
?- janus_numpy_array([1, 2, 3, 4, 5], Arr).
Arr = <py_object 0x...>.

% Call NumPy functions
?- janus_numpy_array([1, 2, 3, 4, 5], Arr),
   janus_numpy_call(mean, [Arr], Mean).
Mean = 3.0.
```

### Matrix Operations

```prolog
% Matrix multiplication
?- janus_numpy_call(array, [[[1, 2], [3, 4]]], MatA),
   janus_numpy_call(array, [[[5, 6], [7, 8]]], MatB),
   py_call(MatA:'__matmul__'(MatB), MatC),
   py_call(MatC:tolist(), Result).
Result = [[19, 22], [43, 50]].

% Linear algebra
?- janus_numpy_call(linalg:inv, [[[1, 2], [3, 4]]], Inv),
   py_call(Inv:tolist(), InvList).
InvList = [[-2.0, 1.0], [1.5, -0.5]].
```

### Statistical Functions

```prolog
?- janus_numpy_array([1, 2, 3, 4, 5, 6, 7, 8, 9, 10], Data),
   janus_numpy_call(mean, [Data], Mean),
   janus_numpy_call(std, [Data], Std).
Mean = 5.5,
Std = 2.8722813232690143.
```

---

## Custom Python Modules

### Creating Modules Dynamically

```prolog
% Define Python functions in-process
create_module :-
    py_call(builtins:exec("
def factorial(n):
    if n <= 1:
        return 1
    return n * factorial(n - 1)

def fibonacci(n):
    if n <= 1:
        return n
    a, b = 0, 1
    for _ in range(n - 1):
        a, b = b, a + b
    return b
"), _).

% Use the functions
?- create_module,
   py_call(factorial(10), Fact).
Fact = 3628800.
```

### Custom Classes

```prolog
create_processor_class :-
    py_call(builtins:exec("
class DataProcessor:
    def __init__(self, multiplier=1):
        self.multiplier = multiplier

    def process(self, values):
        return [v * self.multiplier for v in values]

    def stats(self, values):
        n = len(values)
        mean = sum(values) / n
        variance = sum((x - mean) ** 2 for x in values) / n
        return {'mean': mean, 'variance': variance, 'count': n}
"), _).

% Use the class
?- create_processor_class,
   py_call('DataProcessor'(multiplier=3), Processor),
   py_call(Processor:process([10, 20, 30]), Result).
Result = [30, 60, 90].
```

---

## Bidirectional Calling

### Python Calling Prolog

```python
from janus_swi import Query

# Define Prolog facts in Python
# (assuming Prolog has ancestor/2 defined)

def find_ancestors(person):
    """Use Prolog to find all ancestors of a person"""
    results = []
    for sol in Query('ancestor_chain(X, Person)', {'Person': person}):
        results.append(sol['X'])
    return results
```

### Hybrid Algorithms

```prolog
% Define Prolog knowledge base
ancestor(tom, bob).
ancestor(bob, pat).
ancestor(pat, jim).

ancestor_chain(X, Y) :- ancestor(X, Y).
ancestor_chain(X, Y) :- ancestor(X, Z), ancestor_chain(Z, Y).

% Python can query this via janus_swi
```

---

## Code Generation

### Wrapper Generation

```prolog
:- use_module('src/unifyweaver/glue/janus_glue').

% Generate Janus wrapper for a Python function
?- generate_janus_wrapper(matrix_inverse/1,
       [module(numpy_linalg), function(inv)],
       Code).
```

Generated code:
```prolog
% Janus wrapper for matrix_inverse/1
% Calls Python function numpy_linalg.inv in-process
matrix_inverse(Arg1, Result) :-
    janus_glue:janus_call_python(numpy_linalg, inv, [Arg1], Result).
```

### Pipeline Generation

```prolog
% Generate a hybrid pipeline
?- generate_janus_pipeline([
       step(preprocess, python, transform/1),
       step(analyze, python, analyze/1),
       step(postprocess, prolog, format_output/1)
   ], [], Code).
```

---

## Integration with UnifyWeaver

### Transport Registration

Janus is registered as a transport type in the glue system:

```prolog
% In your pipeline configuration
compile_pipeline(Steps, [transport(janus)], Code).
```

### Decision Matrix

| Scenario | Transport |
|----------|-----------|
| NumPy/SciPy heavy computation | **Janus** |
| ML model inference | **Janus** |
| Large array processing | **Janus** |
| Streaming data | Pipe |
| Process isolation needed | Pipe |
| Distributed system | HTTP |

---

## API Reference

### Availability

```prolog
janus_available/0        % True if Janus is loaded
janus_available(-Version) % Get version info
janus_numpy_available/0   % True if NumPy is available
```

### Module Management

```prolog
janus_import_module(+ModuleName, -ModuleRef)
janus_reload_module(+ModuleName, -ModuleRef)
janus_add_lib_path(+Path)
```

### Function Calling

```prolog
janus_call_python(+Module, +Function, +Args, -Result)
janus_call_method(+Object, +Method, +Args, -Result)
```

### Object Management

```prolog
janus_create_object(+Module, +ClassName, +Args, -Object)
janus_get_attribute(+Object, +AttrName, -Value)
janus_set_attribute(+Object, +AttrName, +Value)
```

### NumPy Integration

```prolog
janus_numpy_array(+List, -NumpyArray)
janus_numpy_call(+Function, +Args, -Result)
```

### Code Generation

```prolog
generate_janus_wrapper(+Pred/Arity, +Options, -Code)
generate_janus_pipeline(+Steps, +Options, -Code)
```

---

## Troubleshooting

### Janus Not Available

```
ERROR: Janus not available.
```

Ensure you're using SWI-Prolog 9.0+ and Janus is compiled:
```bash
swipl --version
# Should show 9.0.0 or higher

swipl -g "use_module(library(janus)), halt"
# Should succeed without errors
```

### NumPy Not Found

```
ERROR: NumPy not available.
```

Install NumPy in the Python environment Janus uses:
```bash
pip install numpy
```

### Bidirectional Calling Fails

For Python→Prolog calling, install the janus_swi package:
```bash
pip install janus-swi
```

---

## Example Project

See `examples/janus-integration/` for a complete demo with:
- Basic Python calls
- NumPy integration
- Custom modules
- Bidirectional calling
- Wrapper generation
- Performance comparison

Run the demo:
```bash
cd examples/janus-integration
swipl janus_demo.pl
?- run_demo.
```

---

**←** [Previous: Pyodide & Python Variants](20_pyodide_python_variants.md) | **→**
