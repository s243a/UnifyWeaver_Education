<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 22: Cross-Runtime Python Bridges with RPyC

Embedding CPython in .NET, JVM, Rust, Ruby, and Go runtimes to access RPyC's live object proxies.

## Overview

This chapter covers seven Python bridge technologies that embed real CPython, enabling access to RPyC from non-Python runtimes:

| Bridge | Runtime | Approach | Status |
|--------|---------|----------|--------|
| **Python.NET** | .NET 6+ | Dynamic execution | Tested |
| **CSnakes** | .NET 8+ | Source generators | Documented |
| **JPype** | JVM | Dynamic execution | Tested |
| **jpy** | JVM | Bi-directional | Tested |
| **PyO3** | Rust | In-process | Tested |
| **PyCall.rb** | Ruby | CPython embedding | Tested |
| **Rust FFI** | Go/Node/Lua | Via Rust cdylib | Tested (Go) |

### Why Embed CPython?

RPyC requires real CPython - reimplementations like IronPython or Jython won't work. By embedding CPython in your .NET or JVM application, you get:

- **Live object proxies** via RPyC
- **Full Python ecosystem** (NumPy, SciPy, pandas)
- **Network transparency** - access remote Python services

---

## 22.1 The Python Bridges Glue Module

Located at `src/unifyweaver/glue/python_bridges_glue.pl`:

```prolog
:- module(python_bridges_glue, [
    % Bridge detection
    detect_pythonnet/1,
    detect_csnakes/1,
    detect_jpype/1,
    detect_jpy/1,
    detect_all_bridges/1,

    % Bridge requirements and validation
    bridge_requirements/2,
    check_bridge_ready/2,
    validate_bridge_config/2,

    % Auto-selection with fallback
    auto_select_bridge/2,
    auto_select_bridge/3,

    % Code generation
    generate_pythonnet_rpyc_client/2,
    generate_csnakes_rpyc_client/2,
    generate_jpype_rpyc_client/2,
    generate_jpy_rpyc_client/2,

    % Generic interface
    generate_python_bridge_client/3,

    % Auto-generation
    generate_auto_client/2,
    generate_auto_client/3
]).
```

### Generic Interface

```prolog
% Generate client for any bridge
?- generate_python_bridge_client(pythonnet, [host("server"), port(18812)], Code).
?- generate_python_bridge_client(jpype, [package("com.myapp")], Code).
```

---

## 22.2 Auto-Detection and Selection

The glue module can automatically detect available bridges and select the best one.

### Detecting Available Bridges

```prolog
?- use_module('src/unifyweaver/glue/python_bridges_glue').

% Detect all available bridges
?- detect_all_bridges(Bridges).
% Bridges = [jpype, jpy]  % (depends on what's installed)

% Check individual bridges
?- detect_jpype(Available).
% Available = true

?- detect_pythonnet(Available).
% Available = false  % (not installed)
```

### Bridge Requirements

```prolog
% What does a bridge need?
?- bridge_requirements(jpype, Reqs).
% Reqs = [requirement(runtime, 'Java 11+'),
%         requirement(python_package, jpype1),
%         requirement(python_package, rpyc),
%         requirement(environment, 'JAVA_HOME must be set')]

?- bridge_requirements(csnakes, Reqs).
% Reqs = [requirement(runtime, '.NET 8.0+'),
%         requirement(nuget_package, 'CSnakes.Runtime'),
%         requirement(python_package, rpyc),
%         requirement(note, 'Uses source generators...')]
```

### Checking Bridge Status

```prolog
% Is a bridge ready to use?
?- check_bridge_ready(jpype, Status).
% Status = ready

?- check_bridge_ready(pythonnet, Status).
% Status = missing_package(pythonnet)

?- check_bridge_ready(csnakes, Status).
% Status = missing_runtime('.NET 8+')
```

### Auto-Selection

```prolog
% Auto-select best bridge for target platform
?- auto_select_bridge(jvm, Bridge).
% Bridge = jpype  % (first available)

?- auto_select_bridge(dotnet, Bridge).
% Bridge = pythonnet  % (or csnakes, or none)

% With explicit preferences
?- auto_select_bridge(jvm, [prefer(jpy)], Bridge).
% Bridge = jpy

% With fallback chain
?- auto_select_bridge(jvm, [fallback([jpy, jpype])], Bridge).
% Bridge = jpy  % (tries jpy first)
```

### Configuration Validation

```prolog
% Validate options before generation
?- validate_bridge_config(jpype, [host(localhost), port(18812)]).
% true

?- validate_bridge_config(jpype, [port(99999)]).
% Invalid port: 99999 (must be 1-65535)
% false
```

---

## 22.3 Preference and Firewall Integration

The auto-selection integrates with UnifyWeaver's preference and firewall systems.

### Preference System

Set bridge preferences at different levels:

```prolog
% Global default preferences
?- assertz(preferences:preferences_default([
       prefer_bridges([jpy, jpype])
   ])).

% Rule-specific preferences
?- assertz(preferences:rule_preferences(
       my_bridge_pred/1,
       [prefer([jpype])]
   )).

% Auto-select now respects these
?- auto_select_bridge(jvm, Bridge).
% Bridge = jpy  % (from global preference)
```

### Firewall System

Control which bridges are allowed:

```prolog
% Deny specific bridges
?- assertz(firewall:rule_firewall(
       python_bridge/1,
       [denied([csnakes])]
   )).

% Only allow specific bridges
?- assertz(firewall:rule_firewall(
       secure_bridge/1,
       [services([pythonnet, jpype])]
   )).

% Auto-select filters by firewall BEFORE applying preferences
?- auto_select_bridge(any, Bridge).
% Only returns bridges allowed by firewall
```

### Firewall Implications

The glue module adds default firewall implications:

```prolog
% If .NET not available, deny .NET bridges
firewall_implies_default(no_dotnet_available,
                        denied(services([pythonnet, csnakes]))).

% If Java not available, deny JVM bridges
firewall_implies_default(no_java_available,
                        denied(services([jpype, jpy]))).

% Prefer source-generated bridges in strict mode
firewall_implies_default(security_policy(strict),
                        prefer(service(dotnet, csnakes),
                               service(dotnet, pythonnet))).
```

---

## 22.4 Auto-Generation

Generate code for the best available bridge automatically:

```prolog
% Auto-generate for JVM (picks jpype or jpy)
?- generate_auto_client(jvm, [port(18812)], Code).
% Generates JPype code (or jpy if JPype unavailable)

% Auto-generate for .NET
?- generate_auto_client(dotnet, [host("server.local")], Code).
% Generates Python.NET code (or CSnakes if preferred)

% With custom options
?- generate_auto_client(jvm, [
       prefer(jpy),
       host("remote-server"),
       port(19000),
       package("com.myapp.bridge")
   ], Code).
```

### Error Handling

```prolog
% If no bridge available
?- generate_auto_client(dotnet, [], Code).
% Code = '// ERROR: No dotnet Python bridge available\n'
```

---

## 22.5 Python.NET (.NET 6+)

Python.NET is the mature choice for .NET Python integration, supporting dynamic Python execution.

### Installation

```bash
pip install pythonnet rpyc
```

### Basic Usage

```python
import os
os.environ['PYTHONNET_RUNTIME'] = 'coreclr'  # Use .NET Core, not Mono

import clr
from System import Environment
print(f".NET Version: {Environment.Version}")

# Connect to RPyC
import rpyc
conn = rpyc.classic.connect('localhost', 18812)
result = conn.modules.math.sqrt(16)  # 4.0
conn.close()
```

### From C#

```csharp
using Python.Runtime;

PythonEngine.Initialize();
using (Py.GIL())
{
    dynamic rpyc = Py.Import("rpyc");
    dynamic conn = rpyc.classic.connect("localhost", 18812);

    dynamic math = conn.modules.math;
    double result = math.sqrt(16);  // 4.0

    conn.close();
}
```

### Code Generation

```prolog
?- generate_pythonnet_rpyc_client([host("localhost"), port(18812)], Code).
```

---

## 22.6 CSnakes (.NET 8+)

CSnakes uses a **source generator** approach - it generates C# wrapper classes from Python files at compile time.

### Key Difference

| Aspect | Python.NET | CSnakes |
|--------|------------|---------|
| Execution | Runtime dynamic | Compile-time generated |
| Type safety | Dynamic | Strong (generated) |
| RPyC access | Direct import | Via wrapper functions |

### Pattern

1. Create Python file with type-annotated functions:

```python
# rpyc_wrapper.py
def connect_rpyc(host: str, port: int) -> bool:
    import rpyc
    conn = rpyc.classic.connect(host, port)
    try:
        return conn.modules.math.sqrt(16) == 4.0
    finally:
        conn.close()
```

2. CSnakes generates C# wrapper at compile time
3. Call from C#:

```csharp
var wrapper = env.RpycWrapper();
bool ok = wrapper.ConnectRpyc("localhost", 18812);
```

### When to Choose CSnakes

- Want compile-time type safety
- Have well-defined Python functions
- Building new .NET 8+ project

---

## 22.7 JPype (JVM)

JPype embeds CPython in JVM with shared memory for NumPy arrays.

### Installation

```bash
pip install jpype1 rpyc
```

### Basic Usage

```python
import jpype
import jpype.imports

jpype.startJVM()

from java.lang import System
print(f"Java: {System.getProperty('java.version')}")

# Connect to RPyC
import rpyc
conn = rpyc.classic.connect('localhost', 18812)
np = conn.modules.numpy
arr = np.array([1, 2, 3, 4, 5])
mean = np.mean(arr)  # 3.0
conn.close()
```

### Code Generation

```prolog
?- generate_jpype_rpyc_client([package("com.myapp")], Code).
```

Generated Java:

```java
public class JPypeRPyCClient implements AutoCloseable {
    public void connect(String host, int port) {
        rpyc = JPype.importModule("rpyc");
        conn = rpyc.invoke("connect", host, port);
    }

    public PyObject getModule(String name) {
        return conn.getAttr("modules").getItem(name);
    }
}
```

---

## 22.8 jpy (Bi-directional)

jpy provides true bi-directional calling - Java can call Python AND Python can call Java.

### Installation

Requires Maven for building Java components:

```bash
export JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64
pip install jpy rpyc
```

### Basic Usage

```python
import jpy

jpy.create_jvm(["-Xmx512m"])

# Python calling Java
ArrayList = jpy.get_type("java.util.ArrayList")
java_list = ArrayList()
java_list.add("hello")

# Connect to RPyC
import rpyc
conn = rpyc.classic.connect('localhost', 18812)
result = conn.modules.math.sqrt(16)
conn.close()
```

### When to Choose jpy

- Java code needs to call Python
- Mixed Java/Python codebases
- JetBrains tools compatibility

---

## 22.9 PyO3 (Rust)

PyO3 provides first-class Rust-Python integration, enabling Rust to call Python code efficiently.

### Installation

```toml
# Cargo.toml
[dependencies]
pyo3 = { version = "0.22", features = ["auto-initialize"] }
```

### Basic Usage

```rust
use pyo3::prelude::*;
use pyo3::types::PyModule;

fn main() -> PyResult<()> {
    Python::with_gil(|py| {
        // Import RPyC
        let rpyc = PyModule::import_bound(py, "rpyc")?;
        let classic = rpyc.getattr("classic")?;

        // Connect
        let conn = classic.call_method1("connect", ("localhost", 18812))?;

        // Call remote function
        let modules = conn.getattr("modules")?;
        let math = modules.get_item("math")?;
        let result: f64 = math.call_method1("sqrt", (16.0,))?.extract()?;

        println!("sqrt(16) = {}", result);  // 4.0

        conn.call_method0("close")?;
        Ok(())
    })
}
```

### Code Generation

```prolog
?- generate_pyo3_rpyc_client([host("localhost"), port(18812)], Code).
```

---

## 22.10 PyCall.rb (Ruby)

PyCall.rb embeds CPython in Ruby, enabling Ruby applications to use Python libraries.

### Installation

```bash
gem install pycall
pip install rpyc
```

### Basic Usage

```ruby
require 'pycall/import'
include PyCall::Import

# Import RPyC
pyimport :rpyc

# Connect to server
conn = rpyc.classic.connect('localhost', 18812)

# Use remote module
np = conn.modules.numpy
arr = np.array([1, 2, 3, 4, 5])
puts "Mean: #{np.mean(arr)}"  # 3.0

conn.close
```

### Code Generation

```prolog
?- generate_pycall_rb_rpyc_client([host("localhost"), port(18812)], Code).
```

---

## 22.11 Rust FFI Bridge (Go, Node.js, Lua)

For languages without mature CPython embedding (Go, Node.js, Lua), we use Rust as a **universal bridge layer**. This approach is fully tested for Go and designed to be extensible to other FFI-capable languages.

### Why Rust as Bridge?

Go's CPython embedding options have limitations:
- **go-python3** (DataDog): Archived since 2021, complex GIL management
- **go-embed-python** (kluctl): Uses subprocess, loses live proxy benefit

Rust's PyO3 is mature, actively maintained, and handles GIL correctly. By building a Rust cdylib, any FFI-capable language can access Python.

### Architecture

```
┌────────────────────────────────────────────────────────┐
│ Go Application                                         │
│ ┌────────────────────────────────────────────────────┐ │
│ │ CGO (C FFI)                                        │ │
│ │ ┌────────────────────────────────────────────────┐ │ │
│ │ │ Rust cdylib (librpyc_bridge.so)                │ │ │
│ │ │ ┌────────────────────────────────────────────┐ │ │ │
│ │ │ │ PyO3 (CPython embedding)                   │ │ │ │
│ │ │ │ ┌────────────────────────────────────────┐ │ │ │ │
│ │ │ │ │ RPyC Client                            │ │ │ │ │
│ │ │ │ └────────────────────────────────────────┘ │ │ │ │
│ │ │ └────────────────────────────────────────────┘ │ │ │
│ │ └────────────────────────────────────────────────┘ │ │
│ └────────────────────────────────────────────────────┘ │
└────────────────────────────────────────────────────────┘
                              │
                              │ TCP (live proxies)
                              ▼
┌────────────────────────────────────────────────────────┐
│ RPyC Server (Python)                                   │
│ - NumPy, SciPy, pandas, scikit-learn, PyTorch         │
│ - Custom ML services                                   │
└────────────────────────────────────────────────────────┘
```

### Building the Rust Bridge

```toml
# Cargo.toml
[package]
name = "rpyc_bridge"
version = "0.1.0"
edition = "2021"

[lib]
crate-type = ["cdylib"]

[dependencies]
pyo3 = { version = "0.22", features = ["auto-initialize"] }
```

The bridge exports C-compatible functions:

```rust
// src/lib.rs
#[no_mangle]
pub extern "C" fn rpyc_init() { ... }

#[no_mangle]
pub extern "C" fn rpyc_connect(host: *const c_char, port: i32) -> i32 { ... }

#[no_mangle]
pub extern "C" fn rpyc_call(
    module: *const c_char,
    function: *const c_char,
    args_json: *const c_char
) -> *mut c_char { ... }

#[no_mangle]
pub extern "C" fn rpyc_disconnect() { ... }
```

### Go Usage

```go
package main

/*
#cgo LDFLAGS: -L${SRCDIR} -lrpyc_bridge
#include "rpyc_bridge.h"
#include <stdlib.h>
*/
import "C"
import (
    "encoding/json"
    "fmt"
    "unsafe"
)

func main() {
    // Initialize Python runtime
    C.rpyc_init()

    // Connect to RPyC server
    host := C.CString("localhost")
    defer C.free(unsafe.Pointer(host))

    if C.rpyc_connect(host, 18812) != 0 {
        panic("Failed to connect")
    }
    defer C.rpyc_disconnect()

    // Call numpy.mean([1, 2, 3, 4, 5])
    result := callPython("numpy", "mean", []interface{}{[]int{1, 2, 3, 4, 5}})
    fmt.Printf("numpy.mean([1,2,3,4,5]) = %v\n", result)  // 3.0
}

func callPython(module, function string, args []interface{}) interface{} {
    argsJSON, _ := json.Marshal(args)

    moduleC := C.CString(module)
    funcC := C.CString(function)
    argsC := C.CString(string(argsJSON))
    defer C.free(unsafe.Pointer(moduleC))
    defer C.free(unsafe.Pointer(funcC))
    defer C.free(unsafe.Pointer(argsC))

    resultC := C.rpyc_call(moduleC, funcC, argsC)
    if resultC == nil {
        return nil
    }
    defer C.rpyc_free_string(resultC)

    var result interface{}
    json.Unmarshal([]byte(C.GoString(resultC)), &result)
    return result
}
```

### Code Generation

```prolog
% Generate Rust bridge library
?- generate_rust_ffi_bridge([host("localhost"), port(18812)], RustCode).

% Generate Go client wrapper
?- generate_go_ffi_client([lib_name(rpyc_bridge)], GoCode).

% Generate Node.js client wrapper
?- generate_node_ffi_client([lib_name(rpyc_bridge)], NodeCode).
```

### Building and Running

```bash
# 1. Build Rust library
cd examples/python-bridges/rust-ffi-go
cargo build --release
cp target/release/librpyc_bridge.so .

# 2. Build Go example
CGO_ENABLED=1 go build -o rpyc_example main.go

# 3. Start RPyC server
python examples/rpyc-integration/rpyc_server.py &

# 4. Run Go example
LD_LIBRARY_PATH=. ./rpyc_example
```

### Expected Output

```
Go + Rust FFI + RPyC Integration
================================

Connecting to RPyC server...
  Connected!

Test 1: math.sqrt(16)
  Result: 4
  PASSED!

Test 2: numpy.mean([1,2,3,4,5])
  Result: 3
  PASSED!

Test 3: math.pi
  Result: 3.141592653589793
  PASSED!

================================
All tests passed!
```

### When to Choose Rust FFI

- Language lacks mature CPython embedding (Go, Lua)
- Need stable, well-maintained bridge
- Want single bridge library for multiple languages
- Performance acceptable (FFI overhead + JSON serialization)

### Node.js Full-Stack Example

The `rust-ffi-node/` directory contains a complete full-stack application:

**Architecture:**
```
Browser (React) → Express API → koffi (FFI) → Rust cdylib → PyO3 → TCP → RPyC → Python
```

**Security Features:**
- **Module whitelisting**: Only `math`, `numpy`, `statistics` allowed
- **Function whitelisting**: Only specific functions per module
- **Input validation**: Type checking, depth limits, size limits
- **Rate limiting**: 100 requests/minute
- **Body size limits**: 100kb max

**Running the Full-Stack Example:**

```bash
# 1. Build Rust library (if not already built)
cd examples/python-bridges/rust-ffi-go
cargo build --release
cp target/release/librpyc_bridge.so ../rust-ffi-node/

# 2. Start RPyC server
python examples/rpyc-integration/rpyc_server.py &

# 3. Install and start backend
cd examples/python-bridges/rust-ffi-node
npm install
npm run dev  # Runs on http://localhost:3001

# 4. Install and start frontend (in another terminal)
cd frontend
npm install
npm start  # Opens http://localhost:3000
```

**API Endpoints:**

| Endpoint | Method | Description |
|----------|--------|-------------|
| `/health` | GET | Health check with connection status |
| `/connect` | POST | Connect to RPyC server |
| `/disconnect` | POST | Disconnect from server |
| `/python/call` | POST | Generic Python call (whitelisted) |
| `/numpy/mean` | POST | Calculate mean of array |
| `/numpy/std` | POST | Calculate standard deviation |
| `/math/sqrt` | POST | Calculate square root |
| `/math/pi` | GET | Get π constant |

**TypeScript FFI with koffi:**

```typescript
import koffi from 'koffi';

const lib = koffi.load('./librpyc_bridge.so');

const rpyc_init = lib.func('void rpyc_init()');
const rpyc_connect = lib.func('int rpyc_connect(const char* host, int port)');
const rpyc_call = lib.func('const char* rpyc_call(const char* m, const char* f, const char* args)');

rpyc_init();
rpyc_connect('localhost', 18812);
const result = rpyc_call('numpy', 'mean', JSON.stringify([[1,2,3,4,5]]));
```

### API Reference

| Function | Description |
|----------|-------------|
| `rpyc_init()` | Initialize Python runtime |
| `rpyc_connect(host, port)` | Connect to RPyC server |
| `rpyc_disconnect()` | Disconnect from server |
| `rpyc_call(module, func, args_json)` | Call remote function |
| `rpyc_getattr(module, attr)` | Get remote attribute |
| `rpyc_free_string(s)` | Free returned string |
| `rpyc_is_connected()` | Check connection status |

---

## 22.12 Declarative TypeScript Generation (Alternative Approach)

The previous sections showed the **manual approach** - writing TypeScript code directly. This section introduces the **declarative approach** using Prolog glue modules that generate TypeScript code from declarative specifications.

### Why Declarative?

The manual approach requires writing:
- Security validation code in TypeScript
- Express routers by hand
- React components manually
- CSS styles for each component

The declarative approach lets you:
- Define **what** you want (not **how** to code it)
- Generate consistent, well-structured TypeScript
- Keep security rules in one place (Prolog)
- Generate entire applications from a single spec

### The Glue Modules

UnifyWeaver provides four TypeScript glue modules:

| Module | Purpose | Location |
|--------|---------|----------|
| `rpyc_security.pl` | Security whitelisting | `glue/rpyc_security.pl` |
| `express_generator.pl` | Express API generation | `glue/express_generator.pl` |
| `react_generator.pl` | React component generation | `glue/react_generator.pl` |
| `full_pipeline.pl` | Full-stack app generation | `glue/full_pipeline.pl` |

### 22.12.1 Declarative Security (rpyc_security.pl)

**Old Approach (Manual TypeScript):**

```typescript
// security.ts - written by hand
const ALLOWED_MODULES: Record<string, Set<string>> = {
  'math': new Set(['sqrt', 'sin', 'cos', 'tan', 'log', 'exp', 'pow']),
  'numpy': new Set(['mean', 'std', 'sum', 'min', 'max', 'array'])
};

export function isCallAllowed(module: string, func: string): boolean {
  const funcs = ALLOWED_MODULES[module];
  return funcs ? funcs.has(func) : false;
}
```

**New Approach (Declarative Prolog):**

```prolog
% Define security rules declaratively
rpyc_allowed_module(math, [sqrt, sin, cos, tan, log, log10, exp, pow, floor, ceil, abs]).
rpyc_allowed_module(numpy, [mean, std, sum, min, max, median, var, array, zeros, ones]).
rpyc_allowed_module(statistics, [mean, median, mode, stdev, variance, quantiles]).

rpyc_allowed_attr(math, [pi, e, tau, inf, nan]).
rpyc_allowed_attr(numpy, ['__version__']).
```

**Generate TypeScript:**

```prolog
?- use_module('src/unifyweaver/glue/rpyc_security').

% Generate the whitelist module
?- generate_typescript_whitelist(Code).
% Code contains:
%   export const ALLOWED_MODULES = {
%     'math': new Set(['sqrt', 'sin', ...]),
%     ...
%   };
%   export function isCallAllowed(module, func) { ... }

% Generate validation middleware
?- generate_express_security_middleware(Code).
% Code contains rate limiter, timeout, validation middleware
```

**Query Security Rules:**

```prolog
% Check what's allowed
?- is_call_allowed(math, sqrt, Result).
Result = true.

?- is_call_allowed(os, system, Result).
Result = false.

% Validate before generation
?- validate_call(numpy, mean, ok).
true.

?- validate_call(subprocess, call, error(Msg)).
Msg = "Blocked module: subprocess".
```

### 22.12.2 Declarative API Endpoints (express_generator.pl)

**Old Approach (Manual Express routes):**

```typescript
// router.ts - written by hand
import { Router } from 'express';

const router = Router();

router.post('/numpy/mean', async (req, res) => {
  const { data } = req.body;
  if (!Array.isArray(data)) {
    return res.status(400).json({ error: 'Missing data array' });
  }
  try {
    const result = await bridge.call('numpy', 'mean', [data]);
    res.json({ success: true, result });
  } catch (error) {
    res.status(500).json({ success: false, error: error.message });
  }
});
```

**New Approach (Declarative Prolog):**

```prolog
% Define API endpoints declaratively
api_endpoint('/numpy/mean', [
    method(post),
    module(numpy),
    function(mean),
    input_schema([data: array(number)]),
    output_schema(number),
    description("Calculate arithmetic mean of an array")
]).

api_endpoint('/math/sqrt', [
    method(post),
    module(math),
    function(sqrt),
    input_schema([value: number]),
    output_schema(number),
    description("Calculate square root")
]).

api_endpoint('/math/pi', [
    method(get),
    module(math),
    attr(pi),
    output_schema(number),
    description("Get mathematical constant pi")
]).
```

**Generate Express Router:**

```prolog
?- use_module('src/unifyweaver/glue/express_generator').

% Generate the complete router
?- generate_express_router(python_api, Code).
% Code contains validated Express routes for all endpoints

% Query endpoints
?- all_endpoints(Endpoints), length(Endpoints, Count).
Count = 12.

?- endpoints_for_module(numpy, NumpyEndpoints).
NumpyEndpoints = ['/numpy/mean', '/numpy/std', '/numpy/sum', ...].
```

### 22.12.3 Declarative React Components (react_generator.pl)

**Old Approach (Manual React components):**

```tsx
// NumpyCalculator.tsx - written by hand
import React, { useState } from 'react';
import styles from './NumpyCalculator.module.css';

export const NumpyCalculator: React.FC = () => {
  const [data, setData] = useState('');
  const [result, setResult] = useState<number | null>(null);
  const [loading, setLoading] = useState(false);

  const handleMean = async () => {
    setLoading(true);
    const response = await fetch('/api/numpy/mean', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ data: data.split(',').map(Number) })
    });
    const json = await response.json();
    setResult(json.result);
    setLoading(false);
  };

  return (
    <div className={styles.container}>
      <h2>NumPy Calculator</h2>
      <input value={data} onChange={e => setData(e.target.value)} />
      <button onClick={handleMean}>Calculate Mean</button>
      {result !== null && <div>{result}</div>}
    </div>
  );
};
```

**New Approach (Declarative Prolog):**

```prolog
% Define UI components declaratively
ui_component(numpy_calculator, [
    type(form),
    title("NumPy Calculator"),
    inputs([
        input(data, array(number), "Numbers", "Enter comma-separated numbers")
    ]),
    operations([
        operation(mean, '/api/numpy/mean', "Mean", []),
        operation(std, '/api/numpy/std', "Std Dev", []),
        operation(sum, '/api/numpy/sum', "Sum", [])
    ]),
    result_display(number, [precision(6)])
]).

ui_component(math_constants, [
    type(display),
    title("Math Constants"),
    constants([
        constant(pi, '/api/math/pi', "π"),
        constant(e, '/api/math/e', "e"),
        constant(tau, '/api/math/tau', "τ")
    ])
]).
```

**Generate React Components:**

```prolog
?- use_module('src/unifyweaver/glue/react_generator').

% Generate TSX component
?- generate_react_component(numpy_calculator, TsxCode).
% TsxCode contains full React.FC component with TypeScript

% Generate CSS module
?- generate_component_styles(numpy_calculator, CssCode).
% CssCode contains .container, .button, .input, .result, .error classes

% Generate custom hooks
?- generate_api_hooks(python, HooksCode).
% HooksCode contains useApiCall hook with loading/error state
```

### 22.12.4 Full Application Generation (full_pipeline.pl)

The most powerful feature: generate a complete full-stack application from a single specification.

**Old Approach:** Manually create 15-20 files, copy boilerplate, ensure consistency.

**New Approach (Single Prolog Specification):**

```prolog
% Define entire application declaratively
application(python_bridge_demo, [
    backend([
        server(express, [port(3001)]),
        rpyc_server([host(localhost), port(18812)]),
        security([whitelist(rpyc_security), rate_limit(100)])
    ]),
    api([
        include_endpoints([math_endpoints, numpy_endpoints, statistics_endpoints]),
        prefix('/api')
    ]),
    frontend([
        framework(react),
        components([numpy_calculator, math_calculator, math_constants]),
        theme(modern)
    ]),
    deployment([
        docker([node_version('18'), python_version('3.10')]),
        port(3000)
    ])
]).
```

**Generate Complete Application:**

```prolog
?- use_module('src/unifyweaver/glue/full_pipeline').

% Generate all files
?- generate_application(python_bridge_demo, Files).
Files = [
    file('package.json', '...'),
    file('tsconfig.json', '...'),
    file('src/server/index.ts', '...'),
    file('src/server/router.ts', '...'),
    file('src/server/whitelist.ts', '...'),
    file('src/server/validator.ts', '...'),
    file('src/server/middleware.ts', '...'),
    file('src/App.tsx', '...'),
    file('src/components/NumpyCalculator.tsx', '...'),
    file('src/components/MathCalculator.tsx', '...'),
    file('src/components/MathConstants.tsx', '...'),
    file('src/hooks/useApiCall.ts', '...'),
    file('src/styles/*.module.css', '...'),
    file('Dockerfile', '...'),
    file('docker-compose.yml', '...'),
    file('README.md', '...')
].
```

**Write to Disk:**

```prolog
% Generate and write all files
?- generate_application(python_bridge_demo, Files),
   forall(member(file(Path, Content), Files),
          (format("Writing: ~w~n", [Path]),
           write_file(Path, Content))).
```

### 22.12.5 Comparison: Manual vs Declarative

| Aspect | Manual Approach | Declarative Approach |
|--------|-----------------|----------------------|
| **Effort** | Write each file by hand | Define spec, generate all |
| **Consistency** | Depends on developer | Guaranteed by generator |
| **Maintainability** | Update each file separately | Update spec, regenerate |
| **Security rules** | Scattered across files | Centralized in Prolog |
| **Customization** | Full control | Extensible templates |
| **Learning curve** | Know TypeScript/React | Know Prolog specs |

### When to Use Each Approach

**Use Manual Approach when:**
- Building highly custom UI
- Need fine-grained control
- Prototyping one-off components
- Team doesn't know Prolog

**Use Declarative Approach when:**
- Building multiple similar applications
- Want consistent security enforcement
- Need to maintain many endpoints
- Integrating with UnifyWeaver pipelines
- Want single source of truth

### Running the Integration Tests

The glue modules include comprehensive tests:

```bash
swipl -g "consult('tests/integration/glue/test_typescript_glue'), run_tests, halt"
```

Expected output:
```
TypeScript Glue Integration Tests
========================================

--- RPyC Security Validation ---
  [PASS] math.sqrt is allowed
  [PASS] os.system is denied
  ...

--- Express Endpoint Queries ---
  [PASS] Has more than 5 endpoints (12 > 5)
  ...

--- React Component Generation ---
  [PASS] Component code length > 3000
  ...

--- Full Pipeline File Generation ---
  [PASS] Generates more than 15 files (20 > 15)
  ...

========================================
Results: 111/111 tests passed
All tests passed!
========================================
```

### 22.12.6 Preferences and Firewall Integration

The glue modules integrate with UnifyWeaver's preference and firewall systems for context-aware code generation.

**Configuration Module:** `glue/typescript_glue_config.pl`

**Setting Generation Context:**

```prolog
?- use_module('src/unifyweaver/glue/typescript_glue_config').

% Set context for all subsequent generation
?- set_generation_context(production).

% Or use context temporarily
?- with_context(testing, generate_application(my_app, Files)).
```

**Context-Based Defaults:**

| Context | RPyC Timeout | Retry Count | Express Morgan | Allowed Modules |
|---------|--------------|-------------|----------------|-----------------|
| production | 60000ms | 5 | combined | math, numpy, statistics |
| development | 5000ms | 1 | dev | + pandas, matplotlib |
| testing | 1000ms | 1 | dev | + unittest, pytest |

**Getting Configuration with Preferences:**

```prolog
% Get RPyC config with defaults and context applied
?- get_rpyc_config(Config).
Config = [host(localhost), port(18812), timeout(30000), ...].

% Override specific values
?- get_rpyc_config([port(19000), timeout(5000)], Config).

% Get Express config
?- get_express_config(Config).
Config = [port(3001), cors_enabled(true), ...].
```

**Context-Aware Application Generation:**

```prolog
% Generate with production settings
?- generate_application(python_bridge_demo, [context(production)], Files).

% Generate with development settings (more lenient)
?- generate_application(python_bridge_demo, [context(development)], Files).
```

**Firewall-Aware Validation:**

```prolog
% Validate using both local rules and firewall
?- validate_with_firewall(math, sqrt, Result).
Result = ok.

?- validate_with_firewall(os, system, Result).
Result = error(module_not_allowed(os)).
```

**Firewall Implications:**

The config module registers default firewall implications:

```prolog
% No Python → deny RPyC services
firewall_implies_default(no_python_available,
                        denied(service(typescript, rpyc(_)))).

% Offline mode → deny RPyC connections
firewall_implies_default(mode(offline),
                        denied(service(typescript, rpyc(_)))).

% Strict security → require module whitelist
firewall_implies_default(security_policy(strict),
                        require_python_module_whitelist).
```

---

## 22.13 Architecture Overview

All bridges follow a similar architecture pattern:

```
┌─────────────────────────────────────────────────────────────┐
│ Host Application (.NET, JVM, Rust, Go, Ruby)                │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ Bridge (Python.NET / JPype / PyO3 / Rust FFI / PyCall)  │ │
│ │ ┌─────────────────────────────────────────────────────┐ │ │
│ │ │ Embedded CPython                                    │ │ │
│ │ │ ┌─────────────────────────────────────────────────┐ │ │ │
│ │ │ │ RPyC Client                                     │ │ │ │
│ │ │ │                                                 │ │ │ │
│ │ │ │  conn = rpyc.connect("server", 18812)           │ │ │ │
│ │ │ │  result = conn.modules.numpy.array([1,2,3])     │ │ │ │
│ │ │ └─────────────────────────────────────────────────┘ │ │ │
│ │ └─────────────────────────────────────────────────────┘ │ │
│ └─────────────────────────────────────────────────────────┘ │
└─────────────────────────────────────────────────────────────┘
                              │
                              │ TCP/IP (live proxies)
                              ▼
┌─────────────────────────────────────────────────────────────┐
│ RPyC Server (Python)                                        │
│ - NumPy, SciPy, pandas available                            │
│ - Custom services                                           │
│ - Live object proxies                                       │
└─────────────────────────────────────────────────────────────┘
```

---

## 22.14 Decision Matrix

| Scenario | Recommended Bridge |
|----------|--------------------|
| .NET project, dynamic Python | **Python.NET** |
| .NET 8+, type-safe wrappers | **CSnakes** |
| Java project, NumPy performance | **JPype** |
| Java↔Python bi-directional | **jpy** |
| Need .NET 6/7 compatibility | **Python.NET** |
| Don't have Maven | **JPype** |
| Rust project, in-process Python | **PyO3** |
| Ruby project | **PyCall.rb** |
| Go project (no CGO alternatives) | **Rust FFI** |
| Node.js project | **Rust FFI + koffi** |
| Full-stack JS with Python ML | **Rust FFI + Express + React** |
| Multi-language FFI from single lib | **Rust FFI** |

---

## 22.15 Tested Configurations

These configurations have been verified (2025-12-29):

| Bridge | Runtime Version | Python | RPyC Tests |
|--------|-----------------|--------|------------|
| Python.NET | .NET Core 9.0 | 3.8.10 | math, NumPy |
| JPype | Java 11.0.27 | 3.8.10 | math, NumPy |
| jpy | Java 11.0.27 | 3.8.10 | math, NumPy, ArrayList |
| PyO3 | Rust/Cargo | 3.8.10 | math, NumPy |
| PyCall.rb | Ruby 3.0+ | 3.8.10 | math, NumPy |
| Rust FFI (Go) | Go 1.21+ | 3.8.10 | math, numpy.mean, math.pi |
| Rust FFI (Node.js) | Node.js 18+ | 3.8.10 | math, numpy.mean, math.pi |

---

## 22.16 Example Projects

See `examples/python-bridges/` for complete working examples:

```
examples/python-bridges/
├── README.md
├── pythonnet/
│   ├── rpyc_client.py
│   ├── RPyCClient.cs
│   └── pythonnet-rpyc.csproj
├── csnakes/
│   ├── rpyc_wrapper.py
│   ├── RPyCClient.cs
│   └── csnakes-rpyc.csproj
├── jpype/
│   ├── rpyc_client.py
│   ├── RPyCClient.java
│   └── build.gradle
├── jpy/
│   ├── rpyc_client.py
│   └── README.md
├── pyo3/
│   ├── src/main.rs
│   └── Cargo.toml
├── pycall-rb/
│   ├── rpyc_client.rb
│   └── Gemfile
├── rust-ffi-go/
│   ├── src/lib.rs
│   ├── Cargo.toml
│   ├── rpyc_bridge.h
│   ├── main.go
│   └── README.md
└── rust-ffi-node/
    ├── src/
    │   ├── rpyc_bridge.ts
    │   ├── server.ts
    │   └── test.ts
    ├── frontend/
    │   └── src/App.tsx
    ├── package.json
    └── README.md
```

---

## 22.17 Exercises

### Exercise 1: Python.NET NumPy Pipeline

Create a .NET application that:
1. Connects to RPyC server
2. Creates a NumPy array remotely
3. Performs matrix operations
4. Returns results to C#

### Exercise 2: JPype Data Processing

Build a Java application that:
1. Loads data from a Java source
2. Passes to remote NumPy via RPyC
3. Gets processed results back
4. Visualizes in Java

### Exercise 3: Hybrid .NET/Python Service

Design a system where:
1. C# handles HTTP requests
2. Python.NET connects to RPyC backend
3. NumPy performs ML inference
4. Results returned to web client

### Exercise 4: Rust FFI Bridge for Go

Build a Go application using the Rust FFI bridge:
1. Build the Rust library with `cargo build --release`
2. Connect to RPyC server from Go
3. Use NumPy for statistical calculations
4. Display results in Go

### Exercise 5: Multi-Language Pipeline

Create a pipeline using multiple bridges:
1. Go reads data (via Rust FFI bridge)
2. Python processes with NumPy
3. Results stored in database
4. Use cross_runtime_pipeline.pl for orchestration

### Exercise 6: Node.js Full-Stack Application

Build a full-stack JavaScript application:
1. Run the rust-ffi-node example
2. Add a new API endpoint for `statistics.median`
3. Update the React UI to include median calculation
4. Add the new function to the security whitelist
5. Test with various input arrays

### Exercise 7: Declarative Security Rules

Define security rules in Prolog:
1. Create whitelisting rules: `allowed_module(Module, Functions)`
2. Generate TypeScript validation code from these rules
3. Compare with manual approach in server.ts
4. Consider how preferences/firewall could control this

---

## 22.18 Summary

| Concept | Key Point |
|---------|-----------|
| **Python.NET** | Dynamic Python execution in .NET |
| **CSnakes** | Source generator, compile-time wrappers |
| **JPype** | JVM Python with NumPy shared memory |
| **jpy** | True bi-directional Java↔Python |
| **PyO3** | Rust-native CPython embedding |
| **PyCall.rb** | Ruby CPython embedding |
| **Rust FFI** | Universal bridge for Go/Node/Lua via cdylib |
| **Node.js + React** | Full-stack JS with security whitelisting |
| **RPyC** | Live object proxies over network |
| **CPython required** | Reimplementations (IronPython, Jython) won't work |
| **Auto-detection** | `detect_all_bridges/1` finds available bridges |
| **Auto-selection** | `auto_select_bridge/2` picks best bridge |
| **Preferences** | Bridge order via `preferences_default/1` |
| **Firewall** | Deny/allow bridges via `rule_firewall/2` |
| **Fallback chains** | `[fallback([jpy, jpype])]` for resilience |
| **Code generation** | `generate_*_rpyc_client/2` for all bridges |
| **Declarative security** | `rpyc_security.pl` - whitelisting in Prolog |
| **Declarative API** | `express_generator.pl` - endpoints from specs |
| **Declarative UI** | `react_generator.pl` - components from specs |
| **Full-stack generation** | `full_pipeline.pl` - 20 files from one spec |
| **Context-aware config** | `typescript_glue_config.pl` - preferences integration |
| **Generation contexts** | production, development, testing modes |

---

**←** [Previous: Chapter 21 - Janus Integration](21_janus_integration.md) | **→** [Next: Chapter 23](23_future.md)
