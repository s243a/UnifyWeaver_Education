<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 22: Cross-Runtime Python Bridges with RPyC

Embedding CPython in .NET and JVM runtimes to access RPyC's live object proxies.

## Overview

This chapter covers four Python bridge technologies that embed real CPython, enabling access to RPyC from non-Python runtimes:

| Bridge | Runtime | Approach | Status |
|--------|---------|----------|--------|
| **Python.NET** | .NET 6+ | Dynamic execution | Tested |
| **CSnakes** | .NET 8+ | Source generators | Documented |
| **JPype** | JVM | Dynamic execution | Tested |
| **jpy** | JVM | Bi-directional | Tested |

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

## 22.9 Architecture

```
┌─────────────────────────────────────────────────────────────┐
│ Host Application (.NET or JVM)                              │
│ ┌─────────────────────────────────────────────────────────┐ │
│ │ Bridge (Python.NET / CSnakes / JPype / jpy)             │ │
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

## 22.10 Decision Matrix

| Scenario | Recommended Bridge |
|----------|--------------------|
| .NET project, dynamic Python | **Python.NET** |
| .NET 8+, type-safe wrappers | **CSnakes** |
| Java project, NumPy performance | **JPype** |
| Java↔Python bi-directional | **jpy** |
| Need .NET 6/7 compatibility | **Python.NET** |
| Don't have Maven | **JPype** |

---

## 22.11 Tested Configurations

These configurations have been verified (2025-12-27):

| Bridge | Runtime Version | Python | RPyC Tests |
|--------|-----------------|--------|------------|
| Python.NET | .NET Core 9.0 | 3.8.10 | math, NumPy |
| JPype | Java 11.0.27 | 3.8.10 | math, NumPy |
| jpy | Java 11.0.27 | 3.8.10 | math, NumPy, ArrayList |

---

## 22.12 Example Projects

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
└── jpy/
    ├── rpyc_client.py
    └── README.md
```

---

## 22.13 Exercises

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

---

## Summary

| Concept | Key Point |
|---------|-----------|
| **Python.NET** | Dynamic Python execution in .NET |
| **CSnakes** | Source generator, compile-time wrappers |
| **JPype** | JVM Python with NumPy shared memory |
| **jpy** | True bi-directional Java↔Python |
| **RPyC** | Live object proxies over network |
| **CPython required** | Reimplementations (IronPython, Jython) won't work |
| **Auto-detection** | `detect_all_bridges/1` finds available bridges |
| **Auto-selection** | `auto_select_bridge/2` picks best bridge |
| **Preferences** | Bridge order via `preferences_default/1` |
| **Firewall** | Deny/allow bridges via `rule_firewall/2` |
| **Fallback chains** | `[fallback([jpy, jpype])]` for resilience |

---

**←** [Previous: Chapter 21 - Janus Integration](21_janus_integration.md) | **→** [Next: Chapter 23](23_future.md)
