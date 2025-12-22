<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 6: In-Process C# Hosting

**Book 12: PowerShell Target**

---

## Overview

This chapter covers UnifyWeaver's support for **in-process C# hosting** from PowerShell. This enables:

- Compiling C# code inline using `Add-Type`
- Loading and invoking .NET assemblies
- Managing PowerShell runspaces from C#
- Building cross-target pipelines (C# ↔ PowerShell ↔ Python)

The key advantage of in-process hosting is **zero serialization overhead** - objects pass directly between runtimes without JSON/TSV conversion.

---

## Architecture

### Communication Patterns

UnifyWeaver supports three communication patterns for .NET interop:

| Pattern | Latency | Use Case |
|---------|---------|----------|
| **In-Process** | ~0ms | Same AppDomain, direct object passing |
| **Pipe-Based** | ~5-10ms | Cross-process, JSON/TSV serialization |
| **Network** | Variable | Distributed, HTTP/gRPC |

This chapter focuses on **in-process** communication using:

1. `System.Management.Automation` for hosting PowerShell from C#
2. `Add-Type` for compiling C# from PowerShell
3. `IronPython.Hosting` for hosting Python from C#

### Module Structure

```
src/unifyweaver/
├── glue/
│   └── dotnet_glue.pl      # Core .NET bridge generation
├── core/
│   └── powershell_compiler.pl  # Integration predicates
└── bindings/
    └── powershell_bindings.pl  # C# hosting bindings
```

---

## Using the C# Hosting API

### Compiling with C# Host

The `compile_with_csharp_host/4` predicate generates both PowerShell code and a C# host class:

```prolog
?- compile_with_csharp_host(
       [filter_users/2, transform_data/2],
       [namespace('MyApp.Generated'), class('DataProcessor')],
       PSCode,
       CSharpCode
   ).
```

This produces:

1. **PSCode** - PowerShell functions for the predicates
2. **CSharpCode** - C# class with typed wrapper methods

### Generating Bridges

Use `generate_csharp_bridge/3` to create specific bridge types:

```prolog
% PowerShell bridge (C# hosts PowerShell)
?- generate_csharp_bridge(powershell, [namespace('MyApp')], Code).

% IronPython bridge (C# hosts Python)
?- generate_csharp_bridge(ironpython, [namespace('MyApp')], Code).

% CPython fallback (pipe-based)
?- generate_csharp_bridge(cpython, [], Code).
```

### Cross-Target Pipelines

Build pipelines that span multiple languages:

```prolog
?- compile_cross_target_pipeline([
       step(powershell, load_users/1, []),
       step(csharp, validate/2, [inline_code("...")]),
       step(python, analyze/2, [])
   ], [], Code).
```

---

## Generated C# Code

### PowerShell Bridge

The generated `PowerShellBridge` class provides:

```csharp
public static class PowerShellBridge
{
    private static readonly Runspace SharedRunspace;

    // Invoke script with typed input/output
    public static IEnumerable<TOutput> Invoke<TInput, TOutput>(
        string script, TInput input);

    // Stream processing
    public static IEnumerable<TOutput> InvokeStream<TInput, TOutput>(
        string script, IEnumerable<TInput> inputStream);

    // Command invocation with parameters
    public static IEnumerable<TOutput> InvokeCommand<TOutput>(
        string commandName, IDictionary<string, object> parameters);

    // Variable management
    public static void SetVariable(string name, object value);
    public static T GetVariable<T>(string name);
}
```

### Host Class

For each compiled predicate, a typed wrapper is generated:

```csharp
public class PowerShellHost
{
    public PowerShellHost(string scriptBlock)
    {
        // Initialize runspace with script
    }

    public IEnumerable<dynamic> filter_users(string arg1, string arg2)
    {
        var script = "filter_users $arg1 $arg2";
        return PowerShellBridge.Invoke<object, dynamic>(script, null);
    }
}
```

---

## PowerShell Bindings for C# Hosting

UnifyWeaver provides 16 bindings for C# hosting operations:

### Inline C# Compilation

| Binding | Target | Description |
|---------|--------|-------------|
| `add_type/1` | `Add-Type -TypeDefinition` | Compile inline C# |
| `load_assembly/1` | `Add-Type -AssemblyName` | Load GAC assembly |
| `load_dll/1` | `Add-Type -Path` | Load DLL from path |

### Object Creation

| Binding | Target | Description |
|---------|--------|-------------|
| `new_object/2` | `New-Object` | Create .NET instance |
| `new_object/3` | `New-Object -TypeName` | Create with arguments |

### Runspace Management

| Binding | Target | Description |
|---------|--------|-------------|
| `create_runspace/1` | `RunspaceFactory.CreateRunspace()` | Create runspace |
| `open_runspace/1` | `.Open()` | Open runspace |
| `create_powershell/1` | `PowerShell.Create()` | Create PS instance |

### Script Execution

| Binding | Target | Description |
|---------|--------|-------------|
| `add_script/2` | `.AddScript` | Add script to PS |
| `add_command/2` | `.AddCommand` | Add command |
| `add_parameter/3` | `.AddParameter` | Add parameter |
| `invoke_powershell/2` | `.Invoke()` | Execute and get results |

### Type Operations

| Binding | Target | Description |
|---------|--------|-------------|
| `cast_type/3` | `-as` | Cast to type |
| `is_type/2` | `-is` | Type check |
| `get_assembly_types/2` | `.GetTypes()` | Get types from assembly |
| `get_type_assembly/2` | `.Assembly` | Get type's assembly |

---

## Example: Data Processing Pipeline

### Prolog Definition

```prolog
% Define facts
user(1, "Alice", 30).
user(2, "Bob", 25).
user(3, "Charlie", 35).

% Filter rule
adult_user(Id, Name) :- user(Id, Name, Age), Age >= 30.

% Compile with C# host
:- compile_with_csharp_host(
       [adult_user/2],
       [namespace('DataPipeline')],
       PSCode, CSharpCode
   ).
```

### Generated PowerShell

```powershell
function adult_user {
    param([string]$X, [string]$Y)

    $user_data = user

    $results = foreach ($fact in $user_data) {
        $Id = $fact.X
        $Name = $fact.Y
        $Age = $fact.F2

        if ($Age -ge 30) {
            [PSCustomObject]@{ X = $Id; Y = $Name }
        }
    }
    $results
}
```

### Using from C#

```csharp
using DataPipeline;

// Initialize with PowerShell script
var host = new PowerShellHost(psScript);

// Call the compiled predicate
foreach (var user in host.adult_user("", ""))
{
    Console.WriteLine($"ID: {user.X}, Name: {user.Y}");
}
```

---

## Runtime Detection

UnifyWeaver automatically detects available runtimes:

```prolog
?- dotnet_glue:detect_dotnet_runtime(Runtime).
Runtime = dotnet_modern.  % .NET 5+

?- dotnet_glue:detect_powershell(Version).
Version = core("PowerShell 7.4.0").

?- dotnet_glue:detect_ironpython(Available).
Available = true.
```

### Runtime Selection

For Python integration, UnifyWeaver chooses the optimal runtime:

```prolog
?- dotnet_glue:python_runtime_choice([json, re, os], Runtime).
Runtime = ironpython.  % All compatible with IronPython

?- dotnet_glue:python_runtime_choice([numpy, pandas], Runtime).
Runtime = cpython_pipe.  % C extensions need CPython
```

---

## IronPython Compatibility

IronPython supports most Python stdlib but NOT C extensions:

### Compatible Modules

- `sys`, `os`, `io`, `json`, `re`, `math`
- `collections`, `itertools`, `functools`
- `datetime`, `time`, `random`
- `pathlib`, `glob`, `shutil`
- `xml.etree`, `csv`, `sqlite3`

### Incompatible (Use CPython)

- `numpy`, `pandas`, `scipy`
- `tensorflow`, `torch`
- `lxml` (use `xml.etree` instead)
- `PIL/Pillow`

---

## Performance Considerations

### In-Process vs Subprocess

| Operation | In-Process | Subprocess |
|-----------|------------|------------|
| First call | ~50ms (JIT) | ~200ms (spawn) |
| Subsequent | ~0.1ms | ~10ms |
| Large data | Zero-copy | Serialization |

### Best Practices

1. **Reuse runspaces** - The shared runspace is initialized once
2. **Batch operations** - Use `InvokeStream` for collections
3. **Avoid repeated Add-Type** - Cache compiled assemblies
4. **Use IronPython when possible** - Avoids subprocess overhead

---

## Testing

Run the C# hosting tests:

```bash
swipl -g "
    use_module('src/unifyweaver/core/powershell_compiler'),
    powershell_compiler:test_csharp_hosting,
    halt
"
```

Expected output:
```
=== Testing C# Hosting Integration ===

[Test 1] Generate PowerShell bridge
  [PASS] PowerShell bridge generated with runspace
[Test 2] Runtime detection
  [PASS] Runtime detection complete
[Test 3] Generate IronPython bridge
  [PASS] IronPython bridge generated with custom namespace
[Test 4] C# parameter generation
  [PASS] Parameters generated correctly

=== C# Hosting Tests Complete ===
```

---

## Summary

This chapter covered:

- **In-process C# hosting** architecture
- **`compile_with_csharp_host/4`** for dual-output compilation
- **Bridge generation** for PowerShell, IronPython, CPython
- **16 new bindings** for C# hosting operations
- **Runtime detection** and selection
- **Performance** characteristics and best practices

The C# hosting integration enables seamless .NET ecosystem access from UnifyWeaver-compiled PowerShell code, with zero serialization overhead for in-process communication.

---

## Next Steps

- [Chapter 4: .NET Integration](04_dotnet_integration.md) - Add-Type and inline C#
- [Book 7: Cross-Target Glue](../book-07-cross-target-glue/README.md) - Multi-language pipelines
- [Book 3: C# Target](../book-03-csharp-target/README.md) - Pure C# compilation
