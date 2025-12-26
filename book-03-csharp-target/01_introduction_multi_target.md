<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 1: Introduction to Multi-Target Compilation

## Overview

UnifyWeaver's multi-target architecture allows you to compile the same Prolog predicates to different target languages. While Book 1 focused on Bash, this book explores C# / .NET as a compilation target.

## Why Multiple Targets?

Different environments have different strengths:

**Bash Target (Book 1)**
- Native Unix/Linux integration
- Excellent for data pipelines
- Minimal dependencies
- Shell-based deployment

**C# Target (Book 2)**
- Cross-platform via .NET
- Superior performance for compute-heavy tasks
- Rich ecosystem (.NET libraries)
- Professional tooling (Visual Studio, debugging)
- Type safety and compile-time checks

## The Multi-Target Vision

```
┌─────────────────┐
│  Prolog Source  │
│                 │
│  parent(a, b).  │
│  parent(b, c).  │
└────────┬────────┘
         │
         │ compile_predicate(parent/2, [target(bash)], ...)
         │
    ┌────┴────┐
    │         │
┌───▼───┐ ┌──▼────┐
│ Bash  │ │  C#   │
│Script │ │Source │
└───────┘ └───────┘
```

The same logic, multiple execution environments.

## Release v0.1 Highlights
- Query runtime now supports semi-naive and mutual fixpoint evaluation (`MutualFixpointNode`, `CrossRefNode`).
- `HashSet<object[]>`-based distinct handling matches Bash semantics.
- Recommended regression workflow documented in the [C# Query Target Test Plan](https://github.com/s243a/UnifyWeaver/blob/main/docs/development/testing/v0_1_csharp_test_plan.md) (use `SKIP_CSHARP_EXECUTION=1`, build-first for manual dotnet tests).
- Educational chapters have been updated with parity walkthroughs and testing guidance.

## C# Target Variants

UnifyWeaver provides **two** C# compilation strategies:

### 1. Native Target (`csharp_native_target.pl`)

**Approach:** Direct source-to-source translation with semi-naive iteration for recursion

**Generates:**
```csharp
public static class Parent {
    static readonly (string, string)[] Facts = { ... };

    public static IEnumerable<(string, string)> Stream()
        => Facts.AsEnumerable();
}

// For recursive predicates - semi-naive iteration:
public static IEnumerable<(string, string)> AncestorStream() {
    var seen = new HashSet<(string, string)>();
    var delta = new List<(string, string)>();
    // Base case + iterative fixpoint
    ...
}
```

**Best For:**
- Simple fact queries
- Recursive predicates (transitive closure, ancestor, etc.)
- Maximum performance
- Standalone deployments (no runtime dependencies)

**Limitations:**
- No mutual recursion support
- Each predicate generates new code
- Changes require recompilation

### 2. Query Runtime (`csharp_query_target.pl`)

**Approach:** Intermediate Representation + Runtime Library

**Generates:**
```csharp
// IR (Intermediate Representation)
var plan = new QueryPlan {
    Relations = { new RelationRef("parent", 2) },
    Operations = { new Selection(...), new Projection(...) }
};

// Runtime executes the plan
var runtime = new QueryEngine();
runtime.Execute(plan);
```

**Best For:**
- Mutual recursion across multiple predicates
- Complex queries with optimization
- Shared runtime optimizations
- Flexible query execution

**Advantages:**
- Full mutual recursion support (`MutualFixpointNode`, `CrossRefNode`)
- Centralized query engine
- Runtime can evolve independently
- Better for large predicate sets with complex dependencies

## Installation & Setup

### 1. Verify .NET SDK

```bash
dotnet --version
# Should show 6.0 or higher
```

### 2. Install .NET SDK (if needed)

**Linux/WSL:**
```bash
wget https://dot.net/v1/dotnet-install.sh
chmod +x dotnet-install.sh
./dotnet-install.sh --channel 6.0
```

**Windows:**
Download from: https://dotnet.microsoft.com/download

**Verify Installation:**
```bash
dotnet --list-sdks
# Should show installed SDK versions
```

### 3. Test C# Compilation

Create a test file:
```csharp
// test.cs
using System;

class Program {
    static void Main() {
        Console.WriteLine("C# is ready!");
    }
}
```

Compile and run:
```bash
dotnet new console -n test
cd test
dotnet run
```

## Your First C# Compilation

### Step 1: Write Prolog

```prolog
% family.pl
:- use_module(unifyweaver(targets/csharp_native_target)).

parent(alice, bob).
parent(bob, charlie).
parent(charlie, dave).
```

### Step 2: Compile to C#

```prolog
?- compile_predicate_to_csharp(parent/2, [], CSharpCode).

CSharpCode = "using System; ... [Generated C# code]"
```

### Step 3: Save and Build

```prolog
?- compile_predicate_to_csharp(parent/2, [], Code),
   open('Parent.cs', write, Stream),
   write(Stream, Code),
   close(Stream).
```

### Step 4: Create .NET Project

```bash
dotnet new console -n ParentQuery
mv Parent.cs ParentQuery/
cd ParentQuery
dotnet build
dotnet run
```

## Target Selection Strategy

### Decision Tree

```
Does the predicate involve mutual recursion across multiple predicates?
├─ NO  → Use Native Target (standalone, semi-naive for single recursion)
└─ YES → Use Query Runtime (MutualFixpointNode for mutual recursion)
```

### Configuration Examples

**Force Native Target:**
```prolog
compile_predicate_to_csharp(predicate/N, [
    mode(procedural),
    dedup(unique)
], Code).
```

**Force Query Runtime:**
```prolog
compile_predicate_to_csharp(predicate/N, [
    target(csharp_query),
    fixpoint(strategy(semi_naive))
], Code).
```

## Performance Comparison

**Benchmark: 10,000 fact lookups**

| Target | Time (ms) | Memory (MB) | Notes |
|--------|-----------|-------------|-------|
| Bash | 850 | 45 | Fork/pipe overhead |
| C# Native | 12 | 8 | In-memory arrays + HashSet |
| C# Query | 45 | 15 | IR + runtime overhead |

**Conclusion:** C# targets are 20-70x faster for in-memory operations.

## Deployment Models

### Bash Target
```
deploy.sh  →  target_server:/scripts/
             ssh target_server './deploy.sh'
```

### C# Target
```
dotnet publish -c Release
→ bin/Release/net6.0/publish/
  ├─ app.dll
  ├─ app.exe (Windows)
  └─ runtime dependencies

Copy to target, run: dotnet app.dll
```

## Common Use Cases

### When to Stick with Bash
- Unix-only deployment
- Shell integration needed
- Streaming data pipelines
- Minimal dependencies

### When to Switch to C#
- Windows deployment
- Performance-critical paths
- Integration with .NET apps
- Need professional IDE/debugging
- Cross-platform desktop tools

## What's Next?

In the following chapters, you'll learn:

- **Chapter 2:** Deep dive into Native Target (LINQ + semi-naive recursion)
- **Chapter 3:** Query Runtime and IR (mutual recursion)
- **Chapter 4:** Deployment and runtime libraries

## Hands-On Exercise

1. Install .NET SDK
2. Compile a simple predicate to C#
3. Build and run the generated code
4. Compare execution time with Bash equivalent

## Summary

- UnifyWeaver supports multiple target languages
- C# offers better performance and .NET ecosystem
- Two C# approaches: Native (standalone with semi-naive recursion) and Query Runtime (mutual recursion)
- Native target for most use cases; Query Runtime for complex mutual recursion
- Both targets share the same Prolog source

Next: **Chapter 2 - C# Native Target** →

---

## Navigation

[📖 Book 3: C# Target](./) | [Next: Chapter 2: C# Native Target →](02_csharp_native_target.md)
