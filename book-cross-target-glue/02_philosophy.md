<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Philosophy and Design Principles

## Core Principles

The cross-target glue system is built on five core principles that guide every design decision.

## Principle 1: Location Transparency

**Predicates should be callable regardless of where they execute.**

The caller shouldn't need to know if a predicate runs:
- In the same process
- In a separate process on the same machine
- On a remote machine

### Example: Location-Agnostic Logic

```prolog
% The caller doesn't care where each predicate runs
result(X) :-
    fetch_data(RawData),      % Maybe Bash/curl
    analyze(RawData, Result),  % Maybe Python/pandas
    store(Result, X).          % Maybe SQL on remote DB
```

### Why This Matters

**Development**: Write logic once, deploy anywhere.

**Testing**: Run everything locally during development, distribute in production.

**Evolution**: Move predicates between targets without changing callers.

### How It's Achieved

1. **Declarative configuration** separates logic from deployment
2. **Automatic glue generation** handles communication details
3. **Consistent interfaces** across all targets

## Principle 2: Sensible Defaults with Override

**The system should "just work" with intelligent defaults, but allow explicit control.**

### Default Behaviors

| Scenario | Default | Rationale |
|----------|---------|-----------|
| Same runtime family | In-process | Zero serialization |
| Different runtimes | Pipes with TSV | Universal, efficient |
| Remote targets | HTTP with JSON | Standard, debuggable |

### Explicit Override

When defaults aren't optimal, override them:

```prolog
% Default: Python runs in separate process
:- declare_target(analyze/2, python).

% Override: Force in-process (IronPython in .NET)
:- declare_target(analyze/2, python, [location(in_process)]).

% Override: Force remote execution
:- declare_location(heavy_compute/2, [
    host('gpu-worker.local'),
    port(8080),
    transport(http)
]).
```

### Gradual Specification

Start simple, add constraints as needed:

```prolog
% Level 1: Just works (TSV over pipes)
:- declare_target(analyze/2, python).

% Level 2: Specify format
:- declare_target(analyze/2, python, [format(json)]).

% Level 3: Full control
:- declare_target(analyze/2, python, [
    format(json),
    location(local_process),
    timeout(30),
    retry(3)
]).
```

## Principle 3: Runtime Family Affinity

**Languages sharing a runtime should prefer in-process communication.**

### Runtime Families

| Family | Languages | Communication |
|--------|-----------|---------------|
| .NET/CLR | C#, F#, PowerShell, IronPython | In-process calls |
| JVM | Java, Scala, Clojure, Jython | In-process calls |
| Native | Go, Rust, C | Shared memory or pipes |
| Shell | Bash, AWK, Perl | Process pipes |
| Python | CPython | Process pipes |

### In-Process Benefits

For .NET family:

```csharp
// C# calling PowerShell in-process
var ps = PowerShell.Create();
ps.AddScript(scriptCode);
var results = ps.Invoke<OutputType>();  // Direct object passing!
```

No serialization. No parsing. Direct memory access.

### When to Break Affinity

Sometimes you want separate processes even for same-family targets:

```prolog
% Force separate process for untrusted code
:- declare_location(untrusted_code/2, [
    process(separate),
    sandbox(true)
]).

% Force separate process for memory isolation
:- declare_location(memory_hungry/2, [
    process(separate)
]).
```

## Principle 4: Data Format Negotiation

**Targets should agree on data formats automatically.**

### Format Selection Rules

```
Same process:     Native objects (no serialization)
Process pipes:    TSV (default), JSON, or binary
Network:          JSON (default), Protocol Buffers, or custom
```

### TSV: The Default Choice

TSV (Tab-Separated Values) is the default for pipes because:

1. **Universal**: Every language can parse it trivially
2. **Streaming**: Process line-by-line, bounded memory
3. **Debuggable**: `cat`, `head`, `tail` work perfectly
4. **Fast**: No parsing overhead for simple types

```
name    age    city
Alice   30     NYC
Bob     25     LA
```

### JSON: For Structure

Use JSON when you need:
- Nested data structures
- Mixed types in arrays
- Self-describing format
- API compatibility

```json
{"name": "Alice", "age": 30, "address": {"city": "NYC", "zip": "10001"}}
```

### Protocol Specification

#### TSV Protocol

```
┌─────────────────────────────────────────────────────┐
│                   TSV PROTOCOL                       │
├─────────────────────────────────────────────────────┤
│ Encoding: UTF-8                                      │
│ Record delimiter: Newline (\n)                       │
│ Field delimiter: Tab (\t)                            │
│ Escape: Backslash for \t, \n, \\                    │
│ NULL: Empty field                                    │
│ EOF: Close pipe                                      │
└─────────────────────────────────────────────────────┘
```

#### JSON Lines Protocol

```
┌─────────────────────────────────────────────────────┐
│                  JSON LINES                          │
├─────────────────────────────────────────────────────┤
│ One JSON object per line                             │
│ Encoding: UTF-8                                      │
│ Record delimiter: Newline (\n)                       │
│ NULL: JSON null                                      │
│ EOF: Close connection                                │
└─────────────────────────────────────────────────────┘
```

## Principle 5: Streaming by Default

**Data should flow as streams, not batch loads.**

### The Streaming Model

```
Producer → Pipe → Consumer → Pipe → Next Consumer
```

### Benefits

**Memory Efficiency**: Process records one at a time, bounded memory.

```bash
# This works on a 100GB file with 1MB of memory
cat huge.tsv | awk '{...}' | python3 transform.py | sort
```

**Pipeline Parallelism**: All stages run concurrently.

```
┌────────┐   ┌────────┐   ┌────────┐
│ Stage 1│──►│ Stage 2│──►│ Stage 3│
│ (busy) │   │ (busy) │   │ (busy) │
└────────┘   └────────┘   └────────┘
     ↑            ↑            ↑
  All stages working simultaneously!
```

**Early Termination**: Consumer can stop producer.

```bash
# Only processes until first 10 matches
grep "ERROR" huge.log | head -10
```

### Backpressure Handling

When a consumer is slow, the pipe buffers, and eventually the producer blocks. This prevents memory exhaustion.

```prolog
% Configure buffer strategy
:- declare_connection(fast_producer/2, slow_consumer/2, [
    buffer(block(65536))  % 64KB blocks
]).
```

## Design Goals

### Composability

Any target should compose with any other:

```prolog
% AWK → Python → Go → SQL
pipeline(Input, Output) :-
    awk_filter(Input, Filtered),
    python_transform(Filtered, Transformed),
    go_aggregate(Transformed, Aggregated),
    sql_insert(Aggregated, Output).
```

### Minimal Overhead

| Communication | Overhead |
|--------------|----------|
| In-process | Zero (direct calls) |
| Local pipe | ~1ms per 10K records |
| Network | ~5-50ms per request |

### Debuggability

- Trace data flow across boundaries
- Inspect intermediate formats
- Profile communication overhead

```bash
# Debug a pipeline by tapping between stages
cat input.tsv \
    | awk -f filter.awk \
    | tee /dev/stderr \      # See intermediate data
    | python3 transform.py
```

## Non-Goals (Initially)

These are valuable but deferred to Phase 6:

1. **Distributed transactions** - Too complex for v1
2. **Automatic load balancing** - Requires orchestration
3. **Service discovery** - Use explicit configuration first
4. **Authentication/encryption** - Layer on top later

## Use Cases

### Use Case 1: ETL Pipeline

```
Bash (curl) → AWK (filter) → Python (transform) → SQL (load)
     ↓              ↓               ↓                ↓
   HTTP          Pipe            Pipe            DB conn
```

**Why these targets?**
- Bash: Best for HTTP fetching with curl
- AWK: Fastest for simple filtering
- Python: pandas for complex transforms
- SQL: Native database operations

### Use Case 2: .NET Integration

```
PowerShell (orchestration)
     ↓ (in-process)
C# (business logic)
     ↓ (in-process)
IronPython (ML model)
```

**Why in-process?**
- Zero serialization overhead
- Direct object sharing
- Single process to manage

### Use Case 3: High-Performance Pipeline

```
Go (parallel parse) → Rust (transform) → Go (aggregate)
        ↓                    ↓                 ↓
   8 goroutines         SIMD ops          merge results
```

**Why native targets?**
- Compiled for maximum speed
- Memory-safe (Rust)
- Excellent concurrency (Go)

### Use Case 4: Microservice Boundary

```
Go Service A ←──HTTP/JSON──→ Rust Service B
      ↓                            ↓
   Local DB                   Local Cache
```

**Why network?**
- Independent deployment
- Scalability
- Fault isolation

## Chapter Summary

The five core principles:

1. **Location Transparency** - Logic doesn't care where it runs
2. **Sensible Defaults** - Works out of the box
3. **Runtime Family Affinity** - Same family = in-process
4. **Format Negotiation** - Automatic, overridable
5. **Streaming by Default** - Memory efficient, parallel

These principles guide every API design decision in the cross-target glue system.

## Next Steps

In Chapter 3, we'll dive into the target registry and mapping system:
- How targets are registered and queried
- Declaring predicate-target mappings
- Resolution rules for location and transport

## Exercises

1. **Default prediction**: Given `csharp` calling `powershell`, what's the default location? Transport?

2. **Override decision**: You have a Python script using numpy. Should it use IronPython or CPython? Why?

3. **Format choice**: Design a pipeline for log analysis. Which stages use TSV? Which need JSON?

4. **Streaming design**: You need to process a 1TB file. Design a pipeline that never loads more than 1GB in memory.

## Advanced Example: Graph Reachability with Location Transparency

This example demonstrates the **Location Transparency** principle using a graph reachability problem. The same logic works whether stages run locally or remotely.

### The Problem

Given a directed graph, find all nodes reachable from a starting node. This is the classic transitive closure problem.

### The Complete Pipeline

```prolog
% reachability.pl
:- use_module('src/unifyweaver/glue/shell_glue').

% Pipeline that computes reachable nodes
% The logic is identical regardless of where stages execute
reachability_pipeline(Script) :-
    generate_pipeline(
        [
            % Stage 1: Parse edge list
            step(parse_edges, awk, '
                BEGIN { FS="[[:space:]]+" }
                NF >= 2 { print $1 "\t" $2 }
            ', []),

            % Stage 2: Compute reachability (streaming BFS)
            step(compute_reach, python, '
import sys

# Read all edges first
edges = {}
for line in sys.stdin:
    parts = line.strip().split("\\t")
    if len(parts) >= 2:
        src, dst = parts[0], parts[1]
        edges.setdefault(src, []).append(dst)

# BFS from "start" node
def reachable_from(start):
    visited = set()
    queue = [start]
    while queue:
        node = queue.pop(0)
        if node not in visited:
            visited.add(node)
            queue.extend(edges.get(node, []))
    return visited

# Output reachable pairs
start = "a"  # Starting node
for node in reachable_from(start):
    print(f"{start}\\t{node}")
', []),

            % Stage 3: Format as readable output
            step(format, awk, '{print "From " $1 " can reach: " $2}', [])
        ],
        [input('graph.txt')],
        Script
    ).
```

### Sample Input (`graph.txt`)

```
a b
a c
b d
c d
d e
e f
b e
```

### Expected Output

```
From a can reach: a
From a can reach: b
From a can reach: c
From a can reach: d
From a can reach: e
From a can reach: f
```

### Demonstrating Location Transparency

The key insight is that this pipeline works identically in three configurations:

**Configuration 1: All Local (Pipes)**
```prolog
% Default - everything runs locally via pipes
reachability_pipeline(Script).
```

**Configuration 2: Remote Compute (Hypothetical)**
```prolog
% Move heavy computation to a remote server
:- declare_location(compute_reach/2, [
    host('compute.cluster.local'),
    transport(http)
]).
```

**Configuration 3: Distributed (Hypothetical)**
```prolog
% Each stage on different machines
:- declare_location(parse_edges/2, [host('parser.local')]).
:- declare_location(compute_reach/2, [host('compute.local')]).
:- declare_location(format/2, [host('frontend.local')]).
```

**The predicate logic doesn't change** - only the deployment configuration.

### How This Demonstrates the Principles

| Principle | How It's Demonstrated |
|-----------|----------------------|
| **Location Transparency** | Same logic works locally or distributed |
| **Sensible Defaults** | Pipes work without configuration |
| **Runtime Family Affinity** | AWK and Python use pipes (shell family) |
| **Format Negotiation** | TSV flows between all stages |
| **Streaming** | Each stage processes line-by-line |

### Modification Exercise

**Task**: Modify the pipeline to accept the **starting node as a parameter** instead of hardcoding "a".

**Hints**:
- The AWK parse stage can pass through a header line specifying the start node
- Or use an environment variable: `os.environ.get("START_NODE", "a")`
- The pipeline options can include `[env([start_node("a")])]`

**Challenge Extension**: Modify the Python stage to output the **shortest path length** to each reachable node, demonstrating how you'd add new fields to the TSV protocol.

**Expected output format after modification**:
```
From a can reach b in 1 steps
From a can reach c in 1 steps
From a can reach d in 2 steps
From a can reach e in 2 steps
From a can reach f in 3 steps
```

## Further Reading

- Location Transparency: https://en.wikipedia.org/wiki/Location_transparency
- Pipes and Filters: https://www.enterpriseintegrationpatterns.com/patterns/messaging/PipesAndFilters.html
- Backpressure: https://mechanical-sympathy.blogspot.com/
