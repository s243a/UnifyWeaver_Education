<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 1: Introduction to Cross-Target Communication

## Why Cross-Target Glue?

UnifyWeaver compiles Prolog predicates to multiple target languages. Each target has strengths:

| Target | Strengths |
|--------|-----------|
| AWK | Fast text processing, simple transformations |
| Python | Rich libraries (pandas, numpy), data science |
| Bash | System orchestration, file operations |
| Go | High performance, concurrency, single binary |
| Rust | Memory safety, zero-cost abstractions |
| C# | .NET ecosystem, enterprise integration |
| PowerShell | Windows administration, .NET access |
| SQL | Database queries, set operations |

Real-world problems often require combining these strengths. A data pipeline might:

1. **Fetch data** with Bash/curl
2. **Filter records** with AWK (fast, streaming)
3. **Transform data** with Python (pandas, complex logic)
4. **Store results** in SQL

Without cross-target glue, you'd manually:
- Write serialization code in each language
- Handle process management
- Debug format mismatches
- Maintain multiple versions

Cross-target glue automates all of this.

## The Unix Philosophy Extended

The Unix philosophy teaches us:

> "Write programs that do one thing and do it well. Write programs to work together."

Cross-target glue extends this philosophy to:
- **Multiple languages** - not just shell tools
- **Multiple machines** - distributed systems
- **Multiple paradigms** - functional, imperative, declarative

```
Traditional Unix:    grep | sort | uniq | wc
Cross-target glue:   awk_filter | python_transform | go_aggregate | sql_store
```

## Location and Transport Model

Cross-target glue separates two concerns:

### Location (Where it runs)

```
in_process      → Same runtime (e.g., C# calling PowerShell)
local_process   → Separate process, same machine (pipes)
remote(Host)    → Different machine (network)
```

### Transport (How they communicate)

```
direct          → Function call (in-process only)
pipe            → Unix pipes with TSV/JSON
socket          → TCP streaming
http            → REST API calls
```

This separation is key: **location is a deployment concern, not a logic concern**.

Your predicate logic doesn't change based on where it runs:

```prolog
% This logic is location-agnostic
process_data(Input, Output) :-
    fetch(Input, Raw),
    transform(Raw, Processed),
    store(Processed, Output).

% Deployment specifies location
:- declare_target(fetch/2, bash).
:- declare_target(transform/2, python).
:- declare_target(store/2, sql).
:- declare_location(store/2, [host('db.example.com')]).
```

## Runtime Families

Targets are grouped into runtime families. Targets in the same family can communicate more efficiently:

| Family | Targets | Default Communication |
|--------|---------|----------------------|
| .NET | C#, PowerShell, IronPython | In-process (zero serialization) |
| JVM | Java, Scala, Jython | In-process |
| Shell | Bash, AWK, sed | Pipes |
| Native | Go, Rust, C | Pipes or shared memory |
| Python | CPython | Pipes |

**Key insight**: C# calling PowerShell can pass objects directly (in-process). AWK calling Python must serialize data (pipes).

## Quick Start Example

Let's build a simple log analysis pipeline:

```prolog
% log_pipeline.pl
:- use_module('src/unifyweaver/glue/shell_glue').

% Generate a 3-stage pipeline
example_pipeline(Script) :-
    generate_pipeline(
        [
            step(filter, awk, 'filter.awk', []),
            step(analyze, python, 'analyze.py', []),
            step(report, awk, 'report.awk', [])
        ],
        [input('access.log')],
        Script
    ).
```

Run it:

```bash
$ swipl -l log_pipeline.pl -g "example_pipeline(S), write(S)" -t halt
```

Generated output:

```bash
#!/bin/bash
set -euo pipefail

cat "access.log" \
    | awk -f "filter.awk" \
    | python3 "analyze.py" \
    | awk -f "report.awk"
```

The glue system:
1. Generated the orchestration script
2. Connected stages with pipes
3. Set up proper error handling (`set -euo pipefail`)

## What Cross-Target Glue Provides

### 1. Automatic Code Generation

Generate complete, working scripts:

```prolog
generate_awk_script(Logic, Fields, Options, Script).
generate_python_script(Logic, Fields, Options, Script).
generate_go_pipe_main(Logic, Options, Code).
generate_rust_pipe_main(Logic, Options, Code).
```

### 2. Format Handling

Automatic serialization/deserialization:

```prolog
% TSV (default) - fast, simple
generate_pipeline(Steps, [format(tsv)], Script).

% JSON - structured, self-describing
generate_pipeline(Steps, [format(json)], Script).
```

### 3. In-Process Communication

For .NET family, avoid serialization entirely:

```prolog
% C# hosting PowerShell in-process
generate_powershell_bridge(Options, CSharpCode).

% Result: direct object passing, no pipes
```

### 4. Network Communication

For distributed systems:

```prolog
% Generate HTTP server
generate_go_http_server(Endpoints, Options, ServerCode).

% Generate HTTP client
generate_python_http_client(Services, Options, ClientCode).
```

### 5. Binary Orchestration

For high-performance native code:

```prolog
% Generate Go with parallel workers
generate_go_pipe_main(Logic, [parallel(8)], Code).

% Cross-compile for multiple platforms
generate_cross_compile(go, Source, [linux-amd64, darwin-arm64], Script).
```

## The Communication Flow

Here's how data flows through a typical cross-target pipeline:

```
┌──────────┐    TSV     ┌──────────┐    TSV     ┌──────────┐
│   AWK    │ ────────►  │  Python  │ ────────►  │   Go     │
│ (filter) │   pipe     │(transform)│   pipe    │(aggregate)│
└──────────┘            └──────────┘            └──────────┘
     │                       │                       │
     └───────────────────────┴───────────────────────┘
                             │
                    Orchestrator (Bash)
```

Each stage:
1. Reads from stdin
2. Parses the agreed format (TSV/JSON)
3. Processes records
4. Writes to stdout in the same format

The orchestrator connects them with pipes.

## Chapter Summary

- Cross-target glue enables multi-language composition
- Location (where) and transport (how) are separate concerns
- Runtime families determine optimal communication
- The system generates complete, working code
- You focus on logic, not plumbing

## Next Steps

In Chapter 2, we'll explore the design philosophy in depth:
- Location transparency principles
- How defaults are chosen
- When to override defaults
- Design patterns for hybrid systems

## Exercises

1. **Identify the pipeline**: Think of a data processing task you do regularly. What languages would be optimal for each stage?

2. **Family grouping**: For a C# application that needs Python ML models, which runtime should IronPython use? When would CPython be better?

3. **Format choice**: When would you choose JSON over TSV? When is TSV better?

## Advanced Example: Family Tree Pipeline

This complete example demonstrates a pipeline that processes family relationships - a classic Prolog problem. The pipeline reads parent-child data, computes ancestors, and outputs the results.

### The Complete Pipeline

```prolog
% family_pipeline.pl
:- use_module('src/unifyweaver/glue/shell_glue').

% Generate a pipeline that finds all ancestors
family_pipeline(Script) :-
    generate_pipeline(
        [
            % Stage 1: Parse family data (AWK - fast text parsing)
            step(parse, awk, '{print $1 "\t" $2}', [
                separator(" ")
            ]),

            % Stage 2: Compute transitive closure (Python - recursion)
            step(ancestors, python, '
# Build parent dict and find all ancestors
parents = {}
for line in open("/dev/stdin"):
    child, parent = line.strip().split("\\t")
    parents.setdefault(child, []).append(parent)

def ancestors(person, seen=None):
    if seen is None:
        seen = set()
    for p in parents.get(person, []):
        if p not in seen:
            seen.add(p)
            ancestors(p, seen)
    return seen

# Output all ancestor relationships
for child in parents:
    for anc in ancestors(child):
        print(f"{child}\\t{anc}")
', []),

            % Stage 3: Format output (AWK - simple formatting)
            step(format, awk, '{print $1 " is descended from " $2}', [])
        ],
        [input('family.txt')],
        Script
    ).
```

### Sample Input (`family.txt`)

```
alice bob
alice carol
bob david
bob eve
carol frank
david george
```

### Running the Pipeline

```bash
$ swipl -l family_pipeline.pl -g "family_pipeline(S), write(S)" -t halt > run.sh
$ chmod +x run.sh
$ ./run.sh
```

### Expected Output

```
alice is descended from bob
alice is descended from carol
alice is descended from david
alice is descended from eve
alice is descended from frank
alice is descended from george
bob is descended from david
bob is descended from eve
bob is descended from george
carol is descended from frank
david is descended from george
```

### How It Works

1. **AWK parse stage**: Reads space-separated "child parent" pairs, outputs TSV
2. **Python ancestors stage**: Builds a graph, computes transitive closure via recursion
3. **AWK format stage**: Converts TSV back to human-readable output

This mirrors the classic Prolog ancestor relation:

```prolog
% Traditional Prolog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

But runs as a streaming pipeline!

### Modification Exercise

**Task**: Modify the pipeline to also compute the **generation distance** between each person and their ancestors.

**Hints**:
- The Python stage needs to track depth during recursion
- Output format becomes: `child \t ancestor \t distance`
- The AWK format stage needs a third field: `$1 " is " $3 " generations from " $2`

**Expected output after modification**:
```
alice is 1 generations from bob
alice is 1 generations from carol
alice is 2 generations from david
alice is 2 generations from eve
alice is 2 generations from frank
alice is 3 generations from george
```

## Further Reading

- Unix Philosophy: https://en.wikipedia.org/wiki/Unix_philosophy
- Pipes and Filters Pattern: https://www.enterpriseintegrationpatterns.com/patterns/messaging/PipesAndFilters.html
- Design documentation: `docs/design/cross-target-glue/01-philosophy.md`
