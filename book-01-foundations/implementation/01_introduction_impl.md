<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 1 Implementation: Introduction to UnifyWeaver

**Detailed function documentation for RAG systems**

This document provides implementation details for setting up and understanding UnifyWeaver's core concepts.

---

## Table of Contents

1. [Installation](#installation)
2. [Project Structure](#project-structure)
3. [Module Loading System](#module-loading-system)
4. [Target Categories](#target-categories)
5. [Compilation Approaches](#compilation-approaches)

---

## Installation

### SWI-Prolog Installation

UnifyWeaver requires SWI-Prolog 8.0 or later.

**Ubuntu/Debian**:
```bash
sudo apt-get update && sudo apt-get install swi-prolog
```

**Arch Linux**:
```bash
sudo pacman -S swi-prolog
```

**macOS (Homebrew)**:
```bash
brew install swi-prolog
```

**Windows (winget)**:
```bash
winget install SWI-Prolog.SWI-Prolog
```

**Termux (Android)**:
```bash
pkg install swi-prolog
```

### Version Verification

```bash
swipl --version
# Should output: SWI-Prolog version 8.x.x or higher
```

### Repository Setup

```bash
# Clone main repository
git clone https://github.com/s243a/UnifyWeaver.git
cd UnifyWeaver

# Clone education repository (optional)
git clone https://github.com/s243a/UnifyWeaver_Education.git education
```

---

## Project Structure

### Main Repository Layout

```
UnifyWeaver/
├── init.pl                 # Module path initialization
├── src/
│   └── unifyweaver/
│       ├── core/           # Core compilation modules
│       │   ├── compiler_driver.pl
│       │   ├── constraint_analyzer.pl
│       │   ├── recursive_compiler.pl
│       │   └── template_system.pl
│       ├── targets/        # Target-specific generators
│       │   ├── bash_target.pl
│       │   ├── python_target.pl
│       │   ├── go_target.pl
│       │   └── ...
│       └── incremental/    # Caching system
│           └── incremental_compiler.pl
├── templates/              # Code generation templates
└── tests/                  # Test suites
```

### Key Directories

| Directory | Purpose |
|-----------|---------|
| `src/unifyweaver/core/` | Core compilation logic |
| `src/unifyweaver/targets/` | Language-specific code generators |
| `src/unifyweaver/incremental/` | Caching and incremental compilation |
| `templates/` | External template files |
| `education/` | Tutorial and documentation (separate repo) |

---

## Module Loading System

### init.pl

The `init.pl` file sets up module search paths using SWI-Prolog's `file_search_path/2`:

```prolog
% init.pl - Sets up the unifyweaver library alias
:- multifile file_search_path/2.
:- dynamic file_search_path/2.

% Add src directory to search path
file_search_path(unifyweaver, 'src/unifyweaver').
```

### Starting UnifyWeaver

```bash
# Start with module paths configured
swipl -f init.pl

# Or load init.pl manually
swipl
?- [init].
```

### Loading Modules

After `init.pl`, use the `unifyweaver(...)` alias:

```prolog
% Load core modules
?- use_module(unifyweaver(core/compiler_driver)).
?- use_module(unifyweaver(core/recursive_compiler)).

% Load target-specific modules
?- use_module(unifyweaver(targets/bash_target)).
?- use_module(unifyweaver(targets/python_target)).
```

### Module Path Resolution

The alias system resolves paths:

```
unifyweaver(core/recursive_compiler)
→ 'src/unifyweaver/core/recursive_compiler.pl'

unifyweaver(targets/bash_target)
→ 'src/unifyweaver/targets/bash_target.pl'
```

---

## Target Categories

UnifyWeaver supports multiple compilation targets organized by category.

### Shell/Scripting Targets

| Target | Use Case | Module |
|--------|----------|--------|
| Bash | Unix shell scripts | `bash_target` |
| PowerShell | Windows automation | `powershell_target` |
| AWK | Text processing | `awk_target` |

### Systems Languages

| Target | Use Case | Module |
|--------|----------|--------|
| Go | Concurrent systems | `go_target` |
| Rust | Memory-safe systems | `rust_target` |
| LLVM | Low-level compilation | `llvm_target` |
| WASM | Browser/edge | `wasm_target` |

### .NET Ecosystem

| Target | Use Case | Module |
|--------|----------|--------|
| C# (Stream) | Streaming pipelines | `csharp_target` |
| C# (Query) | Complex recursion | `csharp_query_target` |
| F# | Functional .NET | `fsharp_target` |

### Dynamic Languages

| Target | Use Case | Module |
|--------|----------|--------|
| Python | General scripting | `python_target` |
| TypeScript | Web development | `typescript_target` |

### Data Targets

| Target | Use Case | Module |
|--------|----------|--------|
| SQL | Database queries | `sql_target` |
| Prolog | Logic execution | `prolog_target` |

---

## Compilation Approaches

Different targets use different compilation strategies based on their runtime characteristics.

### Stream-Based Pipelines

**Targets**: Bash, Go, Rust, AWK

**Approach**: Data flows through Unix-style pipelines with filtering and transformation.

```
Input Stream → Filter → Transform → Filter → Output Stream
```

**Characteristics**:
- Memory efficient (streaming)
- Composable with Unix tools
- Natural for shell environments

**Example** (Bash):
```bash
parent_stream | grep "^alice:" | cut -d: -f2
```

### Generator/Lazy Evaluation

**Targets**: Python

**Approach**: Uses Python generators for lazy evaluation, computing values on demand.

```python
def ancestors(person):
    for parent in parents(person):
        yield parent
        yield from ancestors(parent)
```

**Characteristics**:
- Memory efficient (lazy)
- Pythonic iteration
- Good for large datasets

### Query Runtime with Semi-Naive Evaluation

**Targets**: C# Query

**Approach**: Uses a runtime library with plan nodes and semi-naive fixpoint evaluation for complex recursion.

**Characteristics**:
- Handles complex recursive patterns
- Optimized for .NET runtime
- Good for enterprise applications

### Declarative SQL

**Targets**: SQL

**Approach**: Generates SQL queries that execute on the database engine.

```sql
WITH RECURSIVE ancestors AS (
    SELECT parent, child FROM family
    UNION
    SELECT a.parent, f.child
    FROM ancestors a JOIN family f ON a.child = f.parent
)
SELECT * FROM ancestors;
```

**Characteristics**:
- Database-optimized execution
- Leverages SQL engine optimizations
- Good for data-heavy workloads

---

## Declarative vs Imperative

### Declarative (Prolog)

Describes **what** you want:

```prolog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

**Focus**: Relationships and rules

### Imperative (Generated Bash)

Describes **how** to compute it:

```bash
ancestor_all() {
    local start="$1"
    declare -A visited
    local queue=("$start")

    while [[ ${#queue[@]} -gt 0 ]]; do
        current="${queue[0]}"
        queue=("${queue[@]:1}")

        for child in $(parent_stream | grep "^$current:" | cut -d: -f2); do
            if [[ -z "${visited[$child]}" ]]; then
                visited[$child]=1
                queue+=("$child")
                echo "$start:$child"
            fi
        done
    done
}
```

**Focus**: Step-by-step execution

### UnifyWeaver's Role

UnifyWeaver bridges these paradigms:

1. **Input**: Declarative Prolog specification
2. **Analysis**: Pattern classification, constraint analysis
3. **Output**: Optimized imperative code in target language

The generated code is correct (matches Prolog semantics) and performant (uses target-appropriate idioms).

---

## Source Files

- `init.pl` - Module path configuration
- `src/unifyweaver/core/compiler_driver.pl` - Main compilation entry point
- `src/unifyweaver/targets/` - Target-specific generators

## See Also

- Chapter 1: Introduction to UnifyWeaver (conceptual overview)
- Chapter 3: UnifyWeaver Architecture (compilation pipeline)
- Book 2: Bash Target (practical examples)
