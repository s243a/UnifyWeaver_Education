<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 1 Implementation: Your First UnifyWeaver Program

**Detailed function documentation for RAG systems**

This document provides implementation details for the compilation workflow demonstrated in the first program tutorial.

---

## Table of Contents

1. [Compilation Workflow](#compilation-workflow)
2. [compile_recursive/3](#compile_recursive3)
3. [compile_facts/4](#compile_facts4)
4. [Generated Script Structure](#generated-script-structure)
5. [Test Runner Generation](#test-runner-generation)

---

## Compilation Workflow

The complete workflow from Prolog to executable Bash:

```
1. Load init.pl        → Set up module paths
2. Load family_tree.pl → Load facts and rules
3. compile_facts       → Generate base fact script
4. compile_recursive   → Generate recursive rule script
5. Save to files       → Write .sh files
6. Source and run      → Execute generated scripts
```

### Step-by-Step Commands

```prolog
% 1. Initialize environment
?- ['education/init'].

% 2. Load compilers
?- use_module(unifyweaver(core/recursive_compiler)).
?- use_module(unifyweaver(core/stream_compiler)).

% 3. Load predicates
?- ['education/family_tree'].

% 4. Compile facts
?- stream_compiler:compile_facts(parent, 2, [], BashCode).

% 5. Compile recursive rule
?- compile_recursive(ancestor/2, [], BashCode).

% 6. Save to file
?- open('output/script.sh', write, S), write(S, Code), close(S).
```

---

## compile_recursive/3

```prolog
compile_recursive(+Pred/Arity, +Options, -BashCode)
```

**Purpose**: Compiles a recursive predicate to optimized Bash code.

**Parameters**:

| Parameter | Type | Description |
|-----------|------|-------------|
| `Pred/Arity` | `atom/integer` | Predicate indicator (e.g., `ancestor/2`) |
| `Options` | `list` | Compilation options |
| `BashCode` | `string` | Generated Bash code |

**Options**:

| Option | Values | Default | Description |
|--------|--------|---------|-------------|
| `unique(Bool)` | `true`, `false` | `true` | Deduplicate results |
| `unordered(Bool)` | `true`, `false` | `true` | Allow reordering |

**Example**:

```prolog
?- compile_recursive(ancestor/2, [], Code).
% Code contains complete BFS implementation

?- compile_recursive(ancestor/2, [unique(false)], Code).
% Code without deduplication
```

**Algorithm**:

1. Classify predicate pattern (tail, linear, tree, mutual recursion)
2. Fetch constraints from `constraint_analyzer`
3. Select appropriate template
4. Render template with predicate details
5. Return complete Bash script

---

## compile_facts/4

```prolog
stream_compiler:compile_facts(+Name, +Arity, +Options, -BashCode)
```

**Purpose**: Compiles fact predicates to Bash associative arrays.

**Parameters**:

| Parameter | Type | Description |
|-----------|------|-------------|
| `Name` | `atom` | Predicate name |
| `Arity` | `integer` | Predicate arity |
| `Options` | `list` | Compilation options |
| `BashCode` | `string` | Generated Bash code |

**Generated Functions**:

| Function | Purpose |
|----------|---------|
| `{name}_data` | Associative array holding facts |
| `{name}()` | Point lookup function |
| `{name}_stream()` | Stream all facts |

**Example**:

```prolog
?- stream_compiler:compile_facts(parent, 2, [], Code).
```

**Generated Bash**:

```bash
declare -A parent_data=(
    ["abraham:ishmael"]=1
    ["abraham:isaac"]=1
    ["sarah:isaac"]=1
)

parent() {
    local key="$1:$2"
    [[ -n "${parent_data[$key]}" ]] && echo "$key"
}

parent_stream() {
    for key in "${!parent_data[@]}"; do
        echo "$key"
    done
}
```

---

## Generated Script Structure

### Facts Script (`parent.sh`)

```bash
#!/bin/bash
# parent - fact lookup

# Data storage
declare -A parent_data=(
    ["abraham:ishmael"]=1
    ["abraham:isaac"]=1
    # ...
)

# Point lookup
parent() {
    local key="$1:$2"
    [[ -n "${parent_data[$key]}" ]] && echo "$key"
}

# Stream all facts
parent_stream() {
    for key in "${!parent_data[@]}"; do
        echo "$key"
    done
}
```

### Recursive Script (`ancestor.sh`)

```bash
#!/bin/bash
# ancestor - transitive closure of parent

# Dependency lookup
parent_get_stream() {
    if declare -f parent_stream >/dev/null 2>&1; then
        parent_stream
    elif declare -f parent >/dev/null 2>&1; then
        parent
    else
        echo "Error: parent not found" >&2
        return 1
    fi
}

# BFS implementation
ancestor_all() {
    local start="$1"
    declare -A visited
    local queue_file="/tmp/ancestor_queue_$$"
    local next_queue="/tmp/ancestor_next_$$"

    trap "rm -f $queue_file $next_queue" EXIT

    echo "$start" > "$queue_file"
    visited["$start"]=1

    while [[ -s "$queue_file" ]]; do
        > "$next_queue"
        while IFS= read -r current; do
            while IFS=":" read -r from to; do
                if [[ "$from" == "$current" && -z "${visited[$to]}" ]]; then
                    visited["$to"]=1
                    echo "$to" >> "$next_queue"
                    echo "$start:$to"
                fi
            done < <(parent_get_stream | grep "^$current:")
        done < "$queue_file"
        mv "$next_queue" "$queue_file"
    done
}

# Check specific relationship
ancestor_check() {
    local start="$1"
    local target="$2"
    ancestor_all "$start" | grep -q "^$start:$target$"
}

# Main entry point
ancestor() {
    local start="$1"
    local target="$2"

    if [[ -z "$target" ]]; then
        ancestor_all "$start" | sort -u
    else
        ancestor_check "$start" "$target" && echo "$start:$target"
    fi
}
```

---

## Test Runner Generation

### generate_test_runner_inferred/2

```prolog
generate_test_runner_inferred(+OutputFile, +Options)
```

**Purpose**: Automatically generates test cases by analyzing script signatures.

**Process**:

1. Scan output directory for `.sh` files
2. Extract function signatures
3. Infer test cases from signatures
4. Generate test runner script

**Example**:

```prolog
?- use_module('src/unifyweaver/core/advanced/test_runner_inference').
?- generate_test_runner_inferred('output/test_runner.sh', [output_dir('output')]).
```

**Generated Test Runner**:

```bash
#!/bin/bash
# Auto-generated test runner

source parent.sh
source ancestor.sh

echo "=== Running inferred tests ==="

# Test parent_stream
echo -n "Testing parent_stream... "
result=$(parent_stream | head -1)
[[ -n "$result" ]] && echo "PASS" || echo "FAIL"

# Test ancestor
echo -n "Testing ancestor abraham... "
result=$(ancestor abraham | head -3)
[[ -n "$result" ]] && echo "PASS" || echo "FAIL"

echo "=== Tests complete ==="
```

---

## Script Dependencies

Generated recursive scripts depend on base fact scripts:

```
ancestor.sh
    └── depends on → parent.sh
```

**Loading order matters**:

```bash
# Correct order
source parent.sh
source ancestor.sh

# Wrong order - will fail
source ancestor.sh  # ERROR: parent_stream not found
source parent.sh
```

---

## Troubleshooting

### "Unknown procedure" Error

```prolog
?- compile_recursive(my_pred/2, [], Code).
ERROR: Unknown procedure: my_pred/2
```

**Fix**: Load your predicate file first:
```prolog
?- ['my_predicates.pl'].
```

### "source_sink does not exist" Error

```prolog
?- use_module(unifyweaver(core/recursive_compiler)).
ERROR: source_sink `unifyweaver(...)' does not exist
```

**Fix**: Load init.pl first:
```prolog
?- ['education/init'].
```

### Empty Output from Script

**Possible causes**:
1. Base facts not loaded
2. Wrong source order
3. Predicate has no facts

**Debug**:
```bash
# Check if functions exist
declare -f parent_stream
declare -f ancestor_all

# Test base facts
parent_stream | head -5
```

---

## Source Files

- `src/unifyweaver/core/recursive_compiler.pl`
- `src/unifyweaver/core/stream_compiler.pl`
- `src/unifyweaver/core/advanced/test_runner_inference.pl`

## See Also

- Chapter 1: Your First Program (tutorial)
- Chapter 2: Stream Compilation (non-recursive predicates)
- Chapter 6: Advanced Recursion (recursion patterns)
