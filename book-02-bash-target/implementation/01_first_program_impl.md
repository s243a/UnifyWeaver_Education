<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 1: Your First Program - Implementation Details

This document provides function-level documentation for the Bash target compilation workflow.

**Source**: `src/unifyweaver/core/recursive_compiler.pl`, `src/unifyweaver/core/stream_compiler.pl`

---

## compile_recursive/3

Compiles a recursive Prolog predicate to Bash code with fixpoint evaluation.

### Signature

```prolog
compile_recursive(+Predicate/Arity, +Options, -BashCode)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicate/Arity` | `atom/integer` | The recursive predicate to compile |
| `Options` | `list` | Compilation options |
| `BashCode` | `string` | Generated Bash script |

### Algorithm

1. **Collect clauses**: Gather all clauses for `Predicate/Arity`
2. **Analyze recursion**: Identify base case and recursive case
3. **Generate fixpoint loop**: Create Bash while loop with visited tracking
4. **Generate rule application**: Create functions for each clause

### Example

```prolog
% Define recursive predicate
ancestor(A, D) :- parent(A, D).
ancestor(A, D) :- parent(A, P), ancestor(P, D).

% Compile
?- compile_recursive(ancestor/2, [], BashCode).
```

### Generated Bash Structure

```bash
#!/bin/bash
declare -A _visited_ancestor

ancestor() {
    local arg1="$1"
    local arg2="$2"
    local key="${arg1}:${arg2}"

    # Base case: check visited
    [[ -n "${_visited_ancestor[$key]}" ]] && return 0
    _visited_ancestor[$key]=1

    # Apply rules
    _ancestor_rule_1 "$arg1" "$arg2"
    _ancestor_rule_2 "$arg1" "$arg2"
}

_ancestor_rule_1() {
    # Base: ancestor(A, D) :- parent(A, D)
    parent "$1" "$2" && echo "$1:$2"
}

_ancestor_rule_2() {
    # Recursive: ancestor(A, D) :- parent(A, P), ancestor(P, D)
    while IFS=: read -r a p; do
        ancestor "$p" "$2"
    done < <(parent "$1")
}
```

### Options

| Option | Values | Default | Description |
|--------|--------|---------|-------------|
| `dedup(Mode)` | `sort_u`, `hash`, `none` | `hash` | Deduplication strategy |
| `output_format(F)` | `colon`, `tab`, `json` | `colon` | Output field separator |

---

## compile_facts/4

Compiles Prolog facts to a Bash lookup function.

### Signature

```prolog
stream_compiler:compile_facts(+Predicate, +Arity, +Options, -BashCode)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicate` | `atom` | The fact predicate name |
| `Arity` | `integer` | Number of arguments |
| `Options` | `list` | Compilation options |
| `BashCode` | `string` | Generated Bash script |

### Algorithm

1. **Collect facts**: Query all `Predicate/Arity` facts from database
2. **Generate associative array**: Create `declare -A` with key-value pairs
3. **Generate lookup function**: Create function for querying facts

### Example

```prolog
% Define facts
parent(abraham, ishmael).
parent(abraham, isaac).
parent(sarah, isaac).

% Compile
?- stream_compiler:compile_facts(parent, 2, [], BashCode).
```

### Generated Bash

```bash
#!/bin/bash
declare -A _facts_parent=(
    ["abraham:ishmael"]=1
    ["abraham:isaac"]=1
    ["sarah:isaac"]=1
)

parent() {
    local arg1="$1"
    local arg2="$2"

    if [[ -n "$arg1" && -n "$arg2" ]]; then
        # Both args: membership test
        [[ -n "${_facts_parent[$arg1:$arg2]}" ]]
    elif [[ -n "$arg1" ]]; then
        # First arg only: find matching pairs
        for key in "${!_facts_parent[@]}"; do
            [[ "$key" == "$arg1:"* ]] && echo "$key"
        done
    else
        # No args: enumerate all
        for key in "${!_facts_parent[@]}"; do
            echo "$key"
        done
    fi
}
```

### Query Modes

| Arguments | Behavior | Example |
|-----------|----------|---------|
| Both | Membership test (returns 0/1) | `parent abraham isaac && echo "yes"` |
| First only | Stream matching pairs | `parent abraham` → `abraham:ishmael`, `abraham:isaac` |
| None | Stream all facts | `parent` → all pairs |

---

## Compilation Workflow

### Complete End-to-End Process

```
┌─────────────────────────────┐
│  1. Initialize Environment  │
│  ['education/init']         │
└──────────────┬──────────────┘
               │
               ▼
┌─────────────────────────────┐
│  2. Load Compiler Modules   │
│  use_module(recursive_...)  │
│  use_module(stream_...)     │
└──────────────┬──────────────┘
               │
               ▼
┌─────────────────────────────┐
│  3. Load User Predicates    │
│  ['family_tree.pl']         │
└──────────────┬──────────────┘
               │
               ▼
┌─────────────────────────────┐
│  4. Compile Facts           │
│  compile_facts(parent, 2..) │
└──────────────┬──────────────┘
               │
               ▼
┌─────────────────────────────┐
│  5. Compile Recursive Rules │
│  compile_recursive(anc...)  │
└──────────────┬──────────────┘
               │
               ▼
┌─────────────────────────────┐
│  6. Save to Files           │
│  open(..., write, Stream)   │
└──────────────┬──────────────┘
               │
               ▼
┌─────────────────────────────┐
│  7. Execute in Bash         │
│  source parent.sh           │
│  source ancestor.sh         │
│  ancestor abraham           │
└─────────────────────────────┘
```

### Session Commands

```prolog
% Step 1: Initialize
?- ['education/init'].

% Step 2: Load compilers
?- use_module(unifyweaver(core/recursive_compiler)).
?- use_module(unifyweaver(core/stream_compiler)).

% Step 3: Load predicates
?- ['education/family_tree'].

% Step 4: Compile and save facts
?- stream_compiler:compile_facts(parent, 2, [], Code),
   open('output/parent.sh', write, S),
   write(S, Code),
   close(S).

% Step 5: Compile and save recursive
?- compile_recursive(ancestor/2, [], Code),
   open('output/ancestor.sh', write, S),
   write(S, Code),
   close(S).
```

---

## Dependency Handling

### Fact Dependencies

Recursive predicates depend on base facts. The `ancestor/2` predicate depends on `parent/2`:

```prolog
ancestor(A, D) :- parent(A, D).      % Uses parent
ancestor(A, D) :- parent(A, P), ...  % Uses parent
```

### Sourcing Order

Scripts must be sourced in dependency order:

```bash
# CORRECT: Base facts first
source parent.sh
source ancestor.sh
ancestor abraham

# WRONG: Missing dependency
source ancestor.sh
ancestor abraham  # Error: parent: command not found
```

---

## Visited Tracking

### Purpose

Recursive predicates use visited tracking to:
1. **Prevent infinite loops** - Don't re-process same arguments
2. **Implement memoization** - Cache computed results
3. **Ensure termination** - Fixpoint when no new facts

### Implementation

```bash
declare -A _visited_ancestor

ancestor() {
    local key="${1}:${2}"

    # Skip if already visited
    [[ -n "${_visited_ancestor[$key]}" ]] && return 0

    # Mark as visited
    _visited_ancestor[$key]=1

    # Continue with rule application...
}
```

### Clearing State

For multiple queries, clear the visited array:

```bash
source ancestor.sh

ancestor abraham  # Computes all
unset _visited_ancestor
declare -A _visited_ancestor
ancestor isaac    # Fresh computation
```

---

## Troubleshooting

### Unknown Procedure Error

```prolog
?- compile_recursive(my_pred/2, [], Code).
ERROR: Unknown procedure: my_pred/2
```

**Cause**: Predicate not loaded or wrong arity.

**Fix**:
```prolog
?- listing(my_pred).  % Check what's loaded
?- ['my_predicates.pl'].  % Load file
```

### Source Sink Error

```prolog
?- use_module(unifyweaver(core/recursive_compiler)).
ERROR: source_sink `unifyweaver(...)' does not exist
```

**Cause**: Library path not initialized.

**Fix**:
```prolog
?- ['education/init'].  % Load init first
```

### Empty Output

**Possible causes**:
1. Facts not loaded before compilation
2. Base fact script not sourced
3. Wrong predicate arity

**Fix**: Ensure correct sourcing order:
```bash
source parent.sh   # Base facts FIRST
source ancestor.sh
ancestor abraham
```

---

## Related Documentation

- [Book 2 Chapter 2: Stream Compilation](../02_stream_compilation.md)
- [Recursive Compiler Source](../../../../src/unifyweaver/core/recursive_compiler.pl)
- [Stream Compiler Source](../../../../src/unifyweaver/core/stream_compiler.pl)
