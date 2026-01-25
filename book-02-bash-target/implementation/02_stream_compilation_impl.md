<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2 Implementation: Stream Compilation

**Detailed function documentation for RAG systems**

This document provides implementation details for stream compilation of non-recursive predicates.

---

## Table of Contents

1. [Stream Compilation Philosophy](#stream-compilation-philosophy)
2. [compile_stream/3](#compile_stream3)
3. [Join Operations](#join-operations)
4. [Outer Joins](#outer-joins)
5. [Multiple Rules (OR)](#multiple-rules-or)
6. [compile_predicate/3](#compile_predicate3)

---

## Stream Compilation Philosophy

The `stream_compiler` treats Prolog logic as a blueprint for Unix-style data pipelines:

```
Data Source → Filter → Transform → Join → Filter → Output
```

**Benefits**:
- Memory efficient (streaming, not buffering)
- Composable with Unix tools
- Natural fit for shell environments

**Applicable to**: Non-recursive predicates (no self-calls).

---

## compile_stream/3

```prolog
compile_stream(+Pred/Arity, +Options, -BashCode)
```

**Purpose**: Compiles non-recursive predicates to streaming Bash pipelines.

**Parameters**:

| Parameter | Type | Description |
|-----------|------|-------------|
| `Pred/Arity` | `atom/integer` | Predicate indicator |
| `Options` | `list` | Compilation options |
| `BashCode` | `string` | Generated Bash code |

**Example**:

```prolog
?- compile_stream(grandparent/2, [], Code).
```

**For the Prolog rule**:
```prolog
grandparent(GP, GC) :-
    parent(GP, P),
    parent(P, GC).
```

**Generated Bash**:

```bash
parent_join() {
    while IFS= read -r input; do
        IFS=":" read -r a b <<< "$input"
        for key in "${!parent_data[@]}"; do
            IFS=":" read -r c d <<< "$key"
            [[ "$b" == "$c" ]] && echo "$a:$d"
        done
    done
}

grandparent() {
    parent_stream | parent_join | sort -u
}
```

---

## Join Operations

### Inner Join

The most common join: combines rows where keys match.

**Prolog**:
```prolog
grandparent(GP, GC) :-
    parent(GP, P),    % First relation
    parent(P, GC).    % Second relation (P joins)
```

**Join Logic**:
```
parent(GP, P) × parent(P, GC) where P matches
```

**Generated Bash**:

```bash
parent_join() {
    while IFS= read -r input; do
        IFS=":" read -r a b <<< "$input"  # a=GP, b=P
        for key in "${!parent_data[@]}"; do
            IFS=":" read -r c d <<< "$key"  # c=Parent, d=Child
            [[ "$b" == "$c" ]] && echo "$a:$d"  # If P matches, output GP:GC
        done
    done
}
```

**Data Flow**:
```
parent_stream       parent_join              Output
---------------     ------------------       ------
abraham:isaac  -->  check all parents  -->   abraham:jacob
                    isaac:jacob matches      abraham:esau
                    output abraham:jacob
```

---

## Outer Joins

### LEFT OUTER JOIN

Returns all left-side records, with matched right-side or `null`.

**Prolog Pattern**:
```prolog
employee_dept(Emp, Dept) :-
    employee(Emp, DeptId),
    (department(DeptId, Dept) ; Dept = null).
```

The `(Goal ; Var = null)` pattern triggers LEFT JOIN detection.

**Generated Bash** (conceptual):

```bash
employee_dept() {
    declare -A dept_lookup
    # Build hashtable for right side
    for key in "${!department_data[@]}"; do
        IFS=":" read -r id name <<< "$key"
        dept_lookup["$id"]="$name"
    done

    # Stream left side, join with null fallback
    for key in "${!employee_data[@]}"; do
        IFS=":" read -r emp dept_id <<< "$key"
        if [[ -n "${dept_lookup[$dept_id]}" ]]; then
            echo "$emp:${dept_lookup[$dept_id]}"
        else
            echo "$emp:null"
        fi
    done
}
```

### RIGHT OUTER JOIN

Returns all right-side records.

**Prolog Pattern**:
```prolog
dept_employee(Emp, Dept) :-
    (employee(Emp, DeptId) ; Emp = null),
    department(DeptId, Dept).
```

### FULL OUTER JOIN

Returns all records from both sides.

**Prolog Pattern**:
```prolog
full_join(Emp, Dept) :-
    (employee(Emp, DeptId) ; Emp = null),
    (department(DeptId, Dept) ; Dept = null).
```

---

## Multiple Rules (OR)

When a predicate has multiple rules, Prolog treats them as OR:

**Prolog**:
```prolog
child(C) :- parent(M, C), female(M).  % Rule 1: has mother
child(C) :- parent(F, C), male(F).    % Rule 2: has father
```

**Compilation Strategy**:

1. Generate pipeline for each rule
2. Concatenate outputs
3. Apply `sort -u` to merge and deduplicate

**Generated Bash**:

```bash
child_stream() {
    {
        # Rule 1: children with mothers
        parent_stream | while IFS=":" read -r p c; do
            female "$p" >/dev/null 2>&1 && echo "$c"
        done

        # Rule 2: children with fathers
        parent_stream | while IFS=":" read -r p c; do
            male "$p" >/dev/null 2>&1 && echo "$c"
        done
    } | sort -u
}
```

**Why `sort -u`?**
- Implements logical OR
- Removes duplicates (same child from both rules)
- Matches Prolog semantics

---

## compile_predicate/3

```prolog
compile_predicate(+Pred/Arity, +Options, -BashCode)
```

**Purpose**: Automatically selects the correct compiler based on predicate analysis.

**Algorithm**:

1. Analyze predicate for recursion
2. If recursive → use `compile_recursive/3`
3. If non-recursive → use `compile_stream/3`

**Example**:

```prolog
% Automatically uses stream_compiler (non-recursive)
?- compile_predicate(grandparent/2, [], Code).

% Automatically uses recursive_compiler (recursive)
?- compile_predicate(ancestor/2, [], Code).
```

**Recommendation**: Use `compile_predicate/3` unless you need specific compiler features.

---

## Variable Scope in Joins

Variables in the `while` loop are local to the pipeline:

```bash
parent_join() {
    while IFS= read -r input; do
        IFS=":" read -r a b <<< "$input"  # a, b are local
        # ...
    done
}
```

**Safe because**: We're transforming each line, not accumulating state.

**See Chapter 4** for cases where variable scope matters.

---

## Generated Function Signatures

For a predicate `foo/2`:

| Function | Purpose |
|----------|---------|
| `foo()` | Main entry point |
| `foo_stream()` | Stream all results |
| `foo_check()` | Test specific relationship |
| `{dep}_join()` | Join helper for dependencies |

---

## Performance Characteristics

| Operation | Complexity | Memory |
|-----------|------------|--------|
| Fact lookup | O(1) | O(n) for hashtable |
| Stream | O(n) | O(1) streaming |
| Inner join | O(n × m) | O(1) streaming |
| Outer join | O(n + m) | O(m) for hashtable |
| sort -u | O(n log n) | O(n) buffering |

**Note**: `sort -u` buffers all data. For very large outputs, consider `[unique(false)]` constraint.

---

## Source Files

- `src/unifyweaver/core/stream_compiler.pl`
- `src/unifyweaver/core/compiler_driver.pl`

## See Also

- Chapter 2: Stream Compilation (tutorial)
- Chapter 3: Advanced Constraints (controlling deduplication)
- Chapter 4: Variable Scope (pipeline gotchas)
