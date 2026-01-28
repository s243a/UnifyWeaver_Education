<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Stream Compilation - Implementation Details

This document provides function-level documentation for non-recursive predicate compilation.

**Source**: `src/unifyweaver/core/stream_compiler.pl`

---

## Overview: Stream Processing Philosophy

The stream compiler treats Prolog logic as a blueprint for Unix pipelines:

```
Prolog Rule → Bash Pipeline
```

Data flows line-by-line, avoiding memory issues with large datasets.

---

## compile_stream/3

Compiles non-recursive predicates to streaming Bash pipelines.

### Signature

```prolog
compile_stream(+Predicate/Arity, +Options, -BashCode)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicate/Arity` | `atom/integer` | Non-recursive predicate to compile |
| `Options` | `list` | Compilation options |
| `BashCode` | `string` | Generated Bash code |

### Algorithm

1. **Collect clauses** - Gather all rules for the predicate
2. **Analyze goals** - Identify joins, filters, projections
3. **Generate pipeline** - Create Unix-style data flow
4. **Add deduplication** - Append `sort -u` if needed

---

## Stream Join Pattern

### Prolog Source

```prolog
grandparent(GP, GC) :-
    parent(GP, P),
    parent(P, GC).
```

### Generated Bash

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

### How It Works

1. `parent_stream` outputs all `parent:child` pairs
2. Each line flows through `parent_join`
3. For each line `(a:b)`, check all parent entries `(c:d)`
4. If `b == c` (child matches parent), output `a:d`
5. `sort -u` removes duplicates

---

## Multiple Rules (OR Handling)

### Prolog Source

```prolog
child(C) :- parent(M, C), female(M).
child(C) :- parent(F, C), male(F).
```

### Generated Bash

```bash
child_stream() {
    {
        # Rule 1: mother
        parent_stream | while IFS=: read -r m c; do
            female_check "$m" && echo "$c"
        done

        # Rule 2: father
        parent_stream | while IFS=: read -r f c; do
            male_check "$f" && echo "$c"
        done
    } | sort -u
}
```

### Explanation

- Both rules are evaluated independently
- Results are concatenated with `{ ... }`
- `sort -u` ensures each result appears once (set semantics)

---

## Outer Join Patterns

### LEFT OUTER JOIN

```prolog
employee_dept(Emp, Dept) :-
    employee(Emp, DeptId),
    (department(DeptId, Dept) ; Dept = null).
```

**Detection Pattern**: `(Goal ; Var = null)` in rule body

**Generated Bash**:
```bash
employee_dept() {
    employee_stream | while IFS=: read -r emp deptid; do
        dept=$(department_lookup "$deptid")
        if [[ -n "$dept" ]]; then
            echo "$emp:$dept"
        else
            echo "$emp:null"
        fi
    done
}
```

### RIGHT OUTER JOIN

```prolog
dept_employee(Emp, Dept) :-
    (employee(Emp, DeptId) ; Emp = null),
    department(DeptId, Dept).
```

Swaps the left/right roles in the generated code.

### FULL OUTER JOIN

```prolog
full_join(Emp, Dept) :-
    (employee(Emp, DeptId) ; Emp = null),
    (department(DeptId, Dept) ; Dept = null).
```

Generates code with match tracking for both sides.

---

## compile_predicate/3

Wrapper that auto-selects the correct compiler.

### Signature

```prolog
compile_predicate(+Predicate/Arity, +Options, -BashCode)
```

### Behavior

```prolog
compile_predicate(P/A, Opts, Code) :-
    (   is_recursive(P/A)
    ->  compile_recursive(P/A, Opts, Code)
    ;   compile_stream(P/A, Opts, Code)
    ).
```

### Example

```prolog
?- compile_predicate(grandparent/2, [], Code).  % Uses stream_compiler
?- compile_predicate(ancestor/2, [], Code).     % Uses recursive_compiler
```

---

## Pipeline Building Blocks

### Source: parent_stream

```bash
parent_stream() {
    for key in "${!parent_data[@]}"; do
        echo "$key"
    done
}
```

### Filter: Selection

```bash
# Prolog: adult(P) :- age(P, A), A >= 18.
while IFS=: read -r person age; do
    [[ "$age" -ge 18 ]] && echo "$person"
done
```

### Transform: Projection

```bash
# Prolog: name(N) :- person(N, _, _).
while IFS=: read -r name age city; do
    echo "$name"
done
```

### Combine: Join

```bash
# Prolog: gp(GP, GC) :- parent(GP, P), parent(P, GC).
while IFS=: read -r gp p; do
    parent "$p" | while IFS=: read -r _ gc; do
        echo "$gp:$gc"
    done
done
```

---

## Variable Scope Notes

### Safe Pattern (No State Accumulation)

```bash
parent_stream | while IFS=: read -r a b; do
    # Variables a, b are local to this iteration
    # No state carried between lines
    echo "$a:$b"
done
```

### Caution: Subshell Variables

```bash
# WRONG: count won't be updated outside pipe
count=0
parent_stream | while read -r line; do
    ((count++))  # Updates subshell variable only
done
echo "$count"  # Still 0!

# CORRECT: Use process substitution
count=0
while read -r line; do
    ((count++))
done < <(parent_stream)
echo "$count"  # Correct value
```

See [Book 2 Chapter 4](../04_variable_scope_and_process_substitution.md) for details.

---

## Performance Characteristics

### Streaming Advantages

| Aspect | Streaming | In-Memory |
|--------|-----------|-----------|
| Memory | O(1) per line | O(n) total |
| Start latency | Immediate | Load all first |
| Large datasets | Handles any size | May OOM |
| Parallelism | Natural with pipes | Requires coordination |

### Join Complexity

| Join Type | Complexity | Notes |
|-----------|------------|-------|
| Nested loop | O(n × m) | Simple, works always |
| Hash join | O(n + m) | Requires building hash first |
| Sort-merge | O(n log n + m log m) | Good for sorted data |

The stream compiler uses nested loop joins by default for simplicity.

---

## Related Documentation

- [Book 2 Chapter 1: Your First Program](./01_first_program_impl.md)
- [Book 2 Chapter 3: Constraints](../03_advanced_constraints.md)
- [Stream Compiler Source](../../../../src/unifyweaver/core/stream_compiler.pl)
