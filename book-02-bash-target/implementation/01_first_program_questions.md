<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 1: Your First Program - Questions

Q&A companion for [01_first_program_impl.md](./01_first_program_impl.md).

---

## Question Index

1. [What does compile_recursive/3 do?](#b02c01-q-compile-recursive)
2. [What does compile_facts/4 do?](#b02c01-q-compile-facts)
3. [What is the complete compilation workflow?](#b02c01-q-workflow)
4. [How does visited tracking prevent infinite loops?](#b02c01-q-visited-tracking)
5. [What query modes does the generated fact function support?](#b02c01-q-query-modes)
6. [Why must scripts be sourced in dependency order?](#b02c01-q-dependency-order)
7. [How do I fix "Unknown procedure" errors?](#b02c01-q-unknown-procedure)
8. [How do I fix "source_sink does not exist" errors?](#b02c01-q-source-sink)
9. [How do I clear state between queries?](#b02c01-q-clear-state)

---

## Questions and Answers

### <a id="b02c01-q-compile-recursive"></a>Q1: What does compile_recursive/3 do?

**Answer**: `compile_recursive/3` compiles a recursive Prolog predicate to Bash code with fixpoint evaluation:

```prolog
compile_recursive(+Predicate/Arity, +Options, -BashCode)
```

It:
1. Collects all clauses for the predicate
2. Analyzes recursion patterns (base case, recursive case)
3. Generates a Bash while loop with visited tracking
4. Creates separate functions for each clause

Example:
```prolog
?- compile_recursive(ancestor/2, [], Code).
```

**See**: [compile_recursive/3](./01_first_program_impl.md#compile_recursive3)

---

### <a id="b02c01-q-compile-facts"></a>Q2: What does compile_facts/4 do?

**Answer**: `compile_facts/4` compiles Prolog facts to a Bash lookup function:

```prolog
stream_compiler:compile_facts(+Predicate, +Arity, +Options, -BashCode)
```

It:
1. Queries all facts from the Prolog database
2. Generates a Bash associative array (`declare -A`)
3. Creates a lookup function with multiple query modes

Example:
```prolog
?- stream_compiler:compile_facts(parent, 2, [], Code).
```

**See**: [compile_facts/4](./01_first_program_impl.md#compile_facts4)

---

### <a id="b02c01-q-workflow"></a>Q3: What is the complete compilation workflow?

**Answer**: The workflow has 7 steps:

1. **Initialize environment**: `['education/init']`
2. **Load compiler modules**: `use_module(unifyweaver(core/recursive_compiler))`
3. **Load user predicates**: `['family_tree.pl']`
4. **Compile facts**: `compile_facts(parent, 2, [], Code)`
5. **Compile recursive rules**: `compile_recursive(ancestor/2, [], Code)`
6. **Save to files**: `open('output/ancestor.sh', write, S), write(S, Code), close(S)`
7. **Execute**: `source parent.sh && source ancestor.sh && ancestor abraham`

**See**: [Compilation Workflow](./01_first_program_impl.md#compilation-workflow)

---

### <a id="b02c01-q-visited-tracking"></a>Q4: How does visited tracking prevent infinite loops?

**Answer**: The generated Bash uses an associative array to track processed arguments:

```bash
declare -A _visited_ancestor

ancestor() {
    local key="${1}:${2}"
    [[ -n "${_visited_ancestor[$key]}" ]] && return 0  # Skip if visited
    _visited_ancestor[$key]=1  # Mark as visited
    # ... continue processing
}
```

This ensures:
- Each argument combination is processed exactly once
- Recursive calls to already-visited states return immediately
- The fixpoint is reached when no new facts can be derived

**See**: [Visited Tracking](./01_first_program_impl.md#visited-tracking)

---

### <a id="b02c01-q-query-modes"></a>Q5: What query modes does the generated fact function support?

**Answer**: Three query modes based on which arguments are provided:

| Arguments | Behavior | Example |
|-----------|----------|---------|
| Both | Membership test (returns 0/1) | `parent abraham isaac && echo "yes"` |
| First only | Stream matching pairs | `parent abraham` → `abraham:ishmael` |
| None | Stream all facts | `parent` → all pairs |

The function pattern-matches on which arguments are non-empty.

**See**: [Query Modes](./01_first_program_impl.md#query-modes)

---

### <a id="b02c01-q-dependency-order"></a>Q6: Why must scripts be sourced in dependency order?

**Answer**: Recursive predicates call their dependencies. If `ancestor/2` uses `parent/2`, the `parent` function must be defined first:

```bash
# CORRECT
source parent.sh    # Defines parent()
source ancestor.sh  # Uses parent()
ancestor abraham

# WRONG
source ancestor.sh
ancestor abraham    # Error: parent: command not found
```

The dependency graph determines sourcing order: base facts first, then predicates that depend on them.

**See**: [Dependency Handling](./01_first_program_impl.md#dependency-handling)

---

### <a id="b02c01-q-unknown-procedure"></a>Q7: How do I fix "Unknown procedure" errors?

**Answer**: This error means the predicate isn't loaded or has the wrong arity:

```prolog
?- compile_recursive(my_pred/2, [], Code).
ERROR: Unknown procedure: my_pred/2
```

**Fix**:
```prolog
?- listing(my_pred).       % Check what's loaded
?- ['my_predicates.pl'].   % Load the file
?- assertz(my_pred(a, b)). % Or assert directly
```

**See**: [Troubleshooting](./01_first_program_impl.md#troubleshooting)

---

### <a id="b02c01-q-source-sink"></a>Q8: How do I fix "source_sink does not exist" errors?

**Answer**: This error means the UnifyWeaver library path isn't set up:

```prolog
?- use_module(unifyweaver(core/recursive_compiler)).
ERROR: source_sink `unifyweaver(...)' does not exist
```

**Fix**: Load `education/init.pl` first:
```prolog
?- ['education/init'].
[UnifyWeaver] Educational environment initialized.
```

Also ensure you started `swipl` from the UnifyWeaver project root directory.

**See**: [Troubleshooting](./01_first_program_impl.md#troubleshooting)

---

### <a id="b02c01-q-clear-state"></a>Q9: How do I clear state between queries?

**Answer**: Unset and re-declare the visited array:

```bash
source ancestor.sh

ancestor abraham  # First query

# Clear state for fresh computation
unset _visited_ancestor
declare -A _visited_ancestor

ancestor isaac    # Fresh query
```

This is necessary when you want to recompute results without cached state from previous queries.

**See**: [Clearing State](./01_first_program_impl.md#clearing-state)

---

## Summary

This chapter covers the basics of Bash target compilation:
- `compile_recursive/3` for recursive predicates
- `compile_facts/4` for base facts
- The complete 7-step workflow
- Visited tracking for termination
- Dependency ordering for script sourcing
