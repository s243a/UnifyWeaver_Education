<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Stream Compilation - Questions

Q&A companion for [02_stream_compilation_impl.md](./02_stream_compilation_impl.md).

---

## Question Index

1. [What is the stream compiler's philosophy?](#b02c02-q-philosophy)
2. [What does compile_stream/3 do?](#b02c02-q-compile-stream)
3. [How are stream joins implemented?](#b02c02-q-stream-join)
4. [How are multiple rules (OR) handled?](#b02c02-q-or-handling)
5. [What is a LEFT OUTER JOIN pattern?](#b02c02-q-left-outer)
6. [What does compile_predicate/3 do?](#b02c02-q-compile-predicate)
7. [What are the pipeline building blocks?](#b02c02-q-building-blocks)
8. [What is the variable scope issue with pipes?](#b02c02-q-variable-scope)
9. [What are the performance characteristics?](#b02c02-q-performance)
10. [When should I use streaming vs in-memory?](#b02c02-q-when-streaming)

---

## Questions and Answers

### <a id="b02c02-q-philosophy"></a>Q1: What is the stream compiler's philosophy?

**Answer**: The stream compiler treats Prolog logic as a blueprint for Unix pipelines. Data flows line-by-line through the pipeline, which:
- Avoids loading large datasets into memory
- Processes data one line at a time
- Uses standard Unix tools (pipes, while loops)

This is efficient for large datasets that don't fit in memory.

**See**: [Overview: Stream Processing Philosophy](./02_stream_compilation_impl.md#overview-stream-processing-philosophy)

---

### <a id="b02c02-q-compile-stream"></a>Q2: What does compile_stream/3 do?

**Answer**: `compile_stream/3` compiles non-recursive predicates to Bash pipelines:

```prolog
compile_stream(+Predicate/Arity, +Options, -BashCode)
```

Algorithm:
1. Collect all clauses for the predicate
2. Analyze goals (joins, filters, projections)
3. Generate Unix-style pipeline
4. Add `sort -u` for deduplication if needed

**See**: [compile_stream/3](./02_stream_compilation_impl.md#compile_stream3)

---

### <a id="b02c02-q-stream-join"></a>Q3: How are stream joins implemented?

**Answer**: Joins use nested loop pattern:

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
```

For each input line (a:b), iterate over all facts (c:d) and output matches where the join condition holds.

**See**: [Stream Join Pattern](./02_stream_compilation_impl.md#stream-join-pattern)

---

### <a id="b02c02-q-or-handling"></a>Q4: How are multiple rules (OR) handled?

**Answer**: Multiple rules generate separate pipelines that are concatenated:

```bash
child_stream() {
    {
        # Rule 1 pipeline
        ...
        # Rule 2 pipeline
        ...
    } | sort -u
}
```

The `{ ... }` groups outputs from all rules, and `sort -u` removes duplicates to maintain set semantics.

**See**: [Multiple Rules (OR Handling)](./02_stream_compilation_impl.md#multiple-rules-or-handling)

---

### <a id="b02c02-q-left-outer"></a>Q5: What is a LEFT OUTER JOIN pattern?

**Answer**: The compiler detects `(Goal ; Var = null)` patterns:

```prolog
employee_dept(Emp, Dept) :-
    employee(Emp, DeptId),
    (department(DeptId, Dept) ; Dept = null).
```

This generates code that outputs all employees, with their department if found or `null` if not:

```bash
if [[ -n "$dept" ]]; then
    echo "$emp:$dept"
else
    echo "$emp:null"
fi
```

**See**: [Outer Join Patterns](./02_stream_compilation_impl.md#outer-join-patterns)

---

### <a id="b02c02-q-compile-predicate"></a>Q6: What does compile_predicate/3 do?

**Answer**: It's a wrapper that automatically selects the correct compiler:

```prolog
compile_predicate(P/A, Opts, Code) :-
    (   is_recursive(P/A)
    ->  compile_recursive(P/A, Opts, Code)
    ;   compile_stream(P/A, Opts, Code)
    ).
```

Use this instead of calling `compile_stream` or `compile_recursive` directly.

**See**: [compile_predicate/3](./02_stream_compilation_impl.md#compile_predicate3)

---

### <a id="b02c02-q-building-blocks"></a>Q7: What are the pipeline building blocks?

**Answer**: Four main building blocks:

1. **Source** - `parent_stream` generates data
2. **Filter (Selection)** - `[[ condition ]]` keeps matching lines
3. **Transform (Projection)** - Extract/reorder fields
4. **Combine (Join)** - Nested loops matching on join key

These compose into complex pipelines via `|`.

**See**: [Pipeline Building Blocks](./02_stream_compilation_impl.md#pipeline-building-blocks)

---

### <a id="b02c02-q-variable-scope"></a>Q8: What is the variable scope issue with pipes?

**Answer**: Variables modified in a pipe subshell don't persist:

```bash
# WRONG
count=0
data | while read line; do ((count++)); done
echo "$count"  # Still 0!

# CORRECT: Use process substitution
count=0
while read line; do ((count++)); done < <(data)
echo "$count"  # Correct value
```

For stateless line-by-line processing (most stream compilation), this isn't an issue.

**See**: [Variable Scope Notes](./02_stream_compilation_impl.md#variable-scope-notes)

---

### <a id="b02c02-q-performance"></a>Q9: What are the performance characteristics?

**Answer**:

| Aspect | Streaming | In-Memory |
|--------|-----------|-----------|
| Memory | O(1) per line | O(n) total |
| Start latency | Immediate | Load all first |
| Large datasets | Handles any size | May OOM |

Join complexity: O(n × m) for nested loop joins.

**See**: [Performance Characteristics](./02_stream_compilation_impl.md#performance-characteristics)

---

### <a id="b02c02-q-when-streaming"></a>Q10: When should I use streaming vs in-memory?

**Answer**:

**Use Streaming when:**
- Dataset is large or unbounded
- Memory is constrained
- Processing can start immediately
- Natural pipeline structure

**Use In-Memory when:**
- Dataset fits easily in RAM
- Need random access to data
- Complex multi-pass algorithms
- Latency-sensitive applications

**See**: [Performance Characteristics](./02_stream_compilation_impl.md#performance-characteristics)

---

## Summary

Stream compilation provides:
- Unix pipeline-based code generation
- Line-by-line processing (O(1) memory)
- Nested loop joins
- Outer join support (LEFT, RIGHT, FULL)
- Automatic compiler selection via `compile_predicate/3`
