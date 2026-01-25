<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Stream Compilation - Questions

**Q&A companion to [02_stream_compilation_impl.md](./02_stream_compilation_impl.md)**

---

<a id="b02c02-q-stream-philosophy"></a>
## Q: What is the stream compilation philosophy?

Treat Prolog logic as a blueprint for Unix-style pipelines:

```
Data Source → Filter → Transform → Join → Output
```

Benefits: Memory efficient, composable, natural for shell.

**Reference**: [Stream Compilation Philosophy](./02_stream_compilation_impl.md#stream-compilation-philosophy)

---

<a id="b02c02-q-compile-stream"></a>
## Q: How do I use compile_stream/3?

```prolog
?- compile_stream(grandparent/2, [], Code).
```

Compiles non-recursive predicates to streaming pipelines.

**Reference**: [compile_stream/3](./02_stream_compilation_impl.md#compile_stream3)

---

<a id="b02c02-q-inner-join"></a>
## Q: How does inner join work in stream compilation?

For `grandparent(GP, GC) :- parent(GP, P), parent(P, GC)`:

```bash
parent_join() {
    while IFS= read -r input; do
        IFS=":" read -r a b <<< "$input"  # a=GP, b=P
        for key in "${!parent_data[@]}"; do
            IFS=":" read -r c d <<< "$key"
            [[ "$b" == "$c" ]] && echo "$a:$d"  # Join on P
        done
    done
}
```

**Reference**: [Join Operations](./02_stream_compilation_impl.md#join-operations)

---

<a id="b02c02-q-left-join"></a>
## Q: How do I write a LEFT OUTER JOIN in Prolog?

Use the `(Goal ; Var = null)` pattern:

```prolog
employee_dept(Emp, Dept) :-
    employee(Emp, DeptId),
    (department(DeptId, Dept) ; Dept = null).
```

This returns all employees, with null for unmatched departments.

**Reference**: [Outer Joins](./02_stream_compilation_impl.md#outer-joins)

---

<a id="b02c02-q-right-join"></a>
## Q: How do I write a RIGHT OUTER JOIN?

```prolog
dept_employee(Emp, Dept) :-
    (employee(Emp, DeptId) ; Emp = null),
    department(DeptId, Dept).
```

Returns all departments, with null for empty ones.

**Reference**: [Outer Joins](./02_stream_compilation_impl.md#outer-joins)

---

<a id="b02c02-q-full-join"></a>
## Q: How do I write a FULL OUTER JOIN?

```prolog
full_join(Emp, Dept) :-
    (employee(Emp, DeptId) ; Emp = null),
    (department(DeptId, Dept) ; Dept = null).
```

Returns all records from both sides.

**Reference**: [Outer Joins](./02_stream_compilation_impl.md#outer-joins)

---

<a id="b02c02-q-multiple-rules"></a>
## Q: How does stream compiler handle multiple rules (OR)?

Generates pipeline for each rule, concatenates, deduplicates:

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

**Reference**: [Multiple Rules (OR)](./02_stream_compilation_impl.md#multiple-rules-or)

---

<a id="b02c02-q-compile-predicate"></a>
## Q: What is compile_predicate/3?

Auto-selects the right compiler:

```prolog
?- compile_predicate(grandparent/2, [], Code).  % Uses stream_compiler
?- compile_predicate(ancestor/2, [], Code).      % Uses recursive_compiler
```

**Recommendation**: Use this unless you need specific compiler features.

**Reference**: [compile_predicate/3](./02_stream_compilation_impl.md#compile_predicate3)

---

<a id="b02c02-q-generated-functions"></a>
## Q: What functions are generated for a predicate?

For `foo/2`:
- `foo()` - Main entry point
- `foo_stream()` - Stream all results
- `foo_check()` - Test specific relationship
- `{dep}_join()` - Join helpers

**Reference**: [Generated Function Signatures](./02_stream_compilation_impl.md#generated-function-signatures)

---

<a id="b02c02-q-performance"></a>
## Q: What's the performance of stream operations?

| Operation | Time | Memory |
|-----------|------|--------|
| Fact lookup | O(1) | O(n) |
| Stream | O(n) | O(1) |
| Inner join | O(n×m) | O(1) |
| sort -u | O(n log n) | O(n) |

**Reference**: [Performance Characteristics](./02_stream_compilation_impl.md#performance-characteristics)

---

## Question Index

| ID | Topic |
|----|-------|
| [b02c02-q-stream-philosophy](#b02c02-q-stream-philosophy) | Stream philosophy |
| [b02c02-q-compile-stream](#b02c02-q-compile-stream) | compile_stream/3 |
| [b02c02-q-inner-join](#b02c02-q-inner-join) | Inner join |
| [b02c02-q-left-join](#b02c02-q-left-join) | LEFT OUTER JOIN |
| [b02c02-q-right-join](#b02c02-q-right-join) | RIGHT OUTER JOIN |
| [b02c02-q-full-join](#b02c02-q-full-join) | FULL OUTER JOIN |
| [b02c02-q-multiple-rules](#b02c02-q-multiple-rules) | Multiple rules (OR) |
| [b02c02-q-compile-predicate](#b02c02-q-compile-predicate) | compile_predicate/3 |
| [b02c02-q-generated-functions](#b02c02-q-generated-functions) | Generated functions |
| [b02c02-q-performance](#b02c02-q-performance) | Performance |
