<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# AWK Target - Questions

Q&A companion for [01_overview_impl.md](./01_overview_impl.md).

---

## Question Index

1. [What does compile_predicate_to_awk/3 do?](#bawk-q-compile)
2. [What input formats are supported?](#bawk-q-formats)
3. [How are facts compiled?](#bawk-q-facts)
4. [How are rules with filters compiled?](#bawk-q-rules)
5. [What aggregations are supported?](#bawk-q-aggregations)
6. [How is tail recursion compiled?](#bawk-q-tail-recursion)
7. [How does regex matching work?](#bawk-q-regex)
8. [What are the AWK target limitations?](#bawk-q-limitations)
9. [How do I use a custom field separator?](#bawk-q-separator)
10. [Why use AWK over other targets?](#bawk-q-why-awk)

---

## Questions and Answers

### <a id="bawk-q-compile"></a>Q1: What does compile_predicate_to_awk/3 do?

**Answer**: Compiles a Prolog predicate to AWK code:

```prolog
?- compile_predicate_to_awk(high_salary/2, [], AWK),
   write_awk_script('high_salary.awk', AWK).
```

Run with: `awk -f high_salary.awk data.tsv`

**See**: [compile_predicate_to_awk/3](./01_overview_impl.md#compile_predicate_to_awk3)

---

### <a id="bawk-q-formats"></a>Q2: What input formats are supported?

**Answer**: Three formats:

| Format | Option | Field Access |
|--------|--------|--------------|
| TSV | `record_format(tsv)` | `$1`, `$2`, ... |
| CSV | `record_format(csv)` | `$1`, `$2`, ... |
| JSONL | `record_format(jsonl)` | `match()` extraction |

**See**: [Input Format Options](./01_overview_impl.md#input-format-options)

---

### <a id="bawk-q-facts"></a>Q3: How are facts compiled?

**Answer**: Facts become AWK associative arrays:

```prolog
employee(alice, engineering).
```

```awk
BEGIN {
    employee["alice"] = "engineering"
}
```

Provides O(1) lookup.

**See**: [Fact Compilation](./01_overview_impl.md#fact-compilation)

---

### <a id="bawk-q-rules"></a>Q4: How are rules with filters compiled?

**Answer**: Rules become `if` statements:

```prolog
high_salary(Name, Salary) :- employee(Name, _, Salary), Salary > 50000.
```

```awk
{
    if ($3 > 50000) { print $1, $3 }
}
```

**See**: [Rule Compilation](./01_overview_impl.md#rule-compilation)

---

### <a id="bawk-q-aggregations"></a>Q5: What aggregations are supported?

**Answer**:

| Operation | AWK Pattern |
|-----------|-------------|
| `sum_list/2` | `total += value` |
| `count/1` | `count++` |
| `max_list/2` | `if (val > max) max = val` |
| `min_list/2` | `if (val < min) min = val` |

Results printed in `END` block.

**See**: [Aggregation Compilation](./01_overview_impl.md#aggregation-compilation)

---

### <a id="bawk-q-tail-recursion"></a>Q6: How is tail recursion compiled?

**Answer**: Tail recursion becomes a `while` loop:

```prolog
sum_to(N, Acc, Result) :- N > 0, N1 is N-1, Acc1 is Acc+N, sum_to(N1, Acc1, Result).
```

```awk
function sum_to(n, acc) {
    while (n > 0) { acc = acc + n; n = n - 1 }
    return acc
}
```

**See**: [Tail Recursion Compilation](./01_overview_impl.md#tail-recursion-compilation)

---

### <a id="bawk-q-regex"></a>Q7: How does regex matching work?

**Answer**: Use `match/2`, `match/3`, or `match/4`:

```prolog
extract_ip(Line, IP) :- match(Line, "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+", IP).
```

```awk
if (match($0, /[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+/)) {
    ip = substr($0, RSTART, RLENGTH)
}
```

**See**: [Regex Matching](./01_overview_impl.md#regex-matching)

---

### <a id="bawk-q-limitations"></a>Q8: What are the AWK target limitations?

**Answer**:

| Limitation | Reason |
|------------|--------|
| No deep recursion | Only tail recursion |
| No nested structures | Flat data only |
| No backtracking | First match semantics |

These are inherent to AWK's streaming model.

**See**: [Limitations](./01_overview_impl.md#limitations)

---

### <a id="bawk-q-separator"></a>Q9: How do I use a custom field separator?

**Answer**: Use the `field_separator` option:

```prolog
compile_predicate_to_awk(pred/2, [field_separator(':')], AWK)
```

Generates `BEGIN { FS=":" }`.

**See**: [Custom Separator](./01_overview_impl.md#custom-separator)

---

### <a id="bawk-q-why-awk"></a>Q10: Why use AWK over other targets?

**Answer**:

- **Ubiquitous**: Pre-installed on all Unix systems
- **Fast**: Optimized for line-by-line processing
- **Portable**: Same script works everywhere
- **No dependencies**: Self-contained scripts
- **Pipeline-friendly**: Designed for Unix pipes

**See**: [Overview](./01_overview_impl.md#overview-compilation-features)

---

## Summary

AWK target compilation provides:
- Facts as associative arrays
- Rules as `if` statements
- Aggregations in END blocks
- Tail recursion as while loops
- Regex via `match()` built-in
- TSV/CSV/JSONL input formats
