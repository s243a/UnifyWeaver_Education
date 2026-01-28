<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# AWK Target - Implementation Details

This document provides function-level documentation for AWK target compilation.

**Source**: `src/unifyweaver/targets/awk_target.pl`

---

## Overview: Compilation Features

| Feature | AWK Output |
|---------|------------|
| Facts | Associative arrays |
| Single Rules | Direct translation with constraints |
| Multiple Rules | Union-style logic |
| Aggregations | sum, count, max, min, avg |
| Tail Recursion | While loops |
| Regex Matching | `match/2`, `match/3`, `match/4` |

---

## compile_predicate_to_awk/3

Compiles a Prolog predicate to AWK code.

### Signature

```prolog
compile_predicate_to_awk(+Predicate/Arity, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicate/Arity` | `atom/integer` | Predicate to compile |
| `Options` | `list` | Compilation options |
| `Code` | `string` | Generated AWK code |

### Options

| Option | Values | Default | Description |
|--------|--------|---------|-------------|
| `record_format(F)` | `tsv`, `csv`, `jsonl` | `tsv` | Input format |
| `field_separator(S)` | `string` | `\t` | Custom separator |

### Example

```prolog
?- compile_predicate_to_awk(high_salary/2, [], AWK),
   write_awk_script('high_salary.awk', AWK).
```

---

## Input Format Options

### TSV (Tab-Separated Values)

```prolog
compile_predicate_to_awk(pred/2, [record_format(tsv)], AWK)
```

Default. Uses `$1`, `$2`, etc. for field access.

### CSV (Comma-Separated Values)

```prolog
compile_predicate_to_awk(pred/2, [record_format(csv)], AWK)
```

Sets `FS=","` in BEGIN block.

### JSONL (JSON Lines)

```prolog
compile_predicate_to_awk(pred/2, [record_format(jsonl)], AWK)
```

Uses `match()` for field extraction.

### Custom Separator

```prolog
compile_predicate_to_awk(pred/2, [field_separator(':')], AWK)
```

---

## Fact Compilation

### Pattern

```prolog
employee(alice, engineering).
employee(bob, marketing).
```

### Generated AWK

```awk
BEGIN {
    employee["alice"] = "engineering"
    employee["bob"] = "marketing"
}
```

Facts compile to AWK associative arrays for O(1) lookup.

---

## Rule Compilation

### Simple Filter

```prolog
high_salary(Name, Salary) :-
    employee(Name, _, Salary),
    Salary > 50000.
```

### Generated AWK

```awk
{
    name = $1
    salary = $3
    if (salary > 50000) {
        print name, salary
    }
}
```

---

## Aggregation Compilation

### Pattern

```prolog
total_salary(Total) :-
    findall(S, employee(_, _, S), Salaries),
    sum_list(Salaries, Total).
```

### Generated AWK

```awk
{
    total += $3
}
END {
    print total
}
```

### Supported Aggregations

| Prolog | AWK |
|--------|-----|
| `sum_list/2` | `total += value` |
| `count/1` | `count++` |
| `max_list/2` | `if (val > max) max = val` |
| `min_list/2` | `if (val < min) min = val` |
| `avg/2` | `sum/count` in END |

---

## Tail Recursion Compilation

### Pattern

```prolog
sum_to(0, Acc, Acc).
sum_to(N, Acc, Result) :-
    N > 0,
    N1 is N - 1,
    Acc1 is Acc + N,
    sum_to(N1, Acc1, Result).
```

### Generated AWK

```awk
function sum_to(n, acc) {
    while (n > 0) {
        acc = acc + n
        n = n - 1
    }
    return acc
}
```

Tail recursion compiles to efficient `while` loops.

---

## Regex Matching

### match/2

```prolog
has_error(Line) :- match(Line, "ERROR").
```

### match/3

```prolog
extract_ip(Line, IP) :- match(Line, "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+", IP).
```

### Generated AWK

```awk
{
    if (match($0, /ERROR/)) {
        # has_error succeeds
    }

    if (match($0, /[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+/)) {
        ip = substr($0, RSTART, RLENGTH)
    }
}
```

---

## Limitations

| Limitation | Reason |
|------------|--------|
| No deep recursion | Only tail recursion supported |
| No nested structures | Flat data only |
| No mutual recursion | Single recursive predicates |
| No backtracking | First match semantics |
| No complex unification | Simple variable binding |

These limitations are inherent to AWK's streaming model.

---

## Related Documentation

- [AWK Target Examples](../../../../docs/AWK_TARGET_EXAMPLES.md)
- [AWK Target Status](../../../../docs/AWK_TARGET_STATUS.md)
- [Book 2: Bash Target](../../book-02-bash-target/)
