<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# The AWK Target Book

A practical guide to compiling Prolog predicates to self-contained AWK scripts using UnifyWeaver's AWK target.

## What You'll Learn

This book teaches you how to use UnifyWeaver to generate portable AWK scripts from Prolog predicates. AWK is a powerful text-processing language available on virtually every Unix-like system, making it ideal for data pipelines and log analysis.

## Prerequisites

- Basic understanding of Prolog syntax
- Familiarity with command-line tools
- AWK installed on your system (usually pre-installed on Linux/macOS)
- UnifyWeaver set up and working

## Chapter Overview

### [Chapter 1: Getting Started](chapters/01-getting-started.md)
Introduction to the AWK target, installation verification, and your first compiled script.

### [Chapter 2: Facts and Filtering](chapters/02-facts-filtering.md)
Learn to compile Prolog facts to AWK associative arrays and filter data with constraints.

### [Chapter 3: Rules and Constraints](chapters/03-rules-constraints.md)
Compile single and multiple rules with arithmetic and logical constraints.

### [Chapter 4: Aggregations](chapters/04-aggregations.md)
Use built-in aggregation operations: sum, count, max, min, and avg.

### [Chapter 5: Tail Recursion](chapters/05-tail-recursion.md)
Compile numeric tail loops and use the shared compiler for graph reachability.

### [Chapter 6: Regex Pattern Matching](chapters/06-regex-matching.md)
Use the `match/2`, `match/3`, and `match/4` predicates for regex operations.

### [Chapter 7: Practical Applications](chapters/07-practical-applications.md)
Real-world examples: log analysis, data transformation, and pipeline integration.

### [Native Clause Body Lowering](02_native_clause_lowering.md)
Multi-clause predicates → if/else if chains, arithmetic, error handling.

## Key Features of the AWK Target

| Feature | Description |
|---------|-------------|
| Facts | Compiled to AWK associative arrays |
| Single Rules | Direct translation with constraint evaluation |
| Multiple Rules | Combined with union-style logic |
| Aggregations | sum, count, max, min, avg operations |
| Tail Recursion | Recognized numeric accumulator patterns become while loops |
| Graph Recursion | Shared compiler: binary reachability; direct target: specialized hop traversal |
| Regex Matching | Full support via `match/2`, `match/3`, `match/4` |
| Input Formats | TSV, CSV, JSONL record formats |

## Input Format Options

The AWK target supports multiple input formats:

```prolog
% Tab-separated values (default)
compile_predicate_to_awk(pred/2, [record_format(tsv)], AWK)

% Comma-separated values
compile_predicate_to_awk(pred/2, [record_format(csv)], AWK)

% JSON Lines (one JSON object per line)
compile_predicate_to_awk(pred/2, [record_format(jsonl)], AWK)

% Custom field separator
compile_predicate_to_awk(pred/2, [field_separator(':')], AWK)
```

## Quick Example

**Prolog predicate:**
```prolog
high_salary(Name, Salary) :-
    employee(Name, _, Salary),
    Salary > 50000.
```

**Compile to AWK:**
```prolog
?- compile_predicate_to_awk(high_salary/2, [], AWK),
   write_awk_script(AWK, 'high_salary.awk').
```

**Run the script:**
```bash
awk -f high_salary.awk employees.tsv
```

## Limitations

AWK supports recursive user functions. The limits here concern which Prolog
patterns UnifyWeaver translates and which entry point selects the right backend:

- **Numeric tail loops**: The verified direct `factorial/3` recipe becomes a while
  loop. The shared recursive compiler currently rejects that same AWK tail case.
- **Graph recursion**: The shared compiler supports canonical binary transitive
  closure using adjacency arrays, BFS and a visited set. The direct target has a
  specialized ternary hop traversal using an explicit DFS stack for simple paths.
- **Restricted semantics**: These paths do not establish support for arbitrary
  linear, tree or mutual recursion, general Prolog backtracking, nested terms or
  complex unification. Returning code alone does not establish semantic support.
- **Entry point and query matter**: Single-source reachability, all reachable pairs
  and all simple paths are different computations. The shared BFS excludes the
  starting node even on cycles; a pair fixpoint can include cycle-derived self pairs.

See [Chapter 5](chapters/05-tail-recursion.md) for the working recipes and the
[validation snapshot](examples/VALIDATION.md) for the tested revision, results and
entry-point caveats. These are compiler restrictions and conventions, not inherent
limits of AWK's streaming model.

## Why AWK?

- **Ubiquitous**: Pre-installed on virtually all Unix-like systems
- **Fast**: Optimized for line-by-line text processing
- **Portable**: Same script works across platforms
- **Pipeline-friendly**: Designed for Unix pipes
- **No dependencies**: Self-contained scripts

## Getting Help

- Check the [UnifyWeaver documentation](https://github.com/s243a/UnifyWeaver/tree/main/docs)
- Review the [AWK target examples](https://github.com/s243a/UnifyWeaver/blob/main/docs/AWK_TARGET_EXAMPLES.md)
- See the [AWK target status](https://github.com/s243a/UnifyWeaver/blob/main/docs/AWK_TARGET_STATUS.md)

## License

This educational content is dual-licensed under MIT (code) and CC-BY-4.0 (documentation).
