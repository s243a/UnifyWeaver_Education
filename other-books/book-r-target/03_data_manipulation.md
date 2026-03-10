<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->
# Chapter 3: Data Manipulation

R's primary strength is data manipulation. The R target maps Prolog data
operations to idiomatic R patterns using `subset()`, `aggregate()`, and `order()`.

## Filter Operations

Prolog filter predicates compile to R `subset()` calls.

### Source Prolog

```prolog
filter(DataFrame, age > 30, Output).
```

### Generated R

```r
output <- subset(data_frame, age > 30)
```

The R target translates Prolog comparison operators to R equivalents:

| Prolog | R | Meaning |
|--------|---|---------|
| `>` | `>` | Greater than |
| `<` | `<` | Less than |
| `>=` | `>=` | Greater or equal |
| `=<` | `<=` | Less or equal |
| `==` | `==` | Equal |
| `\=` | `!=` | Not equal |

## Group-By Aggregation

Prolog group-by operations compile to R `aggregate()`:

### Source Prolog

```prolog
group_by(DataFrame, department, Output).
```

### Generated R

```r
output <- aggregate(. ~ department, data = data_frame, FUN = length)
```

This groups rows by the `department` column and counts occurrences. The `aggregate()`
function is one of R's core split-apply-combine tools.

## Sort Operations

Sorting compiles to R's `order()` function:

### Source Prolog

```prolog
sort_by(DataFrame, salary, Output).
```

### Generated R

```r
output <- data_frame[order(data_frame$salary), ]
```

## Pipeline Composition

The R target supports chaining operations into pipelines. Using R 4.1+'s native
pipe operator (`|>`):

### Source Prolog

```prolog
pipeline([
    read_csv("employees.csv"),
    filter(salary > 50000),
    group_by(department),
    sort_by(count)
]).
```

### Generated R

```r
result <- read.csv("employees.csv") |>
  subset(salary > 50000) |>
  aggregate(. ~ department, data = _, FUN = length) |>
  (\(d) d[order(d$count), ])()
```

## JSONL Streaming

For streaming workloads, the R target generates JSONL (JSON Lines) readers and
writers. Each line is a self-contained JSON object:

```r
# Read JSONL from stdin
input_stream <- function() {
    lines <- readLines(con = stdin())
    lapply(lines, jsonlite::fromJSON)
}

# Write JSONL to stdout
output_stream <- function(records) {
    for (record in records) {
        cat(jsonlite::toJSON(record, auto_unbox = TRUE), "\n")
    }
}
```

This allows R scripts to participate in Unix-style pipelines:

```bash
cat data.jsonl | Rscript transform.R | Rscript aggregate.R > result.jsonl
```

## Fixpoint Evaluation

For recursive pipeline operations (like transitive closure over data frames),
the R target generates fixpoint iteration:

```r
# Semi-naive fixpoint: iterate until no new results
fixpoint <- function(base_fn, step_fn) {
    current <- base_fn()
    repeat {
        new_results <- step_fn(current)
        combined <- unique(rbind(current, new_results))
        if (nrow(combined) == nrow(current)) break
        current <- combined
    }
    current
}
```

This pattern is used when Prolog predicates define recursive rules over tabular
data, such as computing all ancestors from a parent relation stored as a data frame.

## Vector Operations

R processes vectors natively without explicit loops. UnifyWeaver leverages this
for Prolog list operations:

| Prolog | R | Vectorized? |
|--------|---|-------------|
| `length(List, N)` | `length(vec)` | Yes |
| `sum_list(List, S)` | `sum(vec)` | Yes |
| `max_list(List, M)` | `max(vec)` | Yes |
| `msort(List, Sorted)` | `sort(vec)` | Yes |
| `member(X, List)` | `X %in% vec` | Yes |
| `append(A, B, C)` | `c(a, b)` | Yes |

Vectorized operations in R are typically 10-100x faster than equivalent loop-based
code, because they're executed in C internally.

---

**<-** [Previous: Facts and Rules](02_facts_and_rules.md) | **->** [Next: Recursion Patterns](04_recursion_patterns.md)
