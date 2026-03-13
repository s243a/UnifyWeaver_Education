<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->
# Chapter 4: Recursion Patterns

The R target supports six recursion patterns. The advanced recursive compiler
detects the pattern automatically and dispatches to the R-specific code generator
via multifile predicates. Memoization is enabled by default but can be disabled
with `memo(false)`, in which case linear recursion compiles to a simple `for` loop.

## Priority Order

The compiler tries patterns in this order:

1. Tail recursion (loop rewriting)
2. Linear recursion (single recursive call, fold-based)
3. Multi-call linear recursion (template-based)
4. Direct multi-call recursion (clause-analysis approach)
5. Fold pattern (numeric tree recursion)
6. Tree recursion (structural)
7. Mutual recursion (SCC detection)

## 1. Tail Recursion

Tail-recursive predicates compile to R `for` loops with an accumulator, avoiding
stack growth.

### Source Prolog

```prolog
sum_list([], Acc, Acc).
sum_list([H|T], Acc, Result) :- Acc1 is Acc + H, sum_list(T, Acc1, Result).
```

### Generated R

```r
sum_list <- function(lst, acc = 0) {
    for (item in lst) {
        acc <- acc + item
    }
    return(acc)
}

if (!interactive()) {
    args <- commandArgs(TRUE)
    if (length(args) >= 1) {
        items <- as.numeric(unlist(strsplit(args[1], ",")))
        cat(sum_list(items), "\n")
    }
}
```

### Running It

```bash
$ Rscript sum_list.R 1,2,3,4,5
15
```

The tail recursion compiler detects the accumulator pattern (`Acc1 is Acc + H`)
and maps it to an in-place update inside a `for` loop.

### Ternary vs Binary

The R target handles two tail recursion shapes:

| Shape | Prolog Arity | R Pattern | Example |
|-------|-------------|-----------|---------|
| **Ternary** | 3 args (list, acc, result) | `for` loop + accumulator | `sum_list/3` |
| **Binary** | 2 args (list, result) | `for` loop + counter | `count_items/2` |

## 2. Linear Recursion

Linear recursion (one recursive call per clause) has two compilation modes:

- **Loop-based** (`memo(false)`): Compiles to a `for` loop — no memoization overhead
- **Fold-based** (default): Compiles to `Reduce()` with memoization

### Source Prolog

```prolog
factorial(0, 1).
factorial(N, F) :- N > 0, N1 is N - 1, factorial(N1, F1), F is N * F1.
```

### Generated R — Loop-Based (memo(false))

```r
factorial <- function(n) {
    if (n == 0) return(1)
    result <- 1
    for (i in seq(n, 1)) {
        result <- i * result
    }
    return(result)
}
```

The compiler extracts the step from `N1 is N - 1` to derive the loop range
`seq(n, 1)`. For a step of 2 (e.g., `N1 is N - 2`), it generates
`seq(n, 2, by = -2)`. If the step is not a simple additive constant (e.g.,
`N1 is N * 2`), the loop-based path fails and the compiler backtracks to try
other strategies — no silent assumptions are made.

### Generated R — Fold-Based (default)

```r
factorial_memo <- new.env(hash=TRUE, parent=emptyenv())

factorial_op <- function(current, acc) {
    return(current * acc)
}

factorial <- function(n, expected=NULL) {
    key <- as.character(n)
    if (!is.null(factorial_memo[[key]])) {
        cached <- factorial_memo[[key]]
        if (!is.null(expected)) {
            if (cached == expected) return(TRUE) else return(FALSE)
        } else {
            return(cached)
        }
    }

    if (n == 0) {
        result <- 1
        factorial_memo[[key]] <- result
        if (!is.null(expected)) {
            if (result == expected) return(TRUE) else return(FALSE)
        } else {
            return(result)
        }
    }

    # Recursive case using Reduce
    range_vals <- seq(n, 1)
    result <- Reduce(factorial_op, range_vals, init=1)

    factorial_memo[[key]] <- result
    if (!is.null(expected)) {
        if (result == expected) return(TRUE) else return(FALSE)
    } else {
        return(result)
    }
}
```

### Running It

```bash
$ Rscript factorial.R 6
720

$ Rscript factorial.R 20
2432902008176640000
```

The compiler extracts the fold operation (`N * F1`) and the step expression
(`N1 is N - 1`) to generate the correct range. The loop range is derived from
the Prolog source, not hardcoded.

### Numeric vs List Fold

| Input Type | R Strategy | Example |
|-----------|-----------|---------|
| Numeric (countdown) | Loop or `Reduce(op, seq(n, base+step), init=identity)` | `factorial` |
| List (structural) | `Reduce(op, lst, init=identity)` | `list_length`, `sum_list` |

The compiler detects the input type from the base case: if the base case argument
is `0` or a number, it's numeric. If it's `[]`, it's a list.

### When to Use memo(false)

Memoization is unnecessary for predicates with no repeated subproblems (e.g.,
factorial, sum). Use `memo(false)` to generate cleaner, faster code. Predicates
like Fibonacci benefit from memoization since `fib(n-1)` and `fib(n-2)` create
overlapping subproblems.

## 3. Tree Recursion

Tree recursion handles predicates that decompose tree-structured data.

### Source Prolog

```prolog
tree_sum([V, L, R], Sum) :-
    tree_sum(L, SL), tree_sum(R, SR), Sum is V + SL + SR.
tree_sum(leaf, 0).
```

### Generated R

```r
tree_sum_memo <- new.env(hash=TRUE, parent=emptyenv())

tree_sum <- function(tree) {
    # Memoize using digest for complex structures
    key <- digest::digest(tree)
    if (!is.null(tree_sum_memo[[key]])) return(tree_sum_memo[[key]])

    if (identical(tree, "leaf")) {
        result <- 0
    } else {
        v <- tree[[1]]
        l <- tree[[2]]
        r <- tree[[3]]
        result <- v + tree_sum(l) + tree_sum(r)
    }

    tree_sum_memo[[key]] <<- result
    return(result)
}
```

### Running It

```bash
$ Rscript tree_sum.R
6
# For tree: [1, [2, leaf, leaf], [3, leaf, leaf]]
```

Tree recursion uses `digest::digest()` to hash complex R objects into memoization
keys, since nested lists can't be used directly as environment keys.

## 4. Direct Multi-Call Recursion

For predicates with 2+ independent recursive calls (like Fibonacci), the direct
multi-call compiler analyzes the clause structure and generates recursive R
functions with memoization.

### Source Prolog

```prolog
fib(0, 0).
fib(1, 1).
fib(N, F) :- N > 1, N1 is N - 1, N2 is N - 2,
             fib(N1, F1), fib(N2, F2), F is F1 + F2.
```

### Generated R

```r
fib_memo <- new.env(hash=TRUE, parent=emptyenv())

fib <- function(input) {
    key <- as.character(input)
    if (!is.null(fib_memo[[key]])) return(fib_memo[[key]])

    # Base cases
    if (input == 0) { fib_memo[[key]] <<- 0; return(0) }
    if (input == 1) { fib_memo[[key]] <<- 1; return(1) }

    # Computations (extracted from clause body)
    n1 <- input - 1
    n2 <- input - 2

    # Recursive calls
    f1 <- fib(n1)
    f2 <- fib(n2)

    # Aggregation
    result <- f1 + f2

    fib_memo[[key]] <<- result
    return(result)
}
```

### Running It

```bash
$ Rscript fib_direct.R 10
55

$ Rscript fib_direct.R 20
6765
```

### How It Works

Unlike the template-based multi-call compiler (`multicall_linear_recursion`), the
direct compiler **analyzes the actual Prolog clause structure**:

1. **`extract_body_components/5`** parses the clause body into three parts:
   - Computations: `N1 is N - 1, N2 is N - 2`
   - Recursive calls: `fib(N1, F1), fib(N2, F2)`
   - Aggregation: `F is F1 + F2` (identified by referencing recursive output variables)

2. Each part is translated to R independently

3. Variable names from Prolog's internal representation are mapped to valid R
   identifiers (prefixed with `v` since R identifiers can't start with `_` + digit)

## 5. Mutual Recursion

Mutually recursive predicates are grouped into a single R script with a shared
memoization environment.

### Source Prolog

```prolog
is_even(0).
is_even(N) :- N > 0, N1 is N - 1, is_odd(N1).
is_odd(1).
is_odd(N) :- N > 1, N1 is N - 1, is_even(N1).
```

### Generated R

```r
mutual_memo <- new.env(hash=TRUE, parent=emptyenv())

is_even <- function(n) {
    key <- paste0("is_even:", n)
    if (!is.null(mutual_memo[[key]])) return(mutual_memo[[key]])

    if (n == 0) {
        mutual_memo[[key]] <<- TRUE
        return(TRUE)
    }

    if (n > 0) {
        n1 <- n - 1
        result <- is_odd(n1)
        mutual_memo[[key]] <<- result
        return(result)
    }

    return(FALSE)
}

is_odd <- function(n) {
    key <- paste0("is_odd:", n)
    if (!is.null(mutual_memo[[key]])) return(mutual_memo[[key]])

    if (n == 1) {
        mutual_memo[[key]] <<- TRUE
        return(TRUE)
    }

    if (n > 1) {
        n1 <- n - 1
        result <- is_even(n1)
        mutual_memo[[key]] <<- result
        return(result)
    }

    return(FALSE)
}

# Main dispatch
if (!interactive()) {
    args <- commandArgs(TRUE)
    if (length(args) >= 2) {
        func <- args[1]
        val <- as.integer(args[2])
        result <- do.call(func, list(val))
        cat(result, "\n")
    }
}
```

### Running It

```bash
$ Rscript even_odd.R is_even 10
TRUE

$ Rscript even_odd.R is_odd 7
TRUE
```

The mutual recursion compiler:
1. Detects the predicate group via SCC (Strongly Connected Component) analysis
2. Generates all functions in one file so they can call each other
3. Uses a shared memo environment with function-prefixed keys to avoid collisions
4. Generates a `do.call()` dispatcher for CLI usage

## Memoization Summary

All recursion patterns use the same memoization strategy:

```r
pred_memo <- new.env(hash=TRUE, parent=emptyenv())
```

| Pattern | Key Format | Scope |
|---------|-----------|-------|
| Tail | N/A (loop, no memo needed) | — |
| Linear (memo=false) | N/A (loop, no memo needed) | — |
| Linear (default) | `as.character(n)` | Per-function |
| Tree | `digest::digest(tree)` | Per-function |
| Multi-call | `as.character(input)` | Per-function |
| Mutual | `paste0("func:", n)` | Shared across group |

---

**<-** [Previous: Data Manipulation](03_data_manipulation.md) | **->** [Next: Bindings](05_bindings.md)
