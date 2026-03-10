<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->
# Chapter 6: Advanced Features

## Direct Multi-Call vs Template-Based Compilation

The R target supports two approaches for predicates with multiple recursive calls
(like Fibonacci). Understanding the difference helps you choose the right tool.

### Template-Based (`multicall_linear_recursion`)

Uses hardcoded templates that recognize specific patterns (e.g., `fib(N-1) + fib(N-2)`):

```r
# Recognized pattern: two calls with N-1 and N-2
fib <- function(n) {
    if (n == 0) return(0)
    if (n == 1) return(1)
    return(fib(n-1) + fib(n-2))
}
```

Pros: Clean output, well-tested for common patterns.
Cons: Only works for patterns the template knows about.

### Direct Clause Analysis (`direct_multi_call_recursion`)

Analyzes the actual Prolog clause structure — variable names, expressions, recursive
calls, and aggregation — then translates each component independently:

```prolog
% This clause is analyzed structurally:
tribonacci(N, T) :- N > 2,
    N1 is N - 1, N2 is N - 2, N3 is N - 3,
    tribonacci(N1, T1), tribonacci(N2, T2), tribonacci(N3, T3),
    T is T1 + T2 + T3.
```

The compiler breaks this into:
- **Computations**: `N1 is N - 1, N2 is N - 2, N3 is N - 3`
- **Recursive calls**: `tribonacci(N1, T1), tribonacci(N2, T2), tribonacci(N3, T3)`
- **Aggregation**: `T is T1 + T2 + T3`

Each part is translated to R independently, so any combination of computations
and aggregations works without needing a specific template.

Pros: Handles arbitrary multi-call patterns.
Cons: Variable names are Prolog internal IDs (mapped to `v12345` style).

### Priority

In the compilation pipeline, template-based matching is tried first. If no template
matches, the direct clause analyzer runs as a fallback.

## SWI-Prolog Clause Storage Quirk

When SWI-Prolog stores `N - 1` in the clause database, it normalizes it to `N + (-1)`.
The R target handles this transparently:

```prolog
% direct_translate_expr_to_r handles both forms:
direct_translate_expr_to_r(N - K, RExpr) :- ...  % explicit subtraction
direct_translate_expr_to_r(N + K, RExpr) :- K < 0, ...  % normalized form
```

Generated R always shows the natural form:

```r
n1 <- input - 1   # not: input + -1
```

## Component Registry

The R target uses a component registry to track which utility functions and
packages are needed by the generated code. This avoids duplicating helper
functions when multiple predicates are compiled together.

```prolog
% Track that digest package is needed
:- dynamic required_r_package/1.
required_r_package(digest).

% Generated code includes:
% if (!require(digest)) install.packages("digest", repos="https://cran.r-project.org")
```

## R Identifier Safety

Prolog's internal variable names (like `_G12345`) aren't valid R identifiers
because R names can't start with `_` followed by a digit. The R target's
`ensure_r_identifier/2` predicate handles this:

```prolog
ensure_r_identifier(Name, ValidName) :-
    atom_chars(Name, [First|Rest]),
    (   (First = '_' ; char_type(First, digit)) ->
        atom_chars(ValidName, [v|Rest])  % prefix with 'v'
    ;   ValidName = Name
    ).
```

| Prolog Internal | R Output |
|----------------|----------|
| `_12345` | `v12345` |
| `_g678` | `v678` |
| `n1` | `n1` (unchanged) |

## Adding New R Recursion Patterns

To add support for a new recursion pattern in R, you register a multifile clause
in `r_target.pl`:

```prolog
% 1. Declare the multifile import
:- multifile my_pattern:compile_my_pattern/N.

% 2. Register the R clause
my_pattern:compile_my_pattern(r, PredStr, ..., RCode) :-
    % Generate R code here
    format(string(RCode), '...', [...]).
```

The core module (`my_pattern.pl`) declares:

```prolog
:- multifile compile_my_pattern/N.

% Default bash clause
compile_my_pattern(bash, ...) :- ...

% The R clause is picked up automatically from r_target.pl
```

This is the same pattern used by all six existing recursion patterns.

## Performance Considerations

### Memoization Overhead

R's environment hash tables have constant-time lookup but non-trivial overhead
per access compared to C-level operations. For small inputs (< 100), the memo
overhead may exceed computation cost. For large recursive computations (Fibonacci
of 1000+), memoization provides orders-of-magnitude speedup.

### Vectorization vs Recursion

Where possible, prefer vectorized operations over recursive implementations:

```r
# Recursive (slow for large n)
factorial <- function(n) {
    if (n == 0) return(1)
    return(n * factorial(n - 1))
}

# Vectorized (fast)
factorial_vec <- function(n) prod(seq_len(n))
```

The fold-based linear recursion compiler generates `Reduce()` calls, which are
R's functional equivalent of vectorized folds — faster than explicit recursion.

### Memory

R copies data on modification (copy-on-write semantics). The `<<-` operator used
for memoization mutates the parent environment in place, avoiding copies. This
makes the memoization pattern memory-efficient.

## Integration with Other Targets

Generated R scripts can participate in cross-target pipelines using JSONL as the
interchange format:

```bash
# Bash generates data, R processes it, Python visualizes
bash generate_data.sh | Rscript transform.R | python plot.py
```

Each script reads from stdin and writes to stdout, following Unix pipeline
conventions. The R target's JSONL support (`jsonlite` package) makes this seamless.

---

**<-** [Previous: Bindings](05_bindings.md)
