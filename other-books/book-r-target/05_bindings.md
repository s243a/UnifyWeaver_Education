<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->
# Chapter 5: Bindings

The R bindings module (`r_bindings.pl`) maps 70+ Prolog built-ins to their R
equivalents. These mappings are used during code generation to translate Prolog
calls into idiomatic R function calls.

## Binding Categories

### Core Built-ins

| Prolog | R | Notes |
|--------|---|-------|
| `length(List, N)` | `length(list)` | Returns vector length |
| `print(X)` | `print(x)` | Print with newline |
| `cat(X)` | `cat(x)` | Print without newline |
| `true` | `TRUE` | Logical true |
| `fail` | `FALSE` | Logical false |

### Math Operations (Vectorized)

All math operations in R operate element-wise on vectors automatically.

| Prolog | R | Category |
|--------|---|----------|
| `sum(List, S)` | `sum(vec)` | Aggregation |
| `mean(List, M)` | `mean(vec)` | Aggregation |
| `min(List, M)` | `min(vec)` | Aggregation |
| `max(List, M)` | `max(vec)` | Aggregation |
| `abs(X, Y)` | `abs(x)` | Elementwise |
| `sqrt(X, Y)` | `sqrt(x)` | Elementwise |
| `floor(X, Y)` | `floor(x)` | Elementwise |
| `ceiling(X, Y)` | `ceiling(x)` | Elementwise |
| `round(X, Y)` | `round(x)` | Elementwise |
| `log(X, Y)` | `log(x)` | Elementwise |
| `log10(X, Y)` | `log10(x)` | Elementwise |
| `exp(X, Y)` | `exp(x)` | Elementwise |
| `sin(X, Y)` | `sin(x)` | Trigonometry |
| `cos(X, Y)` | `cos(x)` | Trigonometry |
| `tan(X, Y)` | `tan(x)` | Trigonometry |

Example — vectorized square root:

```prolog
% Prolog
sqrt(X, Y).
```

```r
# R (operates on entire vector)
y <- sqrt(x)

# Works on scalars and vectors alike:
sqrt(16)       # → 4
sqrt(c(4,9,16)) # → c(2, 3, 4)
```

### String Operations (Vectorized)

| Prolog | R | Notes |
|--------|---|-------|
| `atom_concat(A, B, C)` | `paste0(a, b)` | No separator |
| `atom_length(A, N)` | `nchar(a)` | Character count |
| `atom_string(A, S)` | `as.character(a)` | To string |
| `sub_atom(A, B, _, _, Sub)` | `substr(a, b, ...)` | Substring |
| `split_string(S, Sep, _, Parts)` | `strsplit(s, sep)` | Split |
| `upcase_atom(A, U)` | `toupper(a)` | Uppercase |
| `downcase_atom(A, D)` | `tolower(a)` | Lowercase |
| `format(atom(R), Fmt, Args)` | `sprintf(fmt, ...)` | Format |

R string functions are vectorized — they operate on character vectors without loops:

```r
toupper(c("hello", "world"))  # → c("HELLO", "WORLD")
nchar(c("hi", "there"))       # → c(2, 5)
```

### Pattern Matching

| Prolog | R | Notes |
|--------|---|-------|
| `re_match(Pattern, String)` | `grepl(pattern, string)` | Returns TRUE/FALSE |
| `re_replace(Pattern, Repl, String, Out)` | `gsub(pattern, repl, string)` | Global replace |
| `re_split(Pattern, String, Parts)` | `strsplit(string, pattern)` | Split by regex |

### Type Conversion (Vectorized)

| Prolog | R | Notes |
|--------|---|-------|
| `number_codes(N, Codes)` | `as.numeric(x)` | To number |
| `integer(X)` | `as.integer(x)` | To integer |
| `atom_number(A, N)` | `as.numeric(as.character(a))` | Atom to number |
| `number(X)` | `is.numeric(x)` | Type check |
| `atom(X)` | `is.character(x)` | Type check |

### Vector/List Operations

| Prolog | R | Notes |
|--------|---|-------|
| `append(A, B, C)` | `c(a, b)` | Concatenate |
| `reverse(List, Rev)` | `rev(list)` | Reverse |
| `msort(List, Sorted)` | `sort(list)` | Sort |
| `list_to_set(List, Set)` | `unique(list)` | Remove duplicates |
| `member(X, List)` | `x %in% list` | Membership test |
| `nth0(N, List, Elem)` | `list[[n+1]]` | Index (0-based) |
| `nth1(N, List, Elem)` | `list[[n]]` | Index (1-based) |
| `last(List, Elem)` | `tail(list, 1)` | Last element |
| `select(Elem, List, Rest)` | `setdiff(list, elem)` | Remove element |

### File I/O

| Prolog | R | Notes |
|--------|---|-------|
| `exists_file(Path)` | `file.exists(path)` | Check existence |
| `file_directory_name(P, D)` | `dirname(path)` | Parent directory |
| `file_base_name(P, B)` | `basename(path)` | File name |
| `read_file_to_string(P, S, _)` | `readLines(path)` | Read file |
| `write_to_file(P, Content)` | `writeLines(content, path)` | Write file |

### Data Frame Operations

| Prolog | R | Notes |
|--------|---|-------|
| `filter(DF, Expr, Out)` | `subset(df, expr)` | Row filtering |
| `sort_by(DF, Col, Out)` | `df[order(df$col), ]` | Sort by column |
| `group_by(DF, Col, Out)` | `aggregate(. ~ col, df, FUN)` | Group + aggregate |

## Arithmetic Operator Mapping

Prolog arithmetic expressions are translated to R operators:

| Prolog | R |
|--------|---|
| `X + Y` | `x + y` |
| `X - Y` | `x - y` |
| `X * Y` | `x * y` |
| `X / Y` | `x / y` |
| `X mod Y` | `x %% y` |
| `X ^ Y` | `x ^ y` |
| `X // Y` | `x %/% y` (integer division) |

## Comparison Operator Mapping

| Prolog | R |
|--------|---|
| `X > Y` | `x > y` |
| `X < Y` | `x < y` |
| `X >= Y` | `x >= y` |
| `X =< Y` | `x <= y` |
| `X =:= Y` | `x == y` |
| `X =\= Y` | `x != y` |

## Logical Operators

| Prolog | R | Notes |
|--------|---|-------|
| `A, B` (conjunction) | `a && b` or `a & b` | Short-circuit vs vectorized |
| `A ; B` (disjunction) | `a \|\| b` or `a \| b` | Short-circuit vs vectorized |
| `\+ X` (negation) | `!x` | Logical NOT |

## Required Packages

Some bindings require optional R packages:

| Package | Used By | Install |
|---------|---------|---------|
| `digest` | Tree recursion memoization | `install.packages("digest")` |
| `jsonlite` | JSONL pipeline I/O | `install.packages("jsonlite")` |
| `data.table` | High-performance data ops | `install.packages("data.table")` |

The R target tracks required packages via `required_r_package/1` and can generate
`if (!require(...))` installation checks at the top of generated scripts.

---

**<-** [Previous: Recursion Patterns](04_recursion_patterns.md) | **->** [Next: Advanced Features](06_advanced_features.md)
