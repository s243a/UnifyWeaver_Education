<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->
# Chapter 2: Facts and Rules

## Facts: Prolog Data to R Lists

Prolog facts compile to R lists of character vectors with three accessor functions.

### Source Prolog

```prolog
parent(tom, bob).
parent(bob, jim).
parent(jim, ann).
```

### Compilation

```prolog
?- compile_facts_to_r(parent, 2, Code), write(Code).
```

### Generated R

```r
parent_data <- list(
  c("tom", "bob"),
  c("bob", "jim"),
  c("jim", "ann")
)

parent_get_all <- function() parent_data

parent_stream <- function() parent_data

parent_contains <- function(x, y) {
  any(sapply(parent_data, function(p) p[1] == x && p[2] == y))
}
```

### Using It

```r
source("parent.R")

# Get all facts
parent_get_all()
# [[1]] [1] "tom" "bob"
# [[2]] [1] "bob" "jim"
# [[3]] [1] "jim" "ann"

# Check membership
parent_contains("tom", "bob")
# [1] TRUE

parent_contains("tom", "ann")
# [1] FALSE
```

## Rules: Prolog Clauses to R Functions

Prolog rules with multiple clauses compile to R functions with `if`/`else if`
chains. Each clause becomes a conditional branch.

### Source Prolog

```prolog
greet(hello, "Hello, World!").
greet(goodbye, "Goodbye, World!").
greet(_, "I don't understand.").
```

### Compilation

```prolog
?- compile_predicate_to_r(greet/2, [], Code), write(Code).
```

### Generated R

```r
greet <- function(arg1) {
  if (arg1 == "hello") {
    return("Hello, World!")
  } else if (arg1 == "goodbye") {
    return("Goodbye, World!")
  } else {
    return("I don't understand.")
  }
}
```

## Numeric Facts

Facts with numeric values work the same way:

### Source Prolog

```prolog
age(alice, 30).
age(bob, 25).
age(charlie, 35).
```

### Generated R

```r
age_data <- list(
  c("alice", 30),
  c("bob", 25),
  c("charlie", 35)
)

age_get_all <- function() age_data
age_stream <- function() age_data
age_contains <- function(x, y) {
  any(sapply(age_data, function(p) p[1] == x && p[2] == y))
}
```

## Data Frame Construction

For tabular data, R's strength is the data frame. UnifyWeaver can generate data
frame representations from facts:

```r
# Convert fact list to data frame
parent_df <- do.call(rbind, lapply(parent_data, function(p) {
  data.frame(from = p[1], to = p[2], stringsAsFactors = FALSE)
}))

#   from  to
# 1  tom bob
# 2  bob jim
# 3  jim ann
```

This bridges the gap between Prolog's relational model and R's tabular model,
enabling downstream operations like `subset()`, `merge()`, and `aggregate()`.

## Expected Value Checking

Generated R functions support an optional `expected` parameter for verification
mode, matching UnifyWeaver's streaming protocol:

```r
factorial <- function(n, expected=NULL) {
    # ... compute result ...

    if (!is.null(expected)) {
        if (result == expected) return(TRUE) else return(FALSE)
    } else {
        return(result)
    }
}
```

```r
factorial(6)          # → 720
factorial(6, 720)     # → TRUE (verification mode)
factorial(6, 100)     # → FALSE
```

This allows generated R functions to participate in UnifyWeaver's test infrastructure,
where predicates are checked against known outputs.

---

**<-** [Previous: Introduction](01_introduction.md) | **->** [Next: Data Manipulation](03_data_manipulation.md)
