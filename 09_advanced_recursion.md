# Chapter 9: Advanced Recursion Patterns

## Introduction

While the basic `recursive_compiler` can handle simple transitive closures, the true power of UnifyWeaver lies in its `advanced_recursive_compiler`. This component can identify more complex and specific patterns of recursion and compile them into highly optimized Bash code.

This chapter explores the main advanced recursion patterns that UnifyWeaver understands: **tail recursion**, **linear recursion**, and **mutual recursion**.

## 1. Tail Recursion Optimization

Tail recursion is a special form of recursion where the recursive call is the very last action performed in a function. There is no other work to be done after the recursive call returns.

**Why it matters:** A tail-recursive function can be transformed into a simple iterative loop (like a `while` or `for` loop). This is a major optimization because it avoids the overhead of function calls and eliminates the risk of running out of stack space ("stack overflow") on deep recursions.

### The Pattern: Accumulators

The most common tail-recursive pattern involves an **accumulator**. This is an extra argument that carries the intermediate result down through the recursive calls.

**Example: A tail-recursive counter**

```prolog
% count_items(List, Accumulator, FinalCount)

% Base Case: When the list is empty, the final count is the value
% of the accumulator.
count_items([], Acc, Acc).

% Recursive Step: For a non-empty list, increment the accumulator
% and recurse on the tail of the list.
count_items([_|T], Acc, N) :-
    Acc1 is Acc + 1,
    count_items(T, Acc1, N). % This is the tail call
```

UnifyWeaver's `pattern_matchers` module is specifically designed to recognize this accumulator pattern.

### Generated Code: An Iterative Loop

When the `advanced_recursive_compiler` sees this pattern, it doesn't generate a recursive Bash function. Instead, it generates a highly efficient `for` loop.

**Conceptual Generated Bash:**
```bash
count_items() {
    local input_list=$1
    local initial_acc=$2
    
    local current_acc="$initial_acc"

    # The recursion is transformed into a simple loop!
    for item in ${input_list[@]}; do
        current_acc=$((current_acc + 1))
    done

    echo "$current_acc"
}
```
This transformation from a declarative recursive definition to an imperative loop is one of UnifyWeaver's most powerful optimizations.

## 2. Linear Recursion

Linear recursion occurs when a function calls itself at most once in any of its branches. The classic example is calculating the length of a list without an accumulator.

**Example: A linear-recursive length function**
```prolog
% length(List, Length)

% Base Case: The length of an empty list is 0.
length([], 0).

% Recursive Step: The length of a list is 1 + the length of its tail.
length([_|T], N) :-
    length(T, N1), % The single recursive call
    N is N1 + 1.
```
Notice this is **not** tail-recursive, because an operation (`N is N1 + 1`) happens *after* the recursive call `length(T, N1)` returns.

### Generated Code: Memoization

For linear recursion, UnifyWeaver's strategy is **memoization**. It generates a Bash function that uses a global associative array to cache its results. The first time the function is called with a given input, it computes the result and saves it. On any subsequent call with the same input, it returns the cached result instantly without re-computing.

**Conceptual Generated Bash:**
```bash
# Global associative array for memoization
declare -gA length_memo

length() {
    local input_list=$1
    local key="$input_list"

    # 1. Check if the result is already in the cache
    if [[ -n "${length_memo[$key]}" ]]; then
        echo "${length_memo[$key]}"
        return
    fi

    # 2. If not, compute it (handling base and recursive cases)
    # ... logic to compute the length ...
    local result=...

    # 3. Store the new result in the cache before returning
    length_memo["$key"]="$result"
    echo "$result"
}
```
This is extremely effective for functions like `factorial` or `fibonacci` where the same sub-problems are computed many times.

## 3. Mutual Recursion

Mutual recursion happens when two or more functions call each other in a cycle. The classic example is determining if a number is even or odd.

**Example: `is_even/1` and `is_odd/1`**
```prolog
is_even(0).
is_even(N) :- N > 0, N1 is N - 1, is_odd(N1).

is_odd(1).
is_odd(N) :- N > 1, N1 is N - 1, is_even(N1).
```
Here, `is_even` calls `is_odd`, and `is_odd` calls `is_even`.

### Detection: Strongly Connected Components (SCC)

UnifyWeaver handles this complex case by performing a graph analysis of your code:
1.  **Build a Call Graph:** It first builds a graph where the nodes are your predicates and an edge `A -> B` means `A` calls `B`.
2.  **Find Strongly Connected Components (SCCs):** It then uses Tarjan's algorithm to find all SCCs in the graph. An SCC is a group of nodes where every node is reachable from every other node in the group. A non-trivial SCC (with more than one node) represents a group of mutually recursive predicates.

### Generated Code: Shared Memoization Table

Once an SCC is identified, the compiler generates Bash functions for all predicates in the group, and they all share a **single memoization table**. This is crucial for efficiency and correctness.

**Conceptual Generated Bash:**
```bash
# A single, shared cache for the even/odd group
declare -gA even_odd_scc_memo

is_even() {
    local key="is_even:$1"
    # Check the SHARED cache
    if [[ -n "${even_odd_scc_memo[$key]}" ]]; then ... ; fi

    # ... compute, calling is_odd() ...

    # Store result in the SHARED cache
    even_odd_scc_memo["$key"]="$result"
    echo "$result"
}

is_odd() {
    local key="is_odd:$1"
    # Check the SHARED cache
    if [[ -n "${even_odd_scc_memo[$key]}" ]]; then ... ; fi

    # ... compute, calling is_even() ...

    # Store result in the SHARED cache
    even_odd_scc_memo["$key"]="$result"
    echo "$result"
}
```

## Summary

The advanced recursion compiler elevates UnifyWeaver from a simple code generator to a true optimizing compiler. By recognizing these common recursion patterns, it can produce Bash code that is far more performant and robust than a naive translation would be.

| Recursion Pattern | UnifyWeaver's Strategy | Key Benefit |
|:---|:---|:---|
| **Tail Recursion** | Iterative Loop | O(1) stack space, very fast |
| **Linear Recursion** | Memoization | Avoids re-computing sub-problems |
| **Mutual Recursion** | SCC Analysis + Shared Memo | Correctly handles complex cycles |
