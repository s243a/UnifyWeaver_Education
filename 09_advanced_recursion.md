# Chapter 9: Advanced Recursion Patterns

## Introduction

While the basic `recursive_compiler` can handle simple transitive closures, the true power of UnifyWeaver lies in its `advanced_recursive_compiler`. This component can identify more complex and specific patterns of recursion and compile them into highly optimized Bash code.

This chapter explores the main advanced recursion patterns that UnifyWeaver understands: **tail recursion**, **linear recursion**, **tree recursion**, and **mutual recursion**.

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

Linear recursion occurs when a function has one or more recursive calls in its body, but those calls are **independent** of each other. This means the arguments to the recursive calls are computed before the calls are made, and the results of the calls are simply aggregated at the end.

The classic example is the `fibonacci` function.

**Example: A linear-recursive fibonacci function**
```prolog
% fib(N, F)

% Base Cases
fib(0, 0).
fib(1, 1).

% Recursive Step: Two independent recursive calls
fib(N, F) :-
    N > 1,
    N1 is N - 1,
    N2 is N - 2,
    fib(N1, F1),
    fib(N2, F2),
    F is F1 + F2.
```
Notice that `fib(N1, F1)` and `fib(N2, F2)` are independent. The calculation of `F1` does not affect the calculation of `F2`. This independence is what allows UnifyWeaver to use memoization effectively.

### Generated Code: Memoization

For linear recursion, UnifyWeaver's strategy is **memoization**. It generates a Bash function that uses a global associative array to cache its results. The first time the function is called with a given input, it computes the result and saves it. On any subsequent call with the same input, it returns the cached result instantly without re-computing.

**Conceptual Generated Bash for `fib`:**
```bash
# Global associative array for memoization
declare -gA fib_memo

fib() {
    local n=$1
    local key=$n

    # 1. Check if the result is already in the cache
    if [[ -n "${fib_memo[$key]}" ]]; then
        echo "${fib_memo[$key]}"
        return
    fi

    # 2. If not, compute it
    if (( n <= 1 )); then
        result=$n
    else
        # Two independent recursive calls
        f1=$(fib $((n - 1)))
        f2=$(fib $((n - 2)))
        result=$((f1 + f2))
    fi

    # 3. Store the new result in the cache before returning
    fib_memo["$key"]="$result"
    echo "$result"
}
```
This is extremely effective for functions like `fibonacci` where the same sub-problems are computed many times.

## 3. Tree Recursion

Tree recursion is used for problems where a function operates on a data structure, and the recursive calls are made on smaller parts of that structure. This is common in problems involving trees or nested lists.

The key characteristic of tree recursion is **structural decomposition**. The input is broken down into its constituent parts, and the function is called recursively on those parts.

**Example: A tree-recursive sum function**
```prolog
% tree_sum(Tree, Sum)
% Tree is represented as [Value, LeftSubtree, RightSubtree]

% Base Case: The sum of an empty tree is 0.
tree_sum([], 0).

% Recursive Step: The sum of a tree is the value of the root
% plus the sum of the left and right subtrees.
tree_sum([V, L, R], Sum) :-
    tree_sum(L, LS),
    tree_sum(R, RS),
    Sum is V + LS + RS.
```
In this example, the `tree_sum` function is called on the left (`L`) and right (`R`) subtrees, which are parts of the original tree structure.

### Generated Code: Structure Parsing

For tree recursion, UnifyWeaver generates a Bash function that can parse the tree structure. It does not use memoization by default for tree recursion.

**Conceptual Generated Bash for `tree_sum`:**
```bash
tree_sum() {
    local tree_str=$1

    # Base case
    if [[ -z "$tree_str" || "$tree_str" == "[]" ]]; then
        echo 0
        return
    fi

    # Parse the tree structure
    # (This is a simplified representation of what the compiler does)
    local value=$(echo "$tree_str" | cut -d',' -f1 | tr -d '[]')
    local left_subtree=$(echo "$tree_str" | cut -d',' -f2- | sed 's/.*,\[/[[/;s/\].*/]/')
    local right_subtree=$(echo "$tree_str" | cut -d',' -f3- | sed 's/.*,\[/[[/;s/\].*/]/')

    # Recursive calls on the subtrees
    local left_sum=$(tree_sum "$left_subtree")
    local right_sum=$(tree_sum "$right_subtree")

    echo $((value + left_sum + right_sum))
}
```

## 4. Forbidding Linear Recursion

Sometimes, a predicate might look like it fits the linear recursion pattern, but you want to prevent UnifyWeaver from compiling it that way. For example, you might want to force a predicate to be compiled using the tree recursion strategy to handle a specific graph traversal algorithm.

UnifyWeaver provides a way to do this using the `forbid_linear_recursion/1` directive.

**Example: Forcing a different compilation strategy**
```prolog
% This predicate could be interpreted as linear recursion.
my_graph_traversal(Node, Value) :- ...

% Forbid linear recursion for this predicate.
:- forbid_linear_recursion(my_graph_traversal/2).
```

When UnifyWeaver sees this directive, it will not consider the linear recursion pattern for the `my_graph_traversal/2` predicate. It will then try to match other patterns, such as tree recursion.

This feature gives you more control over the compilation process and allows you to guide the compiler to the most appropriate strategy for your specific problem.

## 5. Mutual Recursion

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
| **Linear Recursion** | Memoization | Avoids re-computing sub-problems in independent calls |
| **Tree Recursion** | Structure Parsing | Handles recursive decomposition of data structures |
| **Mutual Recursion** | SCC Analysis + Shared Memo | Correctly handles complex cycles |
