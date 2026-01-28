<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: Recursive Queries - Implementation Details

Technical deep-dive for F# recursion pattern code generation.

## Recursion Pattern Overview

| Pattern | F# Construct | API |
|---------|--------------|-----|
| Tail Recursion | `let rec` + inner loop | `compile_tail_recursion_fsharp/3` |
| Linear Recursion | Dictionary memo | `compile_linear_recursion_fsharp/3` |
| Mutual Recursion | `and` keyword | `compile_mutual_recursion_fsharp/3` |

## compile_tail_recursion_fsharp/3

### Signature

```prolog
compile_tail_recursion_fsharp(+PredicateSpec, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `PredicateSpec` | `Name/Arity` | Predicate to compile (e.g., `sum/2`) |
| `Options` | `list` | Generation options |
| `Code` | `string` | Generated F# code |

### Algorithm

1. **Outer Function**: Create public function with parameters
2. **Inner Loop**: Generate `let rec loop` with accumulator
3. **Base Case**: Map Prolog termination to `if ... then acc`
4. **Recursive Case**: Tail-call `loop` with updated values

### Generated Code

```fsharp
let sum n =
    let rec loop current acc =
        if current <= 0 then acc
        else loop (current - 1) (acc + current)
    loop n 0
```

### Why Inner Loop?

The inner function pattern:
- Hides the accumulator from the public API
- Ensures tail-call position for optimization
- Achieves O(1) stack space

## compile_linear_recursion_fsharp/3

### Signature

```prolog
compile_linear_recursion_fsharp(+PredicateSpec, +Options, -Code)
```

### Algorithm

1. **Memo Dictionary**: Create `Dictionary<int, int>` for caching
2. **Lookup Pattern**: Use `TryGetValue` with pattern matching
3. **Base Cases**: Map to pattern match arms
4. **Recursive Case**: Compute, store, and return

### Generated Code

```fsharp
let private memo = Dictionary<int, int>()

let rec fib n =
    match memo.TryGetValue(n) with
    | true, v -> v
    | false, _ ->
        let result =
            match n with
            | n when n <= 0 -> 0
            | 1 -> 1
            | n -> (fib (n - 1)) + n
        memo.[n] <- result
        result
```

### TryGetValue Pattern

F# idiomatically deconstructs the `TryGetValue` tuple:

```fsharp
match memo.TryGetValue(key) with
| true, value -> value       // Found
| false, _ -> compute()      // Not found
```

This is cleaner than:
```fsharp
if memo.ContainsKey(key) then memo.[key]
else ...
```

## compile_mutual_recursion_fsharp/3

### Signature

```prolog
compile_mutual_recursion_fsharp(+PredicateList, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `PredicateList` | `list` | List of predicates (e.g., `[is_even/1, is_odd/1]`) |
| `Options` | `list` | Generation options |
| `Code` | `string` | Generated F# code |

### F# `and` Keyword

F# requires `and` to define mutually recursive functions:

```fsharp
let rec is_even n =
    match n with
    | 0 -> true
    | n when n > 0 -> is_odd (n - 1)
    | _ -> false

and is_odd n =
    match n with
    | 0 -> false
    | n when n > 0 -> is_even (n - 1)
    | _ -> false
```

### Why `and`?

Without `and`, F# compilation fails:
```fsharp
let rec is_even n = ... is_odd ...  // Error: is_odd not defined
let rec is_odd n = ... is_even ...
```

The `and` keyword tells F# to resolve both names simultaneously.

## Pattern Matching

### Guards with `when`

```fsharp
match n with
| n when n <= 0 -> 0    // Guard clause
| 1 -> 1                 // Exact match
| n -> fib (n - 1) + n  // Default binding
```

### Wildcard `_`

```fsharp
match memo.TryGetValue(n) with
| true, v -> v
| false, _ -> ...  // Ignore second element
```

## F# vs VB.NET Comparison

| Pattern | F# | VB.NET |
|---------|-----|--------|
| Tail Rec | `let rec` inner loop | Do While loop |
| Linear Rec | match + Dictionary | If + Dictionary |
| Mutual Rec | `and` keyword | Shared Dictionary |
| Base Case | Pattern match | If statements |

## Tail Call Optimization

### How F# Handles Tail Calls

F# compiler recognizes tail position and:
1. Replaces recursive call with `goto`
2. Reuses stack frame
3. Achieves O(1) space

### Tail Position Requirements

```fsharp
// Tail recursive - result is directly returned
let rec loop n acc =
    if n <= 0 then acc
    else loop (n - 1) (acc + n)  // Tail position

// NOT tail recursive - addition after call
let rec sum n =
    if n <= 0 then 0
    else n + sum (n - 1)  // Not tail position
```

## Performance Characteristics

| Pattern | Time | Space | Memoized |
|---------|------|-------|----------|
| Tail Recursion | O(n) | O(1) | No |
| Linear Recursion | O(n)* | O(n) | Yes |
| Mutual Recursion | O(n) | O(n) | Optional |

*First call O(n), subsequent O(1) due to memoization.

## Source Files

- `src/unifyweaver/targets/fsharp_target.pl`
