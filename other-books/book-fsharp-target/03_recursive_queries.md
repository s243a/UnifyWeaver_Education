# Chapter 3: Recursive Queries

This chapter covers recursion patterns for the F# target using functional idioms.

## Recursion Pattern Summary

| Pattern | F# Construct | API |
|---------|--------------|-----|
| Tail Recursion | `let rec` + accumulator | `compile_tail_recursion_fsharp/3` |
| Linear Recursion | Dictionary memo | `compile_linear_recursion_fsharp/3` |
| Mutual Recursion | `let rec ... and` | `compile_mutual_recursion_fsharp/3` |

## Tail Recursion

Tail-recursive predicates compile to `let rec` with inner loop:

```prolog
?- compile_tail_recursion_fsharp(sum/2, [], Code).
```

**Generated F#:**
```fsharp
let sum n =
    let rec loop current acc =
        if current <= 0 then acc
        else loop (current - 1) (acc + current)
    loop n 0
```

This achieves O(1) stack space through tail call optimization.

## Linear Recursion

Linear recursion uses Dictionary memoization with pattern matching:

```prolog
?- compile_linear_recursion_fsharp(fib/2, [], Code).
```

**Generated F#:**
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

## Mutual Recursion

F# natively supports mutual recursion with the `and` keyword:

```prolog
?- compile_mutual_recursion_fsharp([is_even/1, is_odd/1], [], Code).
```

**Generated F#:**
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

The `and` keyword makes mutual recursion first-class in F#.

## F# vs VB.NET Comparison

| Pattern | F# | VB.NET |
|---------|-----|--------|
| Tail Rec | `let rec` inner loop | Do While |
| Linear Rec | match + Dictionary | If + Dictionary |
| Mutual Rec | `and` keyword | Shared Dictionary |

---

**←** [Previous: Chapter 2: Pipeline Mode](02_pipeline_mode) | [📖 Book: F# Target](./)
