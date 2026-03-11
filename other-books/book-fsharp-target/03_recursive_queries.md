# Chapter 3: Recursive Queries

This chapter covers recursion patterns for the F# target using functional idioms.

## Recursion Pattern Summary

| Pattern | F# Construct | Multifile Dispatch | Legacy API |
|---------|--------------|-------------------|------------|
| Tail Recursion | `let rec` + accumulator | `tail_recursion:compile_tail_pattern/9` | `compile_tail_recursion_fsharp/3` |
| Linear Recursion | Dictionary memo | `linear_recursion:compile_linear_pattern/8` | `compile_linear_recursion_fsharp/3` |
| Mutual Recursion | `let rec ... and` | `mutual_recursion:compile_mutual_pattern/5` | `compile_mutual_recursion_fsharp/3` |

The F# target registers multifile dispatch clauses with the core recursion analysis modules. When you compile with `target(fsharp)`, the analyzer automatically detects the pattern and dispatches to the F#-specific code generator:

```prolog
?- compile_tail_recursion(test_sum/3, [target(fsharp)], Code).
?- compile_linear_recursion(factorial/2, [target(fsharp)], Code).
```

The legacy target-specific APIs (`compile_tail_recursion_fsharp/3`, etc.) are still available for direct use.

## Tail Recursion

Tail-recursive predicates compile to `let rec` with inner loop:

```prolog
?- compile_tail_recursion(sum/3, [target(fsharp)], Code).
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
?- compile_linear_recursion(fib/2, [target(fsharp)], Code).
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
?- compile_mutual_recursion([is_even/1, is_odd/1], [target(fsharp)], Code).
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
