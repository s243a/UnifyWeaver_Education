<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: Recursive Queries - Questions

Q&A companion for F# recursion pattern code generation.

## Question Index

1. [What recursion patterns are supported for F#?](#bfs03-q-patterns)
2. [How does tail recursion compile to F#?](#bfs03-q-tail)
3. [Why use an inner loop function?](#bfs03-q-inner-loop)
4. [How does memoization work in F#?](#bfs03-q-memo)
5. [What is the TryGetValue pattern?](#bfs03-q-trygetvalue)
6. [How does mutual recursion work in F#?](#bfs03-q-mutual)
7. [Why is the `and` keyword required?](#bfs03-q-and-keyword)
8. [How do guards work in pattern matching?](#bfs03-q-guards)
9. [How does F# optimize tail calls?](#bfs03-q-tco)
10. [How does F# compare to VB.NET for recursion?](#bfs03-q-comparison)

---

<a id="bfs03-q-patterns"></a>
## Q1: What recursion patterns are supported for F#?

**Question:** What recursion patterns can be compiled to F#?

**Answer:** Three patterns are supported:

| Pattern | API | F# Construct |
|---------|-----|--------------|
| Tail Recursion | `compile_tail_recursion_fsharp/3` | `let rec` + inner loop |
| Linear Recursion | `compile_linear_recursion_fsharp/3` | Dictionary memo |
| Mutual Recursion | `compile_mutual_recursion_fsharp/3` | `and` keyword |

Each uses idiomatic F# constructs for optimal performance.

**See:** [03_recursive_queries_impl.md#recursion-pattern-overview](03_recursive_queries_impl.md#recursion-pattern-overview)

---

<a id="bfs03-q-tail"></a>
## Q2: How does tail recursion compile to F#?

**Question:** What F# code is generated for tail-recursive predicates?

**Answer:** Tail recursion generates an inner loop with accumulator:

```fsharp
let sum n =
    let rec loop current acc =
        if current <= 0 then acc
        else loop (current - 1) (acc + current)
    loop n 0
```

This achieves O(1) stack space through tail call optimization.

**See:** [03_recursive_queries_impl.md#compile_tail_recursion_fsharp3](03_recursive_queries_impl.md#compile_tail_recursion_fsharp3)

---

<a id="bfs03-q-inner-loop"></a>
## Q3: Why use an inner loop function?

**Question:** Why does tail recursion use an inner `loop` function?

**Answer:** The inner function pattern provides:

1. **Clean API**: Hides the accumulator from callers
2. **Tail position**: Ensures recursive call is in tail position
3. **Optimization**: Enables F# compiler to apply TCO

Public API remains simple:
```fsharp
let result = sum 100  // No accumulator needed
```

**See:** [03_recursive_queries_impl.md#why-inner-loop](03_recursive_queries_impl.md#why-inner-loop)

---

<a id="bfs03-q-memo"></a>
## Q4: How does memoization work in F#?

**Question:** How is memoization implemented for linear recursion?

**Answer:** A private Dictionary stores computed values:

```fsharp
let private memo = Dictionary<int, int>()

let rec fib n =
    match memo.TryGetValue(n) with
    | true, v -> v
    | false, _ ->
        let result = match n with ...
        memo.[n] <- result
        result
```

First call: O(n), subsequent calls: O(1).

**See:** [03_recursive_queries_impl.md#compile_linear_recursion_fsharp3](03_recursive_queries_impl.md#compile_linear_recursion_fsharp3)

---

<a id="bfs03-q-trygetvalue"></a>
## Q5: What is the TryGetValue pattern?

**Question:** How does F# idiomatically check dictionary membership?

**Answer:** Pattern match on the `TryGetValue` tuple:

```fsharp
match memo.TryGetValue(key) with
| true, value -> value   // Found: use cached value
| false, _ -> compute()  // Not found: compute it
```

This is more idiomatic than:
```fsharp
if memo.ContainsKey(key) then memo.[key] else ...
```

**See:** [03_recursive_queries_impl.md#trygetvalue-pattern](03_recursive_queries_impl.md#trygetvalue-pattern)

---

<a id="bfs03-q-mutual"></a>
## Q6: How does mutual recursion work in F#?

**Question:** How are mutually recursive predicates compiled?

**Answer:** Use `compile_mutual_recursion_fsharp/3` with a list of predicates:

```prolog
?- compile_mutual_recursion_fsharp([is_even/1, is_odd/1], [], Code).
```

Generates:
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

**See:** [03_recursive_queries_impl.md#compile_mutual_recursion_fsharp3](03_recursive_queries_impl.md#compile_mutual_recursion_fsharp3)

---

<a id="bfs03-q-and-keyword"></a>
## Q7: Why is the `and` keyword required?

**Question:** Why must mutually recursive functions use `and`?

**Answer:** F# processes definitions sequentially. Without `and`:

```fsharp
let rec is_even n = ... is_odd ...  // Error: is_odd undefined
let rec is_odd n = ... is_even ...
```

The `and` keyword tells F# to resolve both names together:

```fsharp
let rec is_even n = ...
and is_odd n = ...  // Both resolved simultaneously
```

**See:** [03_recursive_queries_impl.md#why-and](03_recursive_queries_impl.md#why-and)

---

<a id="bfs03-q-guards"></a>
## Q8: How do guards work in pattern matching?

**Question:** How do I add conditions to pattern match cases?

**Answer:** Use `when` for guard clauses:

```fsharp
match n with
| n when n <= 0 -> 0    // Guard: n <= 0
| 1 -> 1                 // Exact match
| n -> fib (n - 1) + n  // Binding + default
```

Guards allow arbitrary boolean conditions beyond structural matching.

**See:** [03_recursive_queries_impl.md#guards-with-when](03_recursive_queries_impl.md#guards-with-when)

---

<a id="bfs03-q-tco"></a>
## Q9: How does F# optimize tail calls?

**Question:** How does the F# compiler handle tail call optimization?

**Answer:** F# recognizes tail position and:

1. Replaces recursive call with `goto`
2. Reuses the current stack frame
3. Achieves O(1) space complexity

Tail position requirement:
```fsharp
// Tail recursive - direct return
else loop (n - 1) (acc + n)

// NOT tail recursive - operation after call
else n + sum (n - 1)  // Addition after call
```

**See:** [03_recursive_queries_impl.md#tail-call-optimization](03_recursive_queries_impl.md#tail-call-optimization)

---

<a id="bfs03-q-comparison"></a>
## Q10: How does F# compare to VB.NET for recursion?

**Question:** What are the differences between F# and VB.NET recursion patterns?

**Answer:** Comparison:

| Pattern | F# | VB.NET |
|---------|-----|--------|
| Tail Rec | `let rec` inner loop | Do While loop |
| Linear Rec | match + Dictionary | If + Dictionary |
| Mutual Rec | `and` keyword | Shared Dictionary |
| Base Case | Pattern match | If statements |

F# is more concise due to pattern matching and first-class mutual recursion support.

**See:** [03_recursive_queries_impl.md#f-vs-vbnet-comparison](03_recursive_queries_impl.md#f-vs-vbnet-comparison)
