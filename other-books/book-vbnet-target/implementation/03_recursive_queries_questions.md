<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: Recursive Queries - Questions

Q&A companion for VB.NET recursion pattern code generation.

## Question Index

1. [What recursion patterns are supported for VB.NET?](#bvb03-q-patterns)
2. [How does tail recursion compile to VB.NET?](#bvb03-q-tail)
3. [Why use Do While instead of recursion?](#bvb03-q-dowhile)
4. [How does memoization work in VB.NET?](#bvb03-q-memo)
5. [What is the Dictionary syntax?](#bvb03-q-dictionary)
6. [How does mutual recursion work?](#bvb03-q-mutual)
7. [What is the shared dictionary pattern?](#bvb03-q-shared)
8. [How does VB.NET compare to F# for recursion?](#bvb03-q-comparison)
9. [How are If statements structured?](#bvb03-q-if)
10. [How do I declare types in VB.NET?](#bvb03-q-types)

---

<a id="bvb03-q-patterns"></a>
## Q1: What recursion patterns are supported for VB.NET?

**Question:** What recursion patterns can be compiled to VB.NET?

**Answer:** Three patterns are supported:

| Pattern | API | VB.NET Construct |
|---------|-----|------------------|
| Tail Recursion | `compile_tail_recursion_vbnet/3` | Do While loop |
| Linear Recursion | `compile_linear_recursion_vbnet/3` | Dictionary memo |
| Mutual Recursion | `compile_mutual_recursion_vbnet/3` | Shared Dictionary |

Each uses idiomatic VB.NET constructs.

**See:** [03_recursive_queries_impl.md#recursion-pattern-overview](03_recursive_queries_impl.md#recursion-pattern-overview)

---

<a id="bvb03-q-tail"></a>
## Q2: How does tail recursion compile to VB.NET?

**Question:** What VB.NET code is generated for tail-recursive predicates?

**Answer:** Tail recursion becomes a Do While loop:

```vb
Public Function sum(n As Integer, acc As Integer) As Integer
    Dim current = n
    Dim accumulator = acc

    Do While current > 0
        accumulator += current
        current -= 1
    Loop

    Return accumulator
End Function
```

This achieves O(1) stack space through iteration.

**See:** [03_recursive_queries_impl.md#compile_tail_recursion_vbnet3](03_recursive_queries_impl.md#compile_tail_recursion_vbnet3)

---

<a id="bvb03-q-dowhile"></a>
## Q3: Why use Do While instead of recursion?

**Question:** Why convert tail recursion to a Do While loop?

**Answer:** VB.NET lacks guaranteed tail call optimization:

- Recursive calls consume stack space
- Large inputs can cause stack overflow
- Do While guarantees O(1) stack usage

The iterative version is also more idiomatic for VB.NET.

**See:** [03_recursive_queries_impl.md#why-do-while](03_recursive_queries_impl.md#why-do-while)

---

<a id="bvb03-q-memo"></a>
## Q4: How does memoization work in VB.NET?

**Question:** How is memoization implemented for linear recursion?

**Answer:** A private Dictionary stores computed values:

```vb
Private ReadOnly _memo As New Dictionary(Of Integer, Integer)

Public Function fib(n As Integer) As Integer
    If _memo.ContainsKey(n) Then
        Return _memo(n)
    End If
    ' ... compute and store in _memo
End Function
```

First call: O(n), subsequent calls: O(1).

**See:** [03_recursive_queries_impl.md#compile_linear_recursion_vbnet3](03_recursive_queries_impl.md#compile_linear_recursion_vbnet3)

---

<a id="bvb03-q-dictionary"></a>
## Q5: What is the Dictionary syntax?

**Question:** How do I use Dictionary operations in VB.NET?

**Answer:** Common operations:

| Operation | VB.NET Syntax |
|-----------|---------------|
| Declare | `Dictionary(Of Integer, Integer)` |
| Check | `_memo.ContainsKey(n)` |
| Get | `_memo(n)` |
| Set | `_memo(n) = result` |

Example:
```vb
If _memo.ContainsKey(n) Then Return _memo(n)
_memo(n) = result
```

**See:** [03_recursive_queries_impl.md#dictionary-memoization](03_recursive_queries_impl.md#dictionary-memoization)

---

<a id="bvb03-q-mutual"></a>
## Q6: How does mutual recursion work?

**Question:** How are mutually recursive predicates compiled?

**Answer:** Use `compile_mutual_recursion_vbnet/3`:

```prolog
?- compile_mutual_recursion_vbnet([is_even/1, is_odd/1], [], Code).
```

Generates:
```vb
Public Function is_even(n As Integer) As Boolean
    If n = 0 Then Return True
    If n > 0 Then Return is_odd(n - 1)
    Return False
End Function

Public Function is_odd(n As Integer) As Boolean
    If n = 0 Then Return False
    If n > 0 Then Return is_even(n - 1)
    Return False
End Function
```

**See:** [03_recursive_queries_impl.md#compile_mutual_recursion_vbnet3](03_recursive_queries_impl.md#compile_mutual_recursion_vbnet3)

---

<a id="bvb03-q-shared"></a>
## Q7: What is the shared dictionary pattern?

**Question:** How do I memoize mutually recursive functions?

**Answer:** Use a shared dictionary with composite keys:

```vb
Private ReadOnly _memo As New Dictionary(Of String, Boolean)

Public Function is_even(n As Integer) As Boolean
    Dim key = $"even_{n}"
    If _memo.ContainsKey(key) Then Return _memo(key)
    ' ... compute and store
End Function
```

The string key distinguishes between `is_even` and `is_odd` results.

**See:** [03_recursive_queries_impl.md#shared-dictionary-pattern](03_recursive_queries_impl.md#shared-dictionary-pattern)

---

<a id="bvb03-q-comparison"></a>
## Q8: How does VB.NET compare to F# for recursion?

**Question:** What are the differences between VB.NET and F# recursion patterns?

**Answer:** Comparison:

| Pattern | VB.NET | F# |
|---------|--------|-----|
| Tail Rec | Do While loop | `let rec` inner loop |
| Linear Rec | If + Dictionary | match + Dictionary |
| Mutual Rec | Shared Dictionary | `and` keyword |
| Base Case | If statements | Pattern match |

VB.NET is more verbose but explicit; F# is more concise.

**See:** [03_recursive_queries_impl.md#vbnet-vs-f-comparison](03_recursive_queries_impl.md#vbnet-vs-f-comparison)

---

<a id="bvb03-q-if"></a>
## Q9: How are If statements structured?

**Question:** What If statement patterns are used in the generated code?

**Answer:** Two patterns:

Single-line for simple returns:
```vb
If n <= 0 Then Return 0
If n = 1 Then Return 1
```

Multi-line for complex logic:
```vb
If n = 0 Then
    Return True
ElseIf n > 0 Then
    Return is_odd(n - 1)
Else
    Return False
End If
```

**See:** [03_recursive_queries_impl.md#if-statement-patterns](03_recursive_queries_impl.md#if-statement-patterns)

---

<a id="bvb03-q-types"></a>
## Q10: How do I declare types in VB.NET?

**Question:** What type declaration syntax is used?

**Answer:** Various forms:

Function return types:
```vb
Public Function sum(n As Integer, acc As Integer) As Integer
```

Variable declarations:
```vb
Dim current = n              ' Inferred type
Dim current As Integer = n   ' Explicit type
```

Generic types:
```vb
Dictionary(Of Integer, Integer)   ' Key and value types
Dictionary(Of String, Boolean)
```

**See:** [03_recursive_queries_impl.md#type-declarations](03_recursive_queries_impl.md#type-declarations)
