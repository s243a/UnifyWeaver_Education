# Chapter 3: Recursive Queries

This chapter covers recursion patterns for the VB.NET target.

## Recursion Pattern Summary

| Pattern | VB.NET Construct | API |
|---------|------------------|-----|
| Tail Recursion | Do While loop | `compile_tail_recursion_vbnet/3` |
| Linear Recursion | Dictionary memo | `compile_linear_recursion_vbnet/3` |
| Mutual Recursion | Shared Dictionary | `compile_mutual_recursion_vbnet/3` |

## Tail Recursion

Tail-recursive predicates compile to iterative Do While loops for O(1) stack space:

```prolog
?- compile_tail_recursion_vbnet(sum/2, [], Code).
```

**Generated VB.NET:**
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

## Linear Recursion

Linear recursion uses Dictionary-based memoization for O(n) performance:

```prolog
?- compile_linear_recursion_vbnet(fib/2, [], Code).
```

**Generated VB.NET:**
```vb
Private ReadOnly _memo As New Dictionary(Of Integer, Integer)

Public Function fib(n As Integer) As Integer
    If _memo.ContainsKey(n) Then
        Return _memo(n)
    End If
    
    If n <= 0 Then Return 0
    If n = 1 Then Return 1
    
    Dim result = fib(n - 1) + n
    _memo(n) = result
    Return result
End Function
```

## Mutual Recursion

For predicates that call each other (is_even/is_odd):

```prolog
?- compile_mutual_recursion_vbnet([is_even/1, is_odd/1], [], Code).
```

**Generated VB.NET:**
```vb
Private ReadOnly _memo As New Dictionary(Of String, Boolean)

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

---

**←** [Previous: Chapter 2: Pipeline Mode](02_pipeline_mode) | [📖 Book: VB.NET Target](./)
