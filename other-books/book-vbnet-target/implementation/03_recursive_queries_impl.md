<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: Recursive Queries - Implementation Details

Technical deep-dive for VB.NET recursion pattern code generation.

## Recursion Pattern Overview

| Pattern | VB.NET Construct | API |
|---------|------------------|-----|
| Tail Recursion | Do While loop | `compile_tail_recursion_vbnet/3` |
| Linear Recursion | Dictionary memo | `compile_linear_recursion_vbnet/3` |
| Mutual Recursion | Shared Dictionary | `compile_mutual_recursion_vbnet/3` |

## compile_tail_recursion_vbnet/3

### Signature

```prolog
compile_tail_recursion_vbnet(+PredicateSpec, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `PredicateSpec` | `Name/Arity` | Predicate to compile (e.g., `sum/2`) |
| `Options` | `list` | Generation options |
| `Code` | `string` | Generated VB.NET code |

### Algorithm

1. **Function Signature**: Create `Public Function` with parameters
2. **Local Variables**: Declare mutable copies for iteration
3. **Do While Loop**: Convert tail recursion to iteration
4. **Return Statement**: Return accumulator

### Generated Code

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

### Why Do While?

VB.NET lacks guaranteed tail call optimization. Converting to iteration:
- Guarantees O(1) stack space
- Avoids stack overflow on large inputs
- Matches imperative VB.NET style

## compile_linear_recursion_vbnet/3

### Signature

```prolog
compile_linear_recursion_vbnet(+PredicateSpec, +Options, -Code)
```

### Algorithm

1. **Private Dictionary**: Create `Dictionary(Of Integer, Integer)`
2. **ContainsKey Check**: Return cached value if present
3. **Base Cases**: Map to If statements
4. **Recursive Case**: Compute, store, and return

### Generated Code

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

### Dictionary Memoization

| Operation | VB.NET Syntax |
|-----------|---------------|
| Check | `_memo.ContainsKey(n)` |
| Get | `_memo(n)` |
| Set | `_memo(n) = result` |

### ReadOnly vs Private

```vb
Private ReadOnly _memo As New Dictionary(Of Integer, Integer)
```

- `Private`: Class-level scope
- `ReadOnly`: Reference can't change (but contents can)
- `New`: Initialize inline

## compile_mutual_recursion_vbnet/3

### Signature

```prolog
compile_mutual_recursion_vbnet(+PredicateList, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `PredicateList` | `list` | List of predicates (e.g., `[is_even/1, is_odd/1]`) |
| `Options` | `list` | Generation options |
| `Code` | `string` | Generated VB.NET code |

### Generated Code

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

### Shared Dictionary Pattern

For mutual recursion with memoization, use a shared dictionary with composite keys:

```vb
Private ReadOnly _memo As New Dictionary(Of String, Boolean)

Public Function is_even(n As Integer) As Boolean
    Dim key = $"even_{n}"
    If _memo.ContainsKey(key) Then Return _memo(key)
    ' ... compute and store
End Function
```

## VB.NET vs F# Comparison

| Pattern | VB.NET | F# |
|---------|--------|-----|
| Tail Rec | Do While loop | `let rec` inner loop |
| Linear Rec | If + Dictionary | match + Dictionary |
| Mutual Rec | Shared Dictionary | `and` keyword |
| Base Case | If statements | Pattern match |

### Verbosity

VB.NET is more verbose but explicit:

```vb
' VB.NET
If _memo.ContainsKey(n) Then
    Return _memo(n)
End If
```

```fsharp
// F#
match memo.TryGetValue(n) with
| true, v -> v
| false, _ -> ...
```

## If Statement Patterns

### Single-Line If

```vb
If n <= 0 Then Return 0
If n = 1 Then Return 1
```

### Multi-Line If

```vb
If n = 0 Then
    Return True
ElseIf n > 0 Then
    Return is_odd(n - 1)
Else
    Return False
End If
```

## Performance Characteristics

| Pattern | Time | Space | Memoized |
|---------|------|-------|----------|
| Tail Recursion | O(n) | O(1) | No |
| Linear Recursion | O(n)* | O(n) | Yes |
| Mutual Recursion | O(n) | O(n) | Optional |

*First call O(n), subsequent O(1) due to memoization.

## Type Declarations

### Function Return Types

```vb
Public Function sum(n As Integer, acc As Integer) As Integer
```

### Variable Declarations

```vb
Dim current = n              ' Inferred as Integer
Dim current As Integer = n   ' Explicit type
```

### Dictionary Generic Types

```vb
Dictionary(Of Integer, Integer)   ' Key: Integer, Value: Integer
Dictionary(Of String, Boolean)    ' Key: String, Value: Boolean
```

## Source Files

- `src/unifyweaver/targets/vbnet_target.pl`
