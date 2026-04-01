<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 8: Native Clause Body Lowering

Write natural Prolog predicates and let UnifyWeaver compile them into C# methods with `if`/`else if`/`else` chains, PascalCase naming, and type-safe comparisons.

## How It Works

```prolog
classify(X, small) :- X > 0, X < 10.
classify(X, large) :- X >= 10.
```

Generates:

```csharp
public static object Classify(object arg1)
{
    if (Convert.ToInt32(arg1) > Convert.ToInt32(0) && Convert.ToInt32(arg1) < Convert.ToInt32(10))
    {
        return "small";
    }
    else if (Convert.ToInt32(arg1) >= Convert.ToInt32(10))
    {
        return "large";
    }
    else
    {
        throw new ArgumentException("No matching clause for classify");
    }
}
```

## C# Design Choices

**PascalCase naming**: `classify` becomes `Classify`, `my_func` becomes `MyFunc`.

**Type-safe comparisons**: Uses `Convert.ToInt32()` for numeric comparisons since arguments are typed as `object`. This ensures runtime type safety.

**Exception handling**: Unmatched clauses throw `ArgumentException`, following C# conventions.

## Basic Examples

### Arithmetic

```prolog
double(X, R) :- R is X * 2.
```

```csharp
public static object Double(object arg1)
{
    return (Convert.ToInt32(arg1) * Convert.ToInt32(2));
}
```

### Absolute Value

```prolog
abs_test(X, Y) :- Y is abs(X).
```

```csharp
public static object AbsTest(object arg1)
{
    return Math.Abs(Convert.ToInt32(arg1));
}
```

## Integration with LINQ Pipeline

The native lowering coexists with C#'s existing LINQ-based compilation. Native lowering is tried first; if the predicate doesn't fit the guard/output pattern (e.g. it references fact tables or is recursive), it falls through to the LINQ pipeline automatically.

- **Pure logic predicates** (guards + output) → native lowering
- **Fact-based predicates** → LINQ `IEnumerable<T>` stream
- **Recursive predicates** → LINQ TransitiveClosure extension

## C#-Specific Syntax

| Prolog | C# |
|--------|-----|
| `X > 0, X < 10` | `Convert.ToInt32(arg1) > ... && ... < ...` |
| `X =:= 0` | `Convert.ToInt32(arg1) == Convert.ToInt32(0)` |
| `R is abs(X)` | `Math.Abs(Convert.ToInt32(arg1))` |
| `R is X mod 2` | `Convert.ToInt32(arg1) % Convert.ToInt32(2)` |
| No match | `throw new ArgumentException("...")` |
| Naming | PascalCase (`my_func` → `MyFunc`) |

## Summary

- Multi-clause predicates compile to C# `if`/`else if`/`else` chains
- PascalCase method names follow C# conventions
- `Convert.ToInt32()` for type-safe numeric operations
- `Math.Abs()` for absolute value
- `ArgumentException` for unmatched clauses
- Coexists with LINQ pipeline (native tried first, LINQ fallback)

---

## Navigation

**←** [Previous: Chapter 7: PowerShell Semantic](07_powershell_semantic) | [Book 3: C# Target](./)
