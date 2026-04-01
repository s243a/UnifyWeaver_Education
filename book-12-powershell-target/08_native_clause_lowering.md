<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 8: Native Clause Body Lowering

Write natural Prolog predicates and let UnifyWeaver compile them into PowerShell functions with `if`/`elseif`/`else` chains.

## How It Works

```prolog
classify(X, small) :- X > 0, X < 10.
classify(X, large) :- X >= 10.
```

Generates:

```powershell
function classify($arg1) {
    if ($arg1 -gt 0 -and $arg1 -lt 10) {
        return "small"
    }
    if ($arg1 -ge 10) {
        return "large"
    }
    Write-Error "No matching clause for classify/2"
}
```

## Basic Examples

### Three-Way Classification

```prolog
grade(X, low)  :- X < 50.
grade(X, mid)  :- X >= 50, X < 80.
grade(X, high) :- X >= 80.
```

```powershell
function grade($arg1) {
    if ($arg1 -lt 50) {
        return "low"
    }
    if ($arg1 -ge 50 -and $arg1 -lt 80) {
        return "mid"
    }
    if ($arg1 -ge 80) {
        return "high"
    }
    Write-Error "No matching clause for grade/2"
}
```

### Arithmetic

```prolog
double(X, R) :- R is X * 2.
```

```powershell
function double($arg1) {
    return ($arg1 * 2)
}
```

## PowerShell-Specific Syntax

| Prolog | PowerShell |
|--------|------------|
| `X > 0` | `$arg1 -gt 0` |
| `X < 10` | `$arg1 -lt 10` |
| `X >= 50` | `$arg1 -ge 50` |
| `X =< 80` | `$arg1 -le 80` |
| `X =:= 0` | `$arg1 -eq 0` |
| `X =\= 0` | `$arg1 -ne 0` |
| `X > 0, X < 10` | `$arg1 -gt 0 -and $arg1 -lt 10` |
| `R is abs(X)` | `[Math]::Abs($arg1)` |
| No match | `Write-Error "..."` |

PowerShell uses comparison operators (`-gt`, `-lt`, `-ge`, `-le`, `-eq`, `-ne`) and logical operators (`-and`, `-or`) instead of symbolic operators.

## Summary

- Multi-clause predicates compile to PowerShell `if`/`return` chains
- Uses `-gt`/`-lt`/`-ge`/`-le`/`-eq`/`-ne` comparison operators
- `[Math]::Abs()` for absolute value
- `Write-Error` for unmatched clauses

---

## Navigation

**←** [Previous: Chapter 7: Recursive Queries](07_recursive_queries) | [Book 12: PowerShell Target](./)
