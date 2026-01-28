<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Facts and Rules - Implementation Details

This document provides function-level documentation for PowerShell target compilation of facts and rules.

**Source**: `src/unifyweaver/targets/powershell_target.pl`

---

## Overview: Compilation Targets

| Prolog Construct | PowerShell Output | Data Structure |
|------------------|-------------------|----------------|
| Unary fact `p(x).` | String array | `@('x', 'y', ...)` |
| Binary fact `p(x,y).` | PSCustomObject array | `@([PSCustomObject]@{...})` |
| Rule with join | Nested foreach | `foreach ($r1 in ...) { foreach ($r2 in ...) { } }` |
| Rule with negation | Hashtable exclusion | `$set = @{}; if (-not $set.ContainsKey(...))` |

---

## compile_to_powershell/3

Compiles a Prolog predicate to PowerShell code.

### Signature

```prolog
compile_to_powershell(+Predicate/Arity, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicate/Arity` | `atom/integer` | Predicate to compile |
| `Options` | `list` | Compilation options |
| `Code` | `string` | Generated PowerShell code |

### Options

| Option | Values | Description |
|--------|--------|-------------|
| `powershell_mode(Mode)` | `pure`, `baas` | Force compilation mode |
| `output_file(Path)` | `string` | Write directly to file |

### Example

```prolog
?- compile_to_powershell(parent/2, [powershell_mode(pure)], Code).
```

---

## Unary Fact Compilation

### Pattern

```prolog
color(red).
color(green).
color(blue).
```

### Generated Structure

```powershell
function color {
    param([string]$Key)

    $facts = @(
        'red'
        'green'
        'blue'
    )

    if ($Key) {
        $facts | Where-Object { $_ -eq $Key }
    } else {
        $facts
    }
}
```

### Query Modes

| PowerShell Call | Behavior |
|-----------------|----------|
| `color` | Return all values |
| `color red` | Return 'red' if exists |

---

## Binary Fact Compilation

### Pattern

```prolog
parent(anne, bob).
parent(bob, charles).
parent(bob, diana).
```

### Generated Structure

```powershell
function parent {
    param([string]$X, [string]$Y)

    $facts = @(
        [PSCustomObject]@{ X='anne'; Y='bob' },
        [PSCustomObject]@{ X='bob'; Y='charles' },
        [PSCustomObject]@{ X='bob'; Y='diana' }
    )

    if ($X -and $Y) {
        $facts | Where-Object { $_.X -eq $X -and $_.Y -eq $Y }
    } elseif ($X) {
        $facts | Where-Object { $_.X -eq $X } | ForEach-Object { $_.Y }
    } elseif ($Y) {
        $facts | Where-Object { $_.Y -eq $Y } | ForEach-Object { $_.X }
    } else {
        $facts | ForEach-Object { "$($_.X):$($_.Y)" }
    }
}
```

### Query Modes

| PowerShell Call | SQL Equivalent |
|-----------------|----------------|
| `parent` | `SELECT * FROM parent` |
| `parent -X bob` | `SELECT Y FROM parent WHERE X='bob'` |
| `parent -Y charles` | `SELECT X FROM parent WHERE Y='charles'` |
| `parent -X anne -Y bob` | `SELECT * FROM parent WHERE X='anne' AND Y='bob'` |

---

## PSCustomObject Pattern

### Why PSCustomObject?

| Feature | Benefit |
|---------|---------|
| Typed Properties | Named access via `.X`, `.Y` |
| Pipeline-Friendly | Works with `Where-Object`, `Select-Object` |
| Inspection | `Format-Table`, `Format-List` for debugging |
| Serialization | `ConvertTo-Json` for export |

### Property Naming Convention

For n-ary predicates, properties are named sequentially:

| Arity | Properties |
|-------|------------|
| 2 | `X`, `Y` |
| 3 | `X`, `Y`, `Z` |
| 4+ | `Arg1`, `Arg2`, `Arg3`, ... |

---

## compile_facts_to_powershell/3

Generates a class-based fact export (alternative to function-based).

### Signature

```prolog
compile_facts_to_powershell(+Predicate, +Arity, -Code)
```

### Generated Structure

```powershell
class PARENT {
    static [string[][]] $FACTS = @(
        @("anne", "bob"),
        @("bob", "charles")
    )

    static [string[][]] GetAll() {
        return [PARENT]::FACTS
    }

    static [System.Collections.Generic.IEnumerable[string[]]] Stream() {
        foreach ($fact in [PARENT]::FACTS) {
            $fact
        }
    }

    static [bool] Contains([string[]]$target) {
        foreach ($fact in [PARENT]::FACTS) {
            $match = $true
            for ($i = 0; $i -lt $target.Length; $i++) {
                if ($fact[$i] -ne $target[$i]) {
                    $match = $false
                    break
                }
            }
            if ($match) { return $true }
        }
        return $false
    }
}
```

### Static Methods

| Method | Return Type | Description |
|--------|-------------|-------------|
| `GetAll()` | `string[][]` | All facts as 2D array |
| `Stream()` | `IEnumerable` | Lazy iteration |
| `Contains(target)` | `bool` | Membership test |

---

## Rule Compilation: Nested Loop Join

### Pattern

```prolog
grandparent(X, Z) :-
    parent(X, Y),
    parent(Y, Z).
```

### Generated Structure

```powershell
function grandparent {
    param([string]$X, [string]$Z)

    $rel1 = parent
    $rel2 = parent

    # Nested loop join: rel1.Y = rel2.X
    $results = foreach ($r1 in $rel1) {
        foreach ($r2 in $rel2) {
            if ($r1.Y -eq $r2.X) {
                [PSCustomObject]@{
                    X = $r1.X
                    Z = $r2.Y
                }
            }
        }
    }

    # ... query mode filtering
}
```

### Join Condition Derivation

The join condition comes from shared variables:

```
parent(X, Y), parent(Y, Z)
         │          │
         └──────────┘
          Y = Y (shared)
```

Translates to: `$r1.Y -eq $r2.X`

---

## Rule Compilation: Negation

### Pattern

```prolog
parent_only(X) :-
    parent(X, _),
    \+ grandparent(X, _).
```

### Generated Structure

```powershell
function parent_only {
    param([string]$X)

    $facts = parent

    # Load negated facts into hashtable for O(1) lookup
    $grandparent_set = @{}
    foreach ($gp in grandparent) {
        $grandparent_set["$($gp.X):$($gp.Z)"] = $true
    }

    $results = foreach ($f in $facts) {
        $x = $f.X
        # Negation check: skip if X is a grandparent
        $negKey = "$x:*"
        if (-not ($grandparent_set.Keys | Where-Object { $_ -like $negKey })) {
            [PSCustomObject]@{ X = $x }
        }
    }
}
```

### Negation Strategy

1. Pre-compute negated relation into hashtable
2. For each candidate, check if key exists
3. Uses wildcard matching for partial bindings

---

## Compilation Modes

### Pure PowerShell Mode

```prolog
compile_to_powershell(Pred/Arity, [powershell_mode(pure)], Code)
```

| Pros | Cons |
|------|------|
| No Bash required | Some patterns not yet supported |
| Native PowerShell objects | |
| Better IDE integration | |

### BaaS Mode (Bash-as-a-Service)

```prolog
compile_to_powershell(Pred/Arity, [powershell_mode(baas)], Code)
```

| Pros | Cons |
|------|------|
| All Bash templates work | Requires Bash on system |
| Battle-tested code gen | Text-based, not objects |

---

## Pipeline Integration

### ValueFromPipeline Pattern

```powershell
function custom_query {
    param(
        [Parameter(ValueFromPipeline=$true)]
        $InputData
    )

    process {
        parent -X $InputData
    }
}

# Usage
@('anne', 'bob') | custom_query
```

### Pipeline Cmdlet Compatibility

| Cmdlet | Usage |
|--------|-------|
| `Where-Object` | Filter results |
| `Select-Object` | Project fields |
| `Sort-Object` | Order results |
| `Group-Object` | Aggregate |
| `Export-Csv` | Export to file |
| `ConvertTo-Json` | Serialize |

---

## Performance Considerations

### Join Complexity

| Pattern | Complexity | Optimization |
|---------|------------|--------------|
| Nested loop | O(n*m) | Pre-build hashtable index |
| With negation | O(n*m + k) | Pre-compute negated set |

### Index Pre-computation

```powershell
# Pre-build index for faster joins
$parentByChild = @{}
foreach ($p in parent) {
    if (-not $parentByChild.ContainsKey($p.Y)) {
        $parentByChild[$p.Y] = @()
    }
    $parentByChild[$p.Y] += $p.X
}
```

---

## Related Documentation

- [Book 12 Chapter 1: Introduction](../01_introduction.md)
- [Book 12 Chapter 3: Cmdlet Generation](../03_cmdlet_generation.md)
- [PowerShell Target Source](../../../../src/unifyweaver/targets/powershell_target.pl)
