<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Facts and Rules - Questions

Q&A companion for [02_facts_rules_impl.md](./02_facts_rules_impl.md).

---

## Question Index

1. [What does compile_to_powershell/3 do?](#b12c02-q-compile-to-powershell)
2. [How are unary facts compiled?](#b12c02-q-unary-facts)
3. [How are binary facts compiled?](#b12c02-q-binary-facts)
4. [Why does PowerShell use PSCustomObject?](#b12c02-q-pscustomobject)
5. [What does compile_facts_to_powershell/3 generate?](#b12c02-q-class-export)
6. [How are rules with joins compiled?](#b12c02-q-join-rules)
7. [How is negation compiled?](#b12c02-q-negation)
8. [What are the compilation modes?](#b12c02-q-modes)
9. [How does pipeline integration work?](#b12c02-q-pipeline)
10. [What are the query modes for binary facts?](#b12c02-q-query-modes)
11. [What is the performance of nested loop joins?](#b12c02-q-join-performance)
12. [How do I optimize joins with indexes?](#b12c02-q-index-optimization)

---

## Questions and Answers

### <a id="b12c02-q-compile-to-powershell"></a>Q1: What does compile_to_powershell/3 do?

**Answer**: Compiles a Prolog predicate to PowerShell function code:

```prolog
compile_to_powershell(parent/2, [powershell_mode(pure)], Code).
```

Options include `powershell_mode(pure|baas)` and `output_file(Path)`.

**See**: [compile_to_powershell/3](./02_facts_rules_impl.md#compile_to_powershell3)

---

### <a id="b12c02-q-unary-facts"></a>Q2: How are unary facts compiled?

**Answer**: Unary facts become string arrays with optional key lookup:

```prolog
color(red). color(green). color(blue).
```

Generates:
```powershell
$facts = @('red', 'green', 'blue')
if ($Key) { $facts | Where-Object { $_ -eq $Key } }
else { $facts }
```

**See**: [Unary Fact Compilation](./02_facts_rules_impl.md#unary-fact-compilation)

---

### <a id="b12c02-q-binary-facts"></a>Q3: How are binary facts compiled?

**Answer**: Binary facts become arrays of `PSCustomObject`:

```prolog
parent(anne, bob). parent(bob, charles).
```

Generates:
```powershell
$facts = @(
    [PSCustomObject]@{ X='anne'; Y='bob' },
    [PSCustomObject]@{ X='bob'; Y='charles' }
)
```

**See**: [Binary Fact Compilation](./02_facts_rules_impl.md#binary-fact-compilation)

---

### <a id="b12c02-q-pscustomobject"></a>Q4: Why does PowerShell use PSCustomObject?

**Answer**: PSCustomObject provides:

| Feature | Benefit |
|---------|---------|
| Typed Properties | Named access via `.X`, `.Y` |
| Pipeline-Friendly | Works with `Where-Object`, `Select-Object` |
| Serialization | `ConvertTo-Json` for export |

**See**: [PSCustomObject Pattern](./02_facts_rules_impl.md#pscustomobject-pattern)

---

### <a id="b12c02-q-class-export"></a>Q5: What does compile_facts_to_powershell/3 generate?

**Answer**: A PowerShell class with static methods:

```powershell
class PARENT {
    static [string[][]] $FACTS = @(...)
    static [string[][]] GetAll() { return [PARENT]::FACTS }
    static [bool] Contains([string[]]$target) { ... }
}
```

Provides `GetAll()`, `Stream()`, and `Contains()` methods.

**See**: [compile_facts_to_powershell/3](./02_facts_rules_impl.md#compile_facts_to_powershell3)

---

### <a id="b12c02-q-join-rules"></a>Q6: How are rules with joins compiled?

**Answer**: Shared variables become nested loop joins:

```prolog
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

Generates:
```powershell
foreach ($r1 in $rel1) {
    foreach ($r2 in $rel2) {
        if ($r1.Y -eq $r2.X) { ... }
    }
}
```

**See**: [Rule Compilation: Nested Loop Join](./02_facts_rules_impl.md#rule-compilation-nested-loop-join)

---

### <a id="b12c02-q-negation"></a>Q7: How is negation compiled?

**Answer**: Negation uses hashtable exclusion:

```prolog
parent_only(X) :- parent(X, _), \+ grandparent(X, _).
```

Generates:
```powershell
$grandparent_set = @{}
foreach ($gp in grandparent) { $grandparent_set["$($gp.X):..."] = $true }
if (-not ($grandparent_set.Keys | Where-Object { $_ -like $negKey })) { ... }
```

**See**: [Rule Compilation: Negation](./02_facts_rules_impl.md#rule-compilation-negation)

---

### <a id="b12c02-q-modes"></a>Q8: What are the compilation modes?

**Answer**:

| Mode | Description |
|------|-------------|
| `pure` | Native PowerShell, no Bash needed |
| `baas` | Bash-as-a-Service, wraps Bash code |

```prolog
compile_to_powershell(Pred/2, [powershell_mode(pure)], Code).
```

**See**: [Compilation Modes](./02_facts_rules_impl.md#compilation-modes)

---

### <a id="b12c02-q-pipeline"></a>Q9: How does pipeline integration work?

**Answer**: Use `ValueFromPipeline` for pipeline input:

```powershell
param([Parameter(ValueFromPipeline=$true)] $InputData)
process { parent -X $InputData }
```

Results work with `Where-Object`, `Select-Object`, `Export-Csv`, etc.

**See**: [Pipeline Integration](./02_facts_rules_impl.md#pipeline-integration)

---

### <a id="b12c02-q-query-modes"></a>Q10: What are the query modes for binary facts?

**Answer**:

| Call | Behavior |
|------|----------|
| `parent` | All relationships |
| `parent -X bob` | Children of bob |
| `parent -Y charles` | Parents of charles |
| `parent -X anne -Y bob` | Check specific pair |

**See**: [Query Modes](./02_facts_rules_impl.md#query-modes)

---

### <a id="b12c02-q-join-performance"></a>Q11: What is the performance of nested loop joins?

**Answer**: O(n*m) for nested loops. For large datasets:

| Pattern | Complexity |
|---------|------------|
| Nested loop | O(n*m) |
| With negation | O(n*m + k) |

Consider pre-building indexes for frequent queries.

**See**: [Performance Considerations](./02_facts_rules_impl.md#performance-considerations)

---

### <a id="b12c02-q-index-optimization"></a>Q12: How do I optimize joins with indexes?

**Answer**: Pre-build hashtable indexes:

```powershell
$parentByChild = @{}
foreach ($p in parent) {
    if (-not $parentByChild.ContainsKey($p.Y)) {
        $parentByChild[$p.Y] = @()
    }
    $parentByChild[$p.Y] += $p.X
}
```

Reduces join complexity from O(n*m) to O(n).

**See**: [Index Pre-computation](./02_facts_rules_impl.md#index-pre-computation)

---

## Summary

PowerShell target compilation provides:
- Unary facts as string arrays
- Binary facts as PSCustomObject arrays
- Rules as nested loop joins
- Negation via hashtable exclusion
- Pure PowerShell or BaaS modes
- Full pipeline integration
