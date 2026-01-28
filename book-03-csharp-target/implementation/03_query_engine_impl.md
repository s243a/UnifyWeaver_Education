<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: Query Engine - Implementation Details

This document provides function-level documentation for the C# Query Runtime.

**Source**: `src/unifyweaver/targets/csharp_query_target.pl`, `src/unifyweaver/targets/csharp_query_runtime/`

---

## Overview: IR-Based Approach

```
Prolog → IR (Query Plan) → Runtime Engine executes
```

Unlike source-to-source translation, the query runtime uses an intermediate representation that supports:
- Complex recursive patterns
- Mutual recursion
- Runtime optimization

---

## Core IR Components

### RelationRef - Table Lookup

```csharp
new RelationRef("parent", 2)  // "Fetch all parent/2 tuples"
```

**SQL Equivalent**: `SELECT * FROM parent`

### Selection - Filter Rows

```csharp
new Selection(
    new RelationRef("age", 2),
    tuple => tuple.Item2 >= 18
)
```

**SQL Equivalent**: `SELECT * FROM age WHERE age >= 18`

### Projection - Reshape Tuples

```csharp
new Projection(
    new RelationRef("person", 3),
    tuple => (tuple.Item1, tuple.Item3)
)
```

**SQL Equivalent**: `SELECT name, city FROM person`

### Join - Combine Related Tuples

```csharp
new Join(
    new RelationRef("parent", 2),
    new RelationRef("parent", 2),
    outer => outer.Item2,   // Key: child
    inner => inner.Item1,   // Key: parent
    (outer, inner) => (outer.Item1, inner.Item2)
)
```

**SQL Equivalent**: `SELECT p1.parent, p2.child FROM parent p1 JOIN parent p2 ON p1.child = p2.parent`

### Union - Combine Multiple Sources

```csharp
new Union(clause1, clause2)
```

**SQL Equivalent**: `SELECT * FROM source1 UNION SELECT * FROM source2`

### Distinct - Remove Duplicates

```csharp
new Distinct(source)
```

**SQL Equivalent**: `SELECT DISTINCT ...`

---

## QueryPlan Structure

```csharp
var ancestorPlan = new QueryPlan {
    Name = "ancestor",
    Arity = 2,
    Clauses = new[] {
        // Base case: ancestor(X, Y) :- parent(X, Y)
        new Clause {
            Body = new RelationRef("parent", 2)
        },

        // Recursive case: ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z)
        new Clause {
            Body = new Join(
                new RelationRef("parent", 2),
                new RelationRef("ancestor", 2),
                outer => outer.Item2,
                inner => inner.Item1,
                (outer, inner) => (outer.Item1, inner.Item2)
            )
        }
    }
};
```

---

## Semi-Naive Fixpoint Evaluation

### Algorithm

```
1. Initialize: total = base_facts, delta = base_facts
2. While delta is not empty:
   a. new_delta = {}
   b. Apply rules using delta (not total)
   c. For each new tuple:
      - If not in total: add to total and new_delta
   d. delta = new_delta
3. Return total
```

### Example Trace

**Facts**: parent(alice, bob), parent(bob, charlie), parent(charlie, dave)

**Iteration 0**:
```
ancestor = {}
```

**Iteration 1** (base case):
```
ancestor = {(alice,bob), (bob,charlie), (charlie,dave)}
delta = {(alice,bob), (bob,charlie), (charlie,dave)}
```

**Iteration 2** (recursive):
```
Join parent with delta:
- parent(alice,bob) + ancestor(bob,charlie) → (alice,charlie)
- parent(bob,charlie) + ancestor(charlie,dave) → (bob,dave)

ancestor += {(alice,charlie), (bob,dave)}
delta = {(alice,charlie), (bob,dave)}
```

**Iteration 3**:
```
Join parent with delta:
- parent(alice,bob) + ancestor(bob,dave) → (alice,dave)

ancestor += {(alice,dave)}
delta = {(alice,dave)}
```

**Iteration 4**:
```
No new tuples
delta = {}
Fixpoint reached!
```

---

## Semi-Naive vs Naive Performance

| Depth | Naive Joins | Semi-Naive Joins |
|-------|-------------|------------------|
| 1 | 3 | 3 |
| 2 | 15 | 6 |
| 3 | 45 | 3 |
| 4 | 135 | 0 (done) |

Semi-naive only joins with **new** tuples from the previous iteration.

---

## Mutual Recursion Support

### MutualFixpointNode

For predicates that call each other (e.g., `is_even`/`is_odd`):

```csharp
new MutualFixpointNode(new[] {
    new MutualMember(isEvenId, evenBasePlan, evenRecursivePlans),
    new MutualMember(isOddId, oddBasePlan, oddRecursivePlans)
}, isEvenId);
```

### EvaluationContext

```csharp
var context = new EvaluationContext {
    Totals = { [isEvenId] = totalEven, [isOddId] = totalOdd },
    Deltas = { [isEvenId] = deltaEven, [isOddId] = deltaOdd }
};

while (context.Deltas.Values.Any(d => d.Count > 0)) {
    foreach (var member in node.Members) {
        context.Current = member.Predicate;
        // Evaluate and collect new tuples
    }
    context.Deltas = nextDeltas;
}
```

### CrossRefNode

Replaces recursive calls that target **other** predicates in the group:

```csharp
// is_even calls is_odd
new CrossRefNode("is_odd", RecursiveRefKind.Delta)
```

---

## compile_predicate_to_csharp/3

### Signature

```prolog
compile_predicate_to_csharp(+Predicate/Arity, +Options, -Code)
```

### Options

| Option | Values | Description |
|--------|--------|-------------|
| `target(T)` | `csharp_query` | Use query runtime |
| `verbose(B)` | `true`, `false` | Enable debug output |

### Example

```prolog
?- compile_predicate_to_csharp(ancestor/2, [target(csharp_query)], Code).
```

---

## QueryExecutor Class

### Usage

```csharp
var result = AncestorQueryModule.Build();
var executor = new QueryExecutor(result.Provider);
var tuples = executor.Execute(result.Plan).ToList();
```

### Methods

| Method | Description |
|--------|-------------|
| `Execute(plan)` | Run query plan, return tuples |
| `ExecuteWithStats(plan)` | Return tuples + iteration count |

---

## Debugging IR

### Enable Verbose Output

```prolog
compile_predicate_to_csharp(ancestor/2, [
    target(csharp_query),
    verbose(true)
], Code).
```

### Generated Debug Code

```csharp
Console.WriteLine($"Iteration {iteration}");
Console.WriteLine($"New tuples: {newTuples.Count()}");
foreach (var tuple in newTuples) {
    Console.WriteLine($"  {tuple}");
}
```

### Reading Nested IR

Work outside-in:
1. Identify outermost operation (Union, Join, etc.)
2. Understand each nested level
3. Translate to plain English
4. Trace with example data

---

## Testing

### Skip C# Execution

```bash
SKIP_CSHARP_EXECUTION=1 swipl -q \
    -f tests/core/test_csharp_query_target.pl \
    -g test_csharp_query_target:test_csharp_query_target \
    -t halt
```

### End-to-End

```bash
dotnet build --no-restore
dotnet run
```

---

## Related Documentation

- [Book 3 Chapter 2: C# Native Target](../02_csharp_native_target.md)
- [Book 3 Chapter 4: Runtime Libraries](../04_runtime_libraries_deployment.md)
- [Query Runtime Source](../../../../src/unifyweaver/targets/csharp_query_runtime/)
