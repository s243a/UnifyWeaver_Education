<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: Query Engine - Questions

Q&A companion for [03_query_engine_impl.md](./03_query_engine_impl.md).

---

## Question Index

1. [What is the IR-based approach?](#b03c03-q-ir-approach)
2. [What is RelationRef?](#b03c03-q-relation-ref)
3. [What is Selection?](#b03c03-q-selection)
4. [What is Projection?](#b03c03-q-projection)
5. [What is Join?](#b03c03-q-join)
6. [How is a QueryPlan structured?](#b03c03-q-query-plan)
7. [What is semi-naive fixpoint evaluation?](#b03c03-q-semi-naive)
8. [Why is semi-naive faster than naive?](#b03c03-q-semi-naive-performance)
9. [How does mutual recursion work?](#b03c03-q-mutual-recursion)
10. [What is MutualFixpointNode?](#b03c03-q-mutual-fixpoint)
11. [How do I compile to the query runtime?](#b03c03-q-compile)
12. [How do I debug IR?](#b03c03-q-debugging)

---

## Questions and Answers

### <a id="b03c03-q-ir-approach"></a>Q1: What is the IR-based approach?

**Answer**: Instead of direct source-to-source translation, the query runtime uses:

```
Prolog → IR (Query Plan) → Runtime Engine executes
```

Benefits:
- Supports mutual recursion across predicates
- Runtime can optimize execution
- More complex patterns than native target

**See**: [Overview: IR-Based Approach](./03_query_engine_impl.md#overview-ir-based-approach)

---

### <a id="b03c03-q-relation-ref"></a>Q2: What is RelationRef?

**Answer**: `RelationRef` fetches tuples from a relation (table):

```csharp
new RelationRef("parent", 2)  // Get all parent/2 tuples
```

SQL equivalent: `SELECT * FROM parent`

**See**: [Core IR Components](./03_query_engine_impl.md#core-ir-components)

---

### <a id="b03c03-q-selection"></a>Q3: What is Selection?

**Answer**: `Selection` filters rows by a condition:

```csharp
new Selection(
    new RelationRef("age", 2),
    tuple => tuple.Item2 >= 18
)
```

SQL equivalent: `SELECT * FROM age WHERE age >= 18`

**See**: [Core IR Components](./03_query_engine_impl.md#core-ir-components)

---

### <a id="b03c03-q-projection"></a>Q4: What is Projection?

**Answer**: `Projection` reshapes tuples (selects/reorders fields):

```csharp
new Projection(
    new RelationRef("person", 3),
    tuple => (tuple.Item1, tuple.Item3)  // Keep fields 1 and 3
)
```

SQL equivalent: `SELECT name, city FROM person`

**See**: [Core IR Components](./03_query_engine_impl.md#core-ir-components)

---

### <a id="b03c03-q-join"></a>Q5: What is Join?

**Answer**: `Join` combines tuples from two sources on a common key:

```csharp
new Join(
    new RelationRef("parent", 2),
    new RelationRef("parent", 2),
    outer => outer.Item2,   // Key from first
    inner => inner.Item1,   // Key from second
    (outer, inner) => (outer.Item1, inner.Item2)
)
```

This implements: `grandparent(GP, GC) :- parent(GP, P), parent(P, GC)`

**See**: [Core IR Components](./03_query_engine_impl.md#core-ir-components)

---

### <a id="b03c03-q-query-plan"></a>Q6: How is a QueryPlan structured?

**Answer**: A QueryPlan contains:
- `Name` - Predicate name
- `Arity` - Number of arguments
- `Clauses` - Array of clause bodies (each an IR tree)

```csharp
new QueryPlan {
    Name = "ancestor",
    Arity = 2,
    Clauses = new[] {
        new Clause { Body = baseIR },
        new Clause { Body = recursiveIR }
    }
};
```

**See**: [QueryPlan Structure](./03_query_engine_impl.md#queryplan-structure)

---

### <a id="b03c03-q-semi-naive"></a>Q7: What is semi-naive fixpoint evaluation?

**Answer**: An optimization that only applies rules to **new** tuples:

1. Initialize: `total = base_facts`, `delta = base_facts`
2. While delta not empty:
   - Apply rules using delta (not total)
   - New tuples go to next delta
   - Add new tuples to total
3. Return total when delta is empty

**See**: [Semi-Naive Fixpoint Evaluation](./03_query_engine_impl.md#semi-naive-fixpoint-evaluation)

---

### <a id="b03c03-q-semi-naive-performance"></a>Q8: Why is semi-naive faster than naive?

**Answer**: Naive re-joins ALL tuples each iteration. Semi-naive only joins NEW tuples:

| Depth | Naive Joins | Semi-Naive Joins |
|-------|-------------|------------------|
| 2 | 15 | 6 |
| 3 | 45 | 3 |
| 4 | 135 | 0 |

Much faster for deep recursion!

**See**: [Semi-Naive vs Naive Performance](./03_query_engine_impl.md#semi-naive-vs-naive-performance)

---

### <a id="b03c03-q-mutual-recursion"></a>Q9: How does mutual recursion work?

**Answer**: For predicates that call each other (e.g., `is_even`/`is_odd`):

1. Group related predicates into a "strongly connected component"
2. Maintain separate total/delta sets per predicate
3. Iterate until all deltas are empty

**See**: [Mutual Recursion Support](./03_query_engine_impl.md#mutual-recursion-support)

---

### <a id="b03c03-q-mutual-fixpoint"></a>Q10: What is MutualFixpointNode?

**Answer**: IR node that orchestrates mutual recursion:

```csharp
new MutualFixpointNode(new[] {
    new MutualMember(isEvenId, evenBasePlan, evenRecursivePlans),
    new MutualMember(isOddId, oddBasePlan, oddRecursivePlans)
}, headPredicateId);
```

Each `MutualMember` has its base and recursive plans. `CrossRefNode` handles calls between predicates.

**See**: [MutualFixpointNode](./03_query_engine_impl.md#mutualfixpointnode)

---

### <a id="b03c03-q-compile"></a>Q11: How do I compile to the query runtime?

**Answer**: Use `target(csharp_query)` option:

```prolog
?- compile_predicate_to_csharp(ancestor/2, [
    target(csharp_query)
], Code).
```

Then build and run:
```bash
dotnet build
dotnet run
```

**See**: [compile_predicate_to_csharp/3](./03_query_engine_impl.md#compile_predicate_to_csharp3)

---

### <a id="b03c03-q-debugging"></a>Q12: How do I debug IR?

**Answer**: Enable verbose mode:

```prolog
compile_predicate_to_csharp(ancestor/2, [
    target(csharp_query),
    verbose(true)
], Code).
```

This adds iteration logging:
```
Iteration 1: New tuples: 3
  ancestor(alice, bob)
  ancestor(bob, charlie)
```

Read nested IR outside-in, translating each level to plain English.

**See**: [Debugging IR](./03_query_engine_impl.md#debugging-ir)

---

## Summary

The C# Query Runtime provides:
- IR-based query representation
- Six core operators (RelationRef, Selection, Projection, Join, Union, Distinct)
- Semi-naive fixpoint evaluation
- Mutual recursion via MutualFixpointNode
- Verbose debugging support
