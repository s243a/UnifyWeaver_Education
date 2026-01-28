<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: JOINs - Questions

Q&A companion for [03_joins_impl.md](./03_joins_impl.md).

---

## Question Index

1. [How are INNER JOINs created implicitly?](#b10c03-q-inner-join)
2. [What does sql_left_join/1 do?](#b10c03-q-left-join)
3. [What does sql_right_join/1 do?](#b10c03-q-right-join)
4. [What does sql_full_outer_join/1 do?](#b10c03-q-full-outer)
5. [How do self-joins work?](#b10c03-q-self-join)
6. [How do I chain multiple JOINs?](#b10c03-q-chained-joins)
7. [How do I find rows without matches?](#b10c03-q-non-matching)
8. [Can I mix different JOIN types?](#b10c03-q-mixed-joins)
9. [How do I add WHERE conditions to JOINs?](#b10c03-q-where-conditions)
10. [How do I add ORDER BY and LIMIT?](#b10c03-q-order-limit)
11. [When should I use each JOIN type?](#b10c03-q-when-join)

---

## Questions and Answers

### <a id="b10c03-q-inner-join"></a>Q1: How are INNER JOINs created implicitly?

**Answer**: Shared variables between table goals create INNER JOIN conditions:

```prolog
customer_orders(Name, Product) :-
    customers(CustId, Name, _, _),
    orders(_, CustId, Product, _, _).
%          ^^^^^^
%   Shared variable creates JOIN
```

Generates: `INNER JOIN orders ON orders.customer_id = customers.id`

**See**: [INNER JOIN (Implicit)](./03_joins_impl.md#inner-join-implicit)

---

### <a id="b10c03-q-left-join"></a>Q2: What does sql_left_join/1 do?

**Answer**: Returns all rows from the left table, with matching rows from the right (or NULL):

```prolog
customers_with_orders(Name, Product) :-
    customers(CustId, Name, _, _),
    sql_left_join(orders(_, CustId, Product, _, _)).
```

Returns ALL customers, even those without orders (Product = NULL).

**See**: [sql_left_join/1](./03_joins_impl.md#sql_left_join1)

---

### <a id="b10c03-q-right-join"></a>Q3: What does sql_right_join/1 do?

**Answer**: Returns all rows from the right table, with matching rows from the left (or NULL):

```prolog
orders_with_customers(OrderId, Name) :-
    orders(OrderId, CustId, _, _, _),
    sql_right_join(customers(CustId, Name, _, _)).
```

Returns ALL customers, even those without orders.

**See**: [sql_right_join/1](./03_joins_impl.md#sql_right_join1)

---

### <a id="b10c03-q-full-outer"></a>Q4: What does sql_full_outer_join/1 do?

**Answer**: Returns all rows from both tables, with NULLs where there's no match:

```prolog
all_combos(Name, Product) :-
    customers(CustId, Name, _, _),
    sql_full_outer_join(orders(_, CustId, Product, _, _)).
```

Note: SQLite doesn't support FULL OUTER JOIN directly.

**See**: [sql_full_outer_join/1](./03_joins_impl.md#sql_full_outer_join1)

---

### <a id="b10c03-q-self-join"></a>Q5: How do self-joins work?

**Answer**: Reference the same table multiple times with different variables:

```prolog
employee_with_manager(EmpName, MgrName) :-
    employees(_, EmpName, MgrId, _),
    employees(MgrId, MgrName, _, _).
```

The compiler auto-generates aliases (`e1`, `e2`).

**See**: [Self-Joins](./03_joins_impl.md#self-joins)

---

### <a id="b10c03-q-chained-joins"></a>Q6: How do I chain multiple JOINs?

**Answer**: Add more table goals with shared variables:

```prolog
full_info(Name, Product, Tracking) :-
    customers(CustId, Name, _, _),
    orders(OrderId, CustId, Product, _, _),
    shipments(_, OrderId, Tracking, _).
```

Generates three-table JOIN automatically.

**See**: [Chained JOINs](./03_joins_impl.md#chained-joins)

---

### <a id="b10c03-q-non-matching"></a>Q7: How do I find rows without matches?

**Answer**: Use LEFT JOIN + `sql_is_null/1`:

```prolog
customers_without_orders(Name) :-
    customers(CustId, Name, _, _),
    sql_left_join(orders(OrderId, CustId, _, _, _)),
    sql_is_null(OrderId).
```

Generates: `WHERE orders.id IS NULL`

**See**: [Finding Non-Matching Rows](./03_joins_impl.md#finding-non-matching-rows)

---

### <a id="b10c03-q-mixed-joins"></a>Q8: Can I mix different JOIN types?

**Answer**: Yes, use implicit INNER and explicit LEFT/RIGHT in one query:

```prolog
orders_maybe_shipped(Name, Product, Tracking) :-
    customers(CustId, Name, _, _),
    orders(OrderId, CustId, Product, _, _),           % INNER
    sql_left_join(shipments(_, OrderId, Tracking, _)). % LEFT
```

**See**: [Mixed JOIN Types](./03_joins_impl.md#mixed-join-types)

---

### <a id="b10c03-q-where-conditions"></a>Q9: How do I add WHERE conditions to JOINs?

**Answer**: Add unification goals after table references:

```prolog
high_value(Name, Amount) :-
    customers(CustId, Name, _, Country),
    orders(_, CustId, _, Amount, _),
    Country = 'USA',
    Amount > 1000.
```

Generates: `WHERE customers.country = 'USA' AND orders.amount > 1000`

**See**: [JOIN with Filters](./03_joins_impl.md#join-with-filters)

---

### <a id="b10c03-q-order-limit"></a>Q10: How do I add ORDER BY and LIMIT?

**Answer**: Use `sql_order_by/2` and `sql_limit/1`:

```prolog
top_orders(Name, Amount) :-
    customers(CustId, Name, _, _),
    orders(_, CustId, _, Amount, _),
    sql_order_by(Amount, desc),
    sql_limit(10).
```

Generates: `ORDER BY orders.amount DESC LIMIT 10`

**See**: [JOIN with ORDER BY and LIMIT](./03_joins_impl.md#join-with-order-by-and-limit)

---

### <a id="b10c03-q-when-join"></a>Q11: When should I use each JOIN type?

**Answer**:

| JOIN Type | Use When |
|-----------|----------|
| INNER | Only want rows matching in both |
| LEFT | Want all from left, even without matches |
| RIGHT | Want all from right, even without matches |
| FULL OUTER | Want all rows from both tables |

**See**: [Visual JOIN Reference](./03_joins_impl.md#visual-join-reference)

---

## Summary

SQL JOIN compilation provides:
- Implicit INNER JOIN via shared variables
- Explicit LEFT/RIGHT/FULL OUTER via predicates
- Self-joins with auto-generated aliases
- Non-matching row detection with IS NULL
- Filters, ordering, and limits
