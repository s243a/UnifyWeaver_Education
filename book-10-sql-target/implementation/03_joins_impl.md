<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: JOINs - Implementation Details

This document provides function-level documentation for SQL JOIN compilation.

**Source**: `src/unifyweaver/targets/sql_target.pl`

---

## Overview: JOIN Types

| JOIN Type | Generated When | Returns |
|-----------|----------------|---------|
| INNER | Shared variable (implicit) | Matching rows only |
| LEFT | `sql_left_join/1` | All left + matching right |
| RIGHT | `sql_right_join/1` | All right + matching left |
| FULL OUTER | `sql_full_outer_join/1` | All rows from both |

---

## INNER JOIN (Implicit)

Shared variables between table references create INNER JOINs.

### Prolog Pattern

```prolog
customer_orders(CustomerName, Product, Amount) :-
    customers(CustId, CustomerName, _, _),
    orders(_, CustId, Product, Amount, _).
```

The shared `CustId` variable creates the join condition.

### Generated SQL

```sql
SELECT customers.name, orders.product, orders.amount
FROM customers
INNER JOIN orders ON orders.customer_id = customers.id;
```

### Multiple Join Conditions

```prolog
matched_records(A, B) :-
    table1(Id, Code, A),
    table2(Id, Code, B).
```

Generates: `... ON table2.id = table1.id AND table2.code = table1.code`

---

## sql_left_join/1

Returns all rows from left table with matching right rows (or NULL).

### Signature

```prolog
sql_left_join(+TableGoal)
```

### Example

```prolog
customers_with_orders(CustomerName, Product) :-
    customers(CustId, CustomerName, _, _),
    sql_left_join(orders(_, CustId, Product, _, _)).
```

### Generated SQL

```sql
SELECT customers.name, orders.product
FROM customers
LEFT JOIN orders ON orders.customer_id = customers.id;
```

This returns ALL customers, even those without orders (Product = NULL).

---

## sql_right_join/1

Returns all rows from right table with matching left rows (or NULL).

### Signature

```prolog
sql_right_join(+TableGoal)
```

### Example

```prolog
orders_with_customers(OrderId, CustomerName) :-
    orders(OrderId, CustId, _, _, _),
    sql_right_join(customers(CustId, CustomerName, _, _)).
```

### Generated SQL

```sql
SELECT orders.id, customers.name
FROM orders
RIGHT JOIN customers ON customers.id = orders.customer_id;
```

---

## sql_full_outer_join/1

Returns all rows from both tables.

### Signature

```prolog
sql_full_outer_join(+TableGoal)
```

### Example

```prolog
all_customer_order_combos(CustomerName, Product) :-
    customers(CustId, CustomerName, _, _),
    sql_full_outer_join(orders(_, CustId, Product, _, _)).
```

### Generated SQL

```sql
SELECT customers.name, orders.product
FROM customers
FULL OUTER JOIN orders ON orders.customer_id = customers.id;
```

**Note**: SQLite doesn't support FULL OUTER JOIN directly.

---

## Self-Joins

Join a table to itself for hierarchical data.

### Employee-Manager Example

```prolog
employee_with_manager(EmpName, ManagerName) :-
    employees(_, EmpName, ManagerId, _),
    employees(ManagerId, ManagerName, _, _).
```

### Generated SQL

```sql
SELECT e1.name, e2.name
FROM employees e1
INNER JOIN employees e2 ON e2.id = e1.manager_id;
```

The compiler automatically generates table aliases (`e1`, `e2`).

---

## Chained JOINs

### Three-Table JOIN

```prolog
full_order_info(CustomerName, Product, Tracking) :-
    customers(CustId, CustomerName, _, _),
    orders(OrderId, CustId, Product, _, _),
    shipments(_, OrderId, Tracking, _).
```

### Generated SQL

```sql
SELECT customers.name, orders.product, shipments.tracking
FROM customers
INNER JOIN orders ON orders.customer_id = customers.id
INNER JOIN shipments ON shipments.order_id = orders.id;
```

---

## Finding Non-Matching Rows

### sql_is_null/1

Combine LEFT JOIN with IS NULL to find rows without matches.

```prolog
customers_without_orders(CustomerName) :-
    customers(CustId, CustomerName, _, _),
    sql_left_join(orders(OrderId, CustId, _, _, _)),
    sql_is_null(OrderId).
```

### Generated SQL

```sql
SELECT customers.name
FROM customers
LEFT JOIN orders ON orders.customer_id = customers.id
WHERE orders.id IS NULL;
```

---

## Mixed JOIN Types

Combine different JOIN types in one query.

```prolog
orders_maybe_shipped(CustomerName, Product, Tracking) :-
    customers(CustId, CustomerName, _, _),
    orders(OrderId, CustId, Product, _, _),           % INNER
    sql_left_join(shipments(_, OrderId, Tracking, _)). % LEFT
```

### Generated SQL

```sql
SELECT customers.name, orders.product, shipments.tracking
FROM customers
INNER JOIN orders ON orders.customer_id = customers.id
LEFT JOIN shipments ON shipments.order_id = orders.id;
```

---

## JOIN with Filters

### Adding WHERE Conditions

```prolog
high_value_orders(CustomerName, Product, Amount) :-
    customers(CustId, CustomerName, _, Country),
    orders(_, CustId, Product, Amount, _),
    Country = 'USA',
    Amount > 1000.
```

### Generated SQL

```sql
SELECT customers.name, orders.product, orders.amount
FROM customers
INNER JOIN orders ON orders.customer_id = customers.id
WHERE customers.country = 'USA' AND orders.amount > 1000;
```

---

## JOIN with ORDER BY and LIMIT

### sql_order_by/2 and sql_limit/1

```prolog
top_orders_by_customer(CustomerName, Amount) :-
    customers(CustId, CustomerName, _, _),
    orders(_, CustId, _, Amount, _),
    sql_order_by(Amount, desc),
    sql_limit(10).
```

### Generated SQL

```sql
SELECT customers.name, orders.amount
FROM customers
INNER JOIN orders ON orders.customer_id = customers.id
ORDER BY orders.amount DESC
LIMIT 10;
```

---

## compile_predicate_to_sql/3

### Signature

```prolog
compile_predicate_to_sql(+Predicate/Arity, +Options, -SQL)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicate/Arity` | `atom/integer` | Predicate to compile |
| `Options` | `list` | Compilation options |
| `SQL` | `string` | Generated SQL query |

### Example

```prolog
?- compile_predicate_to_sql(customer_orders/2, [], SQL).
```

---

## Visual JOIN Reference

```
INNER JOIN:        LEFT JOIN:         RIGHT JOIN:        FULL OUTER:
   ┌───┐              ┌───┐              ┌───┐              ┌───┐
   │███│              │███│              │   │              │███│
┌──┼───┼──┐        ┌──┼───┼──┐        ┌──┼───┼──┐        ┌──┼───┼──┐
│  │███│  │        │██│███│  │        │  │███│██│        │██│███│██│
└──┼───┼──┘        └──┼───┼──┘        └──┼───┼──┘        └──┼───┼──┘
   │███│              │   │              │███│              │███│
   └───┘              └───┘              └───┘              └───┘
```

---

## Related Documentation

- [Book 10 Chapter 2: Basic Queries](../02_basic_queries.md)
- [Book 10 Chapter 4: Aggregations](../04_aggregations.md)
- [SQL Target Source](../../../../src/unifyweaver/targets/sql_target.pl)
