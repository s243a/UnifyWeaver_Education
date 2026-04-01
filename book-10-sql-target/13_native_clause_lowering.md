<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 13: Native Clause Body Lowering

In previous chapters, you manually wrote `sql_case(...)` terms to generate CASE WHEN expressions. This chapter introduces **native clause body lowering** — where UnifyWeaver automatically compiles standard multi-clause Prolog predicates into SQL CASE WHEN expressions or PL/pgSQL stored functions. No special syntax needed.

## The Idea

Instead of manually constructing `sql_case(...)` terms inside your predicates, you write natural Prolog:

```prolog
% Natural Prolog — multi-clause with guards
classify(X, small) :- X > 0, X < 10.
classify(X, large) :- X >= 10.
```

UnifyWeaver detects the guard/output pattern and automatically generates:

```sql
SELECT
    @arg1 AS arg1,
    CASE
        WHEN arg1 > 0 AND arg1 < 10 THEN 'small'
        WHEN arg1 >= 10 THEN 'large'
        ELSE NULL
    END AS result;
```

This works because the compiler recognizes that each clause has:
- **Guard conditions** in the body (comparisons like `X > 0, X < 10`)
- **An output value** in the last head argument (the atom `small` or `large`)

These map naturally to SQL's `WHEN condition THEN value` syntax.

## When Native Lowering Activates

Native lowering is tried **automatically** when you call `compile_predicate_to_sql/3`. It activates when:

1. The predicate has **multiple clauses** (single clauses work too)
2. Each clause body contains **guard conditions** (comparisons, arithmetic tests)
3. The last argument is the **output** (a constant or computed value)
4. The clauses don't reference database tables (those go through the regular SQL pipeline)

If native lowering can't handle the predicate (e.g. it contains table references, JOINs, or aggregations), it falls through to the standard SQL compilation path automatically.

## Basic Examples

### Categorization

```prolog
grade(X, low)  :- X < 50.
grade(X, mid)  :- X >= 50, X < 80.
grade(X, high) :- X >= 80.

?- compile_predicate_to_sql(grade/2, [], SQL).
```

Generated SQL:
```sql
SELECT
    @arg1 AS arg1,
    CASE
        WHEN arg1 < 50 THEN 'low'
        WHEN arg1 >= 50 AND arg1 < 80 THEN 'mid'
        WHEN arg1 >= 80 THEN 'high'
        ELSE NULL
    END AS result;
```

### Arithmetic Output

```prolog
double(X, R) :- R is X * 2.

?- compile_predicate_to_sql(double/2, [], SQL).
```

Generated SQL:
```sql
SELECT
    @arg1 AS arg1,
    CASE WHEN TRUE THEN (arg1 * 2) ELSE NULL END AS result;
```

### Assignment (Identity)

```prolog
identity(X, R) :- R = X.
```

Generated SQL:
```sql
SELECT
    @arg1 AS arg1,
    CASE WHEN TRUE THEN arg1 ELSE NULL END AS result;
```

## If-Then-Else

Prolog's `(Cond -> Then ; Else)` compiles to nested CASE WHEN:

```prolog
abs_val(X, R) :- (X >= 0 -> R = X ; R is -X).
```

Generated SQL:
```sql
SELECT
    @arg1 AS arg1,
    CASE WHEN arg1 >= 0 THEN arg1
         ELSE (-arg1)
    END AS result;
```

### Nested If-Then-Else

```prolog
range_classify(X, R) :-
    (X < 0 -> R = negative
    ; (X =:= 0 -> R = zero
    ; R = positive)).
```

Generated SQL:
```sql
CASE WHEN arg1 < 0 THEN 'negative'
     ELSE CASE WHEN arg1 = 0 THEN 'zero'
               ELSE 'positive' END
END
```

## SQL-Specific Syntax

The native lowering generates idiomatic SQL:

| Prolog | SQL |
|--------|-----|
| `X > 0` | `arg1 > 0` |
| `X =:= 0` | `arg1 = 0` (not `==`) |
| `X =\= 0` | `arg1 <> 0` (not `!=`) |
| `X > 0, X < 10` | `arg1 > 0 AND arg1 < 10` |
| `R is X * 2` | `(arg1 * 2)` |
| `R is abs(X)` | `ABS(arg1)` |
| `R is X mod 2` | `(arg1 % 2)` |
| `R = hello` | `'hello'` (single-quoted) |

## Output Modes

Three output modes are available via the `sql_output_mode` option:

### Mode 1: `case_select` (Default)

Generates a SELECT with the CASE WHEN as a column:

```prolog
?- compile_predicate_to_sql(grade/2, [], SQL).
```

```sql
SELECT
    @arg1 AS arg1,
    CASE WHEN arg1 < 50 THEN 'low'
         WHEN arg1 >= 50 AND arg1 < 80 THEN 'mid'
         WHEN arg1 >= 80 THEN 'high'
         ELSE NULL END AS result;
```

The `@arg1` placeholder is meant to be replaced with actual values or column references when embedding into a larger query.

### Mode 2: `create_function` (PL/pgSQL)

Generates a PostgreSQL stored function with IF/ELSIF:

```prolog
?- compile_predicate_to_sql(grade/2, [sql_output_mode(create_function)], SQL).
```

```sql
CREATE OR REPLACE FUNCTION grade(arg1 INTEGER)
RETURNS TEXT AS $$
BEGIN
    IF arg1 < 50 THEN
        RETURN 'low';
    ELSIF arg1 >= 50 AND arg1 < 80 THEN
        RETURN 'mid';
    ELSIF arg1 >= 80 THEN
        RETURN 'high';
    ELSE
        RAISE EXCEPTION 'No matching clause for grade';
    END IF;
END;
$$ LANGUAGE plpgsql;
```

Once created, call it like any SQL function:

```sql
SELECT grade(75);        -- Returns 'mid'
SELECT grade(salary) FROM employees;  -- Classify every employee
```

### Mode 3: `case_expression`

Generates just the bare CASE expression, suitable for embedding:

```prolog
?- compile_predicate_to_sql(grade/2, [sql_output_mode(case_expression)], SQL).
```

```sql
CASE WHEN arg1 < 50 THEN 'low'
     WHEN arg1 >= 50 AND arg1 < 80 THEN 'mid'
     WHEN arg1 >= 80 THEN 'high'
     ELSE NULL END
```

Use this mode to embed the expression inside a larger hand-written query:

```sql
SELECT name, salary,
       CASE WHEN salary < 50000 THEN 'low'
            WHEN salary >= 50000 AND salary < 80000 THEN 'mid'
            WHEN salary >= 80000 THEN 'high'
            ELSE NULL END AS tier
FROM employees;
```

## Complex Examples

### Parity Check

```prolog
parity(X, even) :- 0 =:= X mod 2.
parity(X, odd)  :- 0 =\= X mod 2.
```

```sql
CASE WHEN 0 = (arg1 % 2) THEN 'even'
     WHEN 0 <> (arg1 % 2) THEN 'odd'
     ELSE NULL END
```

### Quadratic Formula Component

```prolog
formula(X, Y) :- Y is (X * X) + (X * 2) + 1.
```

```sql
CASE WHEN TRUE THEN ((arg1 * arg1) + ((arg1 * 2) + 1))
     ELSE NULL END
```

### Sign Function (PL/pgSQL)

```prolog
sign(X, positive) :- X > 0.
sign(X, zero)     :- X =:= 0.
sign(X, negative) :- X < 0.
```

```sql
CREATE OR REPLACE FUNCTION sign(arg1 INTEGER)
RETURNS TEXT AS $$
BEGIN
    IF arg1 > 0 THEN
        RETURN 'positive';
    ELSIF arg1 = 0 THEN
        RETURN 'zero';
    ELSIF arg1 < 0 THEN
        RETURN 'negative';
    ELSE
        RAISE EXCEPTION 'No matching clause for sign';
    END IF;
END;
$$ LANGUAGE plpgsql;
```

## Native Lowering vs. Explicit sql_case

| Aspect | Native Lowering (Ch. 13) | Explicit sql_case (Ch. 11) |
|--------|--------------------------|----------------------------|
| Syntax | Standard Prolog multi-clause | `sql_case([when(...), ...], Default)` |
| Activation | Automatic | Manual |
| Use case | Pure logic predicates | Queries referencing tables |
| Table access | No (guard/output only) | Yes (inside table queries) |
| PL/pgSQL mode | Yes (`create_function`) | No |
| Nesting | Prolog `(->; )` syntax | `sql_case` inside `sql_case` |
| Portability | All SQL dialects | All SQL dialects |

**Rule of thumb**: Use native lowering when your predicate is pure logic (no table references). Use explicit `sql_case` when you need conditional logic inside a table query.

## Complete Example

```prolog
% file: native_sql_example.pl
:- encoding(utf8).
:- use_module('src/unifyweaver/targets/sql_target').

% Temperature classification
temp_category(T, freezing) :- T < 0.
temp_category(T, cold)     :- T >= 0, T < 10.
temp_category(T, mild)     :- T >= 10, T < 20.
temp_category(T, warm)     :- T >= 20, T < 30.
temp_category(T, hot)      :- T >= 30.

% BMI classification
bmi_class(B, underweight) :- B < 18.5.
bmi_class(B, normal)      :- B >= 18.5, B < 25.
bmi_class(B, overweight)  :- B >= 25, B < 30.
bmi_class(B, obese)       :- B >= 30.

% Fibonacci-like with absolute value
abs_diff(X, Y, R) :- (X >= Y -> R is X - Y ; R is Y - X).

test :-
    format('~n=== Native Clause Lowering Examples ===~n~n'),

    format('--- Temperature (CASE SELECT) ---~n'),
    compile_predicate_to_sql(temp_category/2, [], SQL1),
    format('~w~n~n', [SQL1]),

    format('--- BMI (PL/pgSQL) ---~n'),
    compile_predicate_to_sql(bmi_class/2, [sql_output_mode(create_function)], SQL2),
    format('~w~n~n', [SQL2]),

    format('--- Abs Diff (CASE expression) ---~n'),
    compile_predicate_to_sql(abs_diff/3, [sql_output_mode(case_expression)], SQL3),
    format('~w~n~n', [SQL3]).

:- initialization(test, main).
```

## Exercises

1. **Basic**: Write a Prolog predicate `letter_grade(Score, Grade)` that maps numeric scores to letter grades (A/B/C/D/F). Compile it to SQL in all three modes.

2. **Arithmetic**: Write `tax_bracket(Income, Rate)` that returns the tax rate as a decimal. Use it in `case_expression` mode inside a `SELECT income, income * <your_expression> AS tax FROM taxpayers` query.

3. **PL/pgSQL**: Write `shipping_cost(Weight, Zone, Cost)` with different rates per zone and weight bracket. Generate it as a stored function and test with `SELECT shipping_cost(5, 'domestic')`.

4. **Comparison**: Rewrite the `salary_category` example from Chapter 11 using native lowering (no `sql_case`). Compare the generated SQL.

5. **Advanced**: Write a multi-argument `risk_score(Age, Income, CreditScore, Risk)` predicate with 4+ clauses combining multiple guards. Generate both CASE and PL/pgSQL versions.

## Summary

In this chapter, you learned:

- **Native clause body lowering** automatically compiles multi-clause Prolog predicates to SQL
- Three output modes: `case_select`, `create_function`, `case_expression`
- SQL uses `=` / `<>` / `AND` instead of Prolog's `=:=` / `=\=` / `,`
- If-then-else (`->` / `;`) compiles to nested CASE WHEN
- The `create_function` mode generates PL/pgSQL with `IF`/`ELSIF`/`RAISE EXCEPTION`
- Native lowering and explicit `sql_case` are complementary — use native for pure logic, explicit for table queries

This feature makes SQL the 28th and final target with native clause body lowering, completing UnifyWeaver's cross-target compilation coverage.

---

## Navigation

**←** [Previous: Chapter 12: Practical Applications](12_practical_applications) | [Book 10: SQL Target](./)
