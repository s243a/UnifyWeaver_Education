<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
-->

# Chapter 4: Aggregations

The Perl target supports `aggregate_all/3` for computing summary values over query results.

## Supported Templates

| Template | Description | Init Value | Update |
|----------|-------------|------------|--------|
| `count` | Count matching tuples | `0` | `+= 1` |
| `sum(X)` | Sum of values | `0` | `+= X` |
| `min(X)` | Minimum value | `undef` | `= X if !defined or X <` |
| `max(X)` | Maximum value | `undef` | `= X if !defined or X >` |
| `avg(X)` | Average value | `[0, 0]` | `sum += X; count++` |

## Basic Usage

### Count

```prolog
employee_count(Count) :-
    aggregate_all(count, employee(_, _), Count).
```

Compiles to:

```perl
sub employee_count {
    my $callback = shift;
    my $agg_result = 0;

    employee(sub {
        my ($g1, $g2) = @_;
        $agg_result += 1;
    });

    $callback->($agg_result);
}
```

### Sum

```prolog
total_salary(Total) :-
    aggregate_all(sum(Salary), employee(_, Salary), Total).
```

Compiles to:

```perl
sub total_salary {
    my $callback = shift;
    my $agg_result = 0;

    employee(sub {
        my ($g1, $g2) = @_;
        my $tmpl_val = $g2;  # Salary
        $agg_result += $tmpl_val;
    });

    $callback->($agg_result);
}
```

### Min/Max

```prolog
min_price(Min) :-
    aggregate_all(min(Price), product(_, Price), Min).

max_price(Max) :-
    aggregate_all(max(Price), product(_, Price), Max).
```

Compiles to:

```perl
sub min_price {
    my $callback = shift;
    my $agg_result = undef;

    product(sub {
        my ($g1, $g2) = @_;
        my $tmpl_val = $g2;
        if (!defined($agg_result) || $tmpl_val < $agg_result) {
            $agg_result = $tmpl_val;
        }
    });

    $callback->($agg_result);
}

sub max_price {
    my $callback = shift;
    my $agg_result = undef;

    product(sub {
        my ($g1, $g2) = @_;
        my $tmpl_val = $g2;
        if (!defined($agg_result) || $tmpl_val > $agg_result) {
            $agg_result = $tmpl_val;
        }
    });

    $callback->($agg_result);
}
```

### Average

```prolog
avg_score(Avg) :-
    aggregate_all(avg(Score), student(_, Score), Avg).
```

Compiles to:

```perl
sub avg_score {
    my $callback = shift;
    my $agg_sum = 0;
    my $agg_count = 0;

    student(sub {
        my ($g1, $g2) = @_;
        my $tmpl_val = $g2;
        $agg_sum += $tmpl_val;
        $agg_count += 1;
    });

    my $agg_result = ($agg_count > 0) ? ($agg_sum / $agg_count) : 0;
    $callback->($agg_result);
}
```

## Complex Goals

Aggregations work with any goal, including joins:

```prolog
dept_salary_total(Dept, Total) :-
    aggregate_all(sum(Sal), (employee(Name, Dept), salary(Name, Sal)), Total).
```

The goal `(employee(Name, Dept), salary(Name, Sal))` compiles to nested callbacks before aggregation.

## Combining Aggregations

Multiple aggregations require separate predicates:

```prolog
salary_stats(Count, Total, Avg) :-
    employee_count(Count),
    total_salary(Total),
    (Count > 0 -> Avg is Total / Count ; Avg = 0).
```

## Practical Example

```prolog
:- dynamic order/3.  % order(Id, Product, Amount)
order(1, widget, 100).
order(2, gadget, 250).
order(3, widget, 150).
order(4, gadget, 300).

order_summary(Count, Total, MaxOrder) :-
    aggregate_all(count, order(_, _, _), Count),
    aggregate_all(sum(A), order(_, _, A), Total),
    aggregate_all(max(A), order(_, _, A), MaxOrder).
```

Usage:

```perl
order_summary(sub {
    my ($count, $total, $max) = @_;
    print "Orders: $count\n";
    print "Total: \$$total\n";
    print "Max order: \$$max\n";
});
```

Output:
```
Orders: 4
Total: $800
Max order: $300
```

## Exercises

### Exercise 4.1: Write an Aggregation

Write a Prolog predicate to find the average order amount:

<details>
<summary>Solution</summary>

```prolog
avg_order_amount(Avg) :-
    aggregate_all(avg(Amount), order(_, _, Amount), Avg).
```

</details>

### Exercise 4.2: Trace Aggregation

Given:
```prolog
product(apple, 2).
product(banana, 1).
product(cherry, 3).

total_items(Total) :- aggregate_all(sum(Q), product(_, Q), Total).
```

Trace the execution of `total_items(T)`:

<details>
<summary>Solution</summary>

```
1. Initialize: $agg_result = 0
2. product callback (apple, 2): $agg_result = 0 + 2 = 2
3. product callback (banana, 1): $agg_result = 2 + 1 = 3
4. product callback (cherry, 3): $agg_result = 3 + 3 = 6
5. Final: $callback->(6)
```

</details>

## Limitations

- **No GROUP BY**: Aggregations are global (over all matching tuples)
- **Single template**: Each `aggregate_all` uses one template
- **No HAVING**: Filter after aggregation must be manual

## Next Steps

In the next chapter, we'll explore JSON output and pipeline modes for shell integration.

→ [Chapter 5: JSON and Pipeline Modes](05_json_pipeline.md)
