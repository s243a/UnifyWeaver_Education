<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
-->

# Chapter 4: Aggregations

The Ruby target supports `aggregate_all/3` for computing summary values over query results.

## Supported Templates

| Template | Description | Init Value | Update |
|----------|-------------|------------|--------|
| `count` | Count matching tuples | `0` | `+= 1` |
| `sum(X)` | Sum of values | `0` | `+= X` |
| `min(X)` | Minimum value | `nil` | `= X if nil or X <` |
| `max(X)` | Maximum value | `nil` | `= X if nil or X >` |
| `avg(X)` | Average value | `[0, 0]` | `sum += X; count += 1` |

## Basic Usage

### Count

```prolog
employee_count(Count) :-
    aggregate_all(count, employee(_, _), Count).
```

Compiles to:

```ruby
def employee_count(&block)
  agg_result = 0

  employee do |g1, g2|
    agg_result += 1
  end

  block.call(agg_result)
end
```

### Sum

```prolog
total_salary(Total) :-
    aggregate_all(sum(Salary), employee(_, Salary), Total).
```

Compiles to:

```ruby
def total_salary(&block)
  agg_result = 0

  employee do |g1, g2|
    tmpl_val = g2  # Salary
    agg_result += tmpl_val
  end

  block.call(agg_result)
end
```

### Min/Max

```prolog
min_price(Min) :-
    aggregate_all(min(Price), product(_, Price), Min).

max_price(Max) :-
    aggregate_all(max(Price), product(_, Price), Max).
```

Compiles to:

```ruby
def min_price(&block)
  agg_result = nil

  product do |g1, g2|
    tmpl_val = g2
    if agg_result.nil? || tmpl_val < agg_result
      agg_result = tmpl_val
    end
  end

  block.call(agg_result)
end

def max_price(&block)
  agg_result = nil

  product do |g1, g2|
    tmpl_val = g2
    if agg_result.nil? || tmpl_val > agg_result
      agg_result = tmpl_val
    end
  end

  block.call(agg_result)
end
```

### Average

```prolog
avg_score(Avg) :-
    aggregate_all(avg(Score), student(_, Score), Avg).
```

Compiles to:

```ruby
def avg_score(&block)
  agg_sum = 0
  agg_count = 0

  student do |g1, g2|
    tmpl_val = g2
    agg_sum += tmpl_val
    agg_count += 1
  end

  agg_result = agg_count > 0 ? (agg_sum.to_f / agg_count) : 0
  block.call(agg_result)
end
```

Note: Ruby uses `to_f` to ensure floating-point division.

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

```ruby
# Each aggregation needs separate calls
order_count { |count| @count = count }
order_total { |total| @total = total }
order_max { |max| @max = max }

puts "Orders: #{@count}"
puts "Total: $#{@total}"
puts "Max order: $#{@max}"
```

Output:
```
Orders: 4
Total: $800
Max order: $300
```

## Ruby Idioms

### Safe Navigation for nil

Ruby's safe navigation can handle nil results:

```ruby
min_price { |min| puts min&.to_s || "No data" }
```

### Chaining with tap

```ruby
result = nil
order_total { |t| result = t }
result.tap { |r| puts "Total: #{r}" if r }
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

Trace the execution of `total_items`:

<details>
<summary>Solution</summary>

```
1. Initialize: agg_result = 0
2. product block (apple, 2): agg_result = 0 + 2 = 2
3. product block (banana, 1): agg_result = 2 + 1 = 3
4. product block (cherry, 3): agg_result = 3 + 3 = 6
5. Final: block.call(6)
```

</details>

## Limitations

- **No GROUP BY**: Aggregations are global (over all matching tuples)
- **Single template**: Each `aggregate_all` uses one template
- **No HAVING**: Filter after aggregation must be manual

## Next Steps

In the next chapter, we'll explore JSON output and Rails integration.

→ [Chapter 5: JSON and Rails Integration](05_json_rails.md)
