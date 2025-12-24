<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: Advanced Features

The Go target supports advanced features for data extraction, filtering, and aggregation.

## Regex Matching

You can use the `match/2` predicate to filter records using Regular Expressions.

```prolog
% Filter lines containing 'ERROR'
error_log(Line) :-
    log(Line),
    match(Line, 'ERROR').
```

### Capture Groups

Extract data using regex capture groups with `match/4`.

```prolog
% Extract date and level from log
% Input: "2025-01-01 ERROR: Something happened"
parse_log(Date, Level) :-
    log(Line),
    match(Line, '^([0-9-]+) ([A-Z]+):', auto, [Date, Level]).
```

The compiler translates this into efficient Go `regexp` calls.

## Constraints

UnifyWeaver supports numeric constraints that are compiled into Go comparison operators.

```prolog
% Filter adults
adult(Name, Age) :-
    person(Name, Age),
    Age >= 18.
```

Supported operators: `>`, `<`, `>=`, `=<`, `==`, `\=`.

## Aggregations & Analytics

UnifyWeaver supports powerful aggregation capabilities for both database and stream processing, comparable to SQL.

### Simple Aggregations

Aggregate across all matching records.

```prolog
% Calculate sum of values
total_score(Sum) :-
    aggregate(sum(Score), json_record([score-Score]), Sum).

% Count records
record_count(Count) :-
    aggregate(count, json_record([_-_]), Count).
```

### Group By

Group records by one or more fields and compute multiple metrics.

```prolog
% Stats per city
city_stats(City, Count, AvgAge) :-
    group_by(City, 
             json_record([city-City, age-Age]), 
             [count(Count), avg(Age, AvgAge)]).
```

### Advanced Functions

- **Statistical**: `stddev(Field, Result)`, `median(Field, Result)`, `percentile(Field, 90, Result)`
- **Arrays**: `collect_list(Field, Result)`, `collect_set(Field, Result)`
- **Window**: `row_number(OrderField, Result)`, `rank(OrderField, Result)`

```prolog
% Advanced analytics
analysis(City, MedAge, TopAges) :-
    group_by(City, 
             json_record([city-City, age-Age]), 
             [median(Age, MedAge), collect_set(Age, TopAges)]).
```

### Filtering (HAVING)

Filter groups based on aggregation results.

```prolog
% High-value customers
vip_customers(Customer, TotalSpent) :-
    group_by(Customer, json_record([cust-Customer, amount-Amount]), sum(Amount, TotalSpent)),
    TotalSpent > 1000.
```

### Example: Log Analysis Pipeline

Combine these features to build a powerful log analysis tool:

```prolog
% Extract response time from web logs and find slow requests (> 500ms)
slow_request(Url, Time) :-
    log(Line),
    match(Line, 'GET (.+) ([0-9]+)ms', auto, [Url, TimeStr]),
    Time is TimeStr,  % Convert to number
    Time > 500.
```

Compile and run:
```bash
go build slow_request.go
cat access.log | ./slow_request
```

---

## Navigation

**←** [Previous: Chapter 2: Basic Compilation](02_basic_compilation) | [📖 Book 6: Go Target](./) | [Next: Chapter 4: JSON Processing →](04_json_processing)
