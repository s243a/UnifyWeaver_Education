<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 4: Advanced Features

The Rust target provides high-performance data processing with statistical aggregations, collection operations, and comprehensive observability features.

## JSON Processing

The Rust target supports high-performance JSON processing using `serde` and `serde_json`.

### JSON Input

```prolog
:- json_schema(user, [field(name, string), field(age, integer)]).

user_info(Name, Age) :-
    json_record([name-Name, age-Age]).

compile_json :-
    compile_predicate_to_rust(user_info/2, [
        json_input(true),
        json_schema(user)
    ], Code),
    write_rust_project(Code, 'output/json_processor').
```

### JSON Output

```prolog
output_user(Name, Age) :- input_data(Name, Age).

compile_output :-
    compile_predicate_to_rust(output_user/2, [json_output(true)], Code),
    write_rust_project(Code, 'output/json_writer').
```

### Schema Validation

Define schemas with field constraints for data validation:

```prolog
% Define a user schema with constraints
:- json_schema(user, [
    field(name, string, [required, min(1), max(100)]),
    field(age, integer, [required, min(0), max(150)]),
    field(email, string, [required, format(email)]),
    field(bio, string, [optional, max(500)])
]).

% Nested schema with address
:- json_schema(customer, [
    field(name, string, [required]),
    field(address, object(address_schema), [optional])
]).

:- json_schema(address_schema, [
    field(street, string, [required]),
    field(city, string, [required])
]).
```

**Supported Types:**
- `string`, `integer`, `float`, `boolean`, `any`
- `array(Type)` - Typed arrays
- `object(SchemaName)` - Nested schemas

**Field Options:**
- `required` / `optional` - Field presence
- `min(N)`, `max(N)` - Value/length constraints
- `format(email)`, `format(date)` - Format validation

**Generate Validators:**
```prolog
?- generate_rust_schema_validator(user, Code).
% Generates: fn validate_user(data: &serde_json::Value) -> bool { ... }
```

## XML Processing

Stream and process XML files using the `quick-xml` crate:

```prolog
% Define XML field extraction
book_info(Title, Author) :-
    xml_field(title, Title),
    xml_field(author, Author).

% Compile for XML input
compile_books :-
    compile_rust_xml_mode(book_info, 2, [
        xml_file("library.xml"),
        tags([book]),
        unique(true)
    ], Code),
    write_rust_project(Code, 'output/book_processor').
```

**Features:**
- Streaming parsing (memory-efficient)
- Attribute extraction with `@` prefix
- Tag filtering with `tags([...])`
- Deduplication with `unique(true)`

**Cargo.toml:**
```toml
[dependencies]
quick-xml = "0.31"
```

## Aggregations

### Basic Aggregations

Rust target supports efficient aggregations compiled to native Rust iterators.

```prolog
total_sales(Sum) :-
    aggregation(sum),
    sale(Sum).
```

Supported: `sum`, `count`, `min`, `max`, `avg`.

### Statistical Aggregations

The Rust target provides advanced statistical functions for analytics:

```prolog
% Calculate standard deviation of scores
score_stddev(StdDev) :-
    aggregation(stddev),
    score(StdDev).

% Find the median value
median_price(Median) :-
    aggregation(median),
    price(Median).

% Calculate 95th percentile
percentile_95(P95) :-
    aggregation(percentile(95)),
    response_time(P95).
```

**Supported statistical aggregations:**
- `stddev` - Sample standard deviation
- `median` - Median value (50th percentile)
- `percentile(N)` - Nth percentile (0-100)

### Collection Aggregations

Aggregate values into JSON arrays:

```prolog
% Collect all tags (with duplicates)
all_tags(Tags) :-
    aggregation(collect_list),
    tag(Tags).

% Collect unique categories (sorted)
unique_categories(Categories) :-
    aggregation(collect_set),
    category(Categories).
```

**Output examples:**
```json
// collect_list preserves duplicates
["rust", "go", "rust", "python"]

// collect_set produces sorted unique values
["go", "python", "rust"]
```

## Observability

The Rust target provides comprehensive observability features for production data pipelines.

### Progress Reporting

Report processing progress to stderr at regular intervals:

```prolog
compile_with_progress :-
    compile_predicate_to_rust(process/2, [
        json_input(true),
        progress(interval(10000))  % Report every 10,000 records
    ], Code),
    write_rust_project(Code, 'output/processor').
```

Output:
```
Processed 10000 records...
Processed 20000 records...
Processed 30000 records...
Completed: 35000 records in 2.34s
```

### Error Logging

Log parsing and validation errors to a JSON file:

```prolog
compile_with_error_log :-
    compile_predicate_to_rust(process/2, [
        json_input(true),
        error_file("errors.jsonl")
    ], Code),
    write_rust_project(Code, 'output/processor').
```

Error log format:
```json
{"timestamp": "2025-12-25T10:30:00Z", "error": "parse error", "line": "invalid data"}
{"timestamp": "2025-12-25T10:30:01Z", "error": "missing field: name", "line": "{\"age\": 25}"}
```

### Error Threshold

Exit processing after a maximum number of errors:

```prolog
compile_with_threshold :-
    compile_predicate_to_rust(process/2, [
        json_input(true),
        error_threshold(count(100))  % Exit after 100 errors
    ], Code),
    write_rust_project(Code, 'output/processor').
```

### Metrics Export

Export processing statistics to a JSON file:

```prolog
compile_with_metrics :-
    compile_predicate_to_rust(process/2, [
        json_input(true),
        metrics_file("metrics.json")
    ], Code),
    write_rust_project(Code, 'output/processor').
```

Metrics output:
```json
{
  "total_records": 50000,
  "error_records": 5,
  "duration_secs": 2.345,
  "records_per_sec": 21322.6
}
```

### Combined Observability

For production pipelines, combine all observability features:

```prolog
compile_production :-
    compile_predicate_to_rust(etl_pipeline/3, [
        json_input(true),
        json_output(true),
        progress(interval(10000)),
        error_file("errors.jsonl"),
        error_threshold(count(1000)),
        metrics_file("metrics.json")
    ], Code),
    write_rust_project(Code, 'output/etl_processor').
```

## Window Functions

The Rust target provides full window function support for analytics queries.

### Ranking Functions

```prolog
% Rank items by score
ranked_items(Id, Score, Rank) :-
    item(Id, Score),
    rank(Score, Rank).

% Row numbers for pagination
paginated(Id, Name, RowNum) :-
    record(Id, Name),
    row_number(Id, RowNum).
```

**Supported ranking functions:**
- `row_number/2` - Sequential numbering within partition
- `rank/2` - Rank with gaps for ties
- `dense_rank/2` - Rank without gaps

### LAG and LEAD Functions

Access values from previous or following rows - essential for time-series analysis.

```prolog
% Calculate day-over-day change
daily_change(Date, Price, PrevPrice, Change) :-
    stock_price(Date, Price),
    lag(Date, Price, 1, 0, PrevPrice),
    Change is Price - PrevPrice.

% Look ahead to next value
next_value(Date, Value, NextValue) :-
    time_series(Date, Value),
    lead(Date, Value, 1, null, NextValue).
```

**LAG function variants:**
- `lag(SortField, ValueField, Result)` - Previous value (offset 1, default null)
- `lag(SortField, ValueField, Offset, Result)` - Previous by N rows
- `lag(SortField, ValueField, Offset, Default, Result)` - With custom default

**LEAD function variants:**
- `lead(SortField, ValueField, Result)` - Next value (offset 1, default null)
- `lead(SortField, ValueField, Offset, Result)` - Next by N rows
- `lead(SortField, ValueField, Offset, Default, Result)` - With custom default

### First and Last Value

Access boundary values within a window partition.

```prolog
% Get first and last price in each session
session_bounds(Session, FirstPrice, LastPrice) :-
    trade(Session, Time, Price),
    first_value(Time, Price, FirstPrice),
    last_value(Time, Price, LastPrice).
```

## Constraints

Numeric constraints are compiled to native Rust comparisons.

```prolog
high_value(Val) :-
    input(Val),
    Val > 1000,
    Val =< 5000.
```

## Feature Comparison with Go Target

| Feature | Rust | Go | Notes |
|---------|------|-----|-------|
| Statistical Aggs | ✅ | ✅ | Both support stddev, median, percentile |
| collect_list/set | ✅ | ✅ | Both output JSON arrays |
| Observability | ✅ | ✅ | Same options available |
| Window Functions | ✅ | ✅ | Both support LAG/LEAD/first/last |
| Database Integration | ❌ | ✅ | Use Go for BoltDB |

Choose Rust when you need:
- Maximum performance with zero-cost abstractions
- Compile-time memory safety guarantees
- Small binary size and minimal memory footprint

Choose Go when you need:
- Embedded database support (BoltDB)
- Faster compilation times

---

## Navigation

**←** [Previous: Chapter 3: Project Generation](03_project_generation) | [📖 Book 9: Rust Target](./) | [Next: Book 10: SQL Target →](../book-10-sql-target/)
