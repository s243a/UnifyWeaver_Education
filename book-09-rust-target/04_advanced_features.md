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

## Database Integration

The Rust target supports embedded database persistence using the `sled` crate.

### Basic Database Operations

```prolog
% Write mode - store records from JSONL input
compile_with_db_write :-
    compile_predicate_to_rust(user/3, [
        db_backend(sled),
        db_file('users.db'),
        db_key_field(name),
        db_mode(write),
        json_input(true)
    ], Code),
    write_rust_project(Code, 'output/db_writer').

% Read mode - query from database
compile_with_db_read :-
    compile_predicate_to_rust(user/3, [
        db_backend(sled),
        db_file('users.db'),
        db_key_field(name),
        db_mode(read)
    ], Code),
    write_rust_project(Code, 'output/db_reader').
```

### Key Strategies

Define how primary keys are generated:

```prolog
% Simple field key
compile_simple_key :-
    compile_predicate_to_rust(user/3, [
        db_backend(sled),
        db_key_field(email)
    ], Code).

% Composite key (dept:name)
compile_composite_key :-
    compile_predicate_to_rust(employee/3, [
        db_backend(sled),
        db_key_strategy(composite([field(dept), field(name)])),
        db_key_delimiter(':')
    ], Code).

% Hash-based key
compile_hash_key :-
    compile_predicate_to_rust(document/2, [
        db_backend(sled),
        db_key_strategy(hash(field(content), sha256))
    ], Code).

% UUID key
compile_uuid_key :-
    compile_predicate_to_rust(event/2, [
        db_backend(sled),
        db_key_strategy(uuid)
    ], Code).
```

### Secondary Indexes

Declare secondary indexes for faster lookups:

```prolog
% Declare index on email field
:- index(user/3, email).
:- index(user/3, age).

% The compiler will automatically:
% 1. Create index trees (index_user_email, index_user_age)
% 2. Update indexes during writes
% 3. Use indexes for optimized reads when possible
```

### Predicate Pushdown

The compiler automatically optimizes database queries:

```prolog
% Direct lookup - O(log n) when key field is constrained
find_user(Name, Email, Age) :-
    Name = alice,  % Constraint on key field
    user(Name, Email, Age).

% Prefix scan - O(k log n) for composite key prefix
find_dept_users(Dept, Name, Salary) :-
    Dept = engineering,  % First field of composite key
    employee(Dept, Name, Salary).

% Index scan - O(k log n) using secondary index
find_by_email(Name, Email, Age) :-
    Email = 'alice@example.com',  % Indexed field
    user(Name, Email, Age).
```

### Statistics Analysis

Generate tools to collect database statistics:

```prolog
compile_analyzer :-
    compile_predicate_to_rust(user/3, [
        db_backend(sled),
        db_file('users.db'),
        db_mode(analyze)
    ], Code),
    write_rust_project(Code, 'output/db_analyzer').
```

Output:
```json
{
  "tree": "user",
  "record_count": 10000,
  "avg_key_length": 12.5,
  "avg_value_length": 156.3,
  "field_counts": {"name": 10000, "email": 10000, "age": 9500},
  "field_cardinality": {"name": 10000, "email": 10000, "age": 80},
  "field_selectivity": {"name": 1.0, "email": 1.0, "age": 0.008}
}
```

**Cargo.toml:**
```toml
[dependencies]
sled = "0.34"
serde_json = "1.0"
```

## Feature Comparison with Go Target

| Feature | Rust | Go | Notes |
|---------|------|-----|-------|
| Statistical Aggs | ✅ | ✅ | Both support stddev, median, percentile |
| collect_list/set | ✅ | ✅ | Both output JSON arrays |
| Observability | ✅ | ✅ | Same options available |
| Window Functions | ✅ | ✅ | Both support LAG/LEAD/first/last |
| Database Integration | ✅ (sled) | ✅ (BoltDB) | Both support secondary indexes |

Choose Rust when you need:
- Maximum performance with zero-cost abstractions
- Compile-time memory safety guarantees
- Small binary size and minimal memory footprint
- Pure Rust embedded database (sled)

Choose Go when you need:
- BoltDB (battle-tested embedded database)
- Faster compilation times

---

## Navigation

**←** [Previous: Chapter 3: Project Generation](03_project_generation) | [📖 Book 9: Rust Target](./) | [Next: Book 10: SQL Target →](../book-10-sql-target/)
