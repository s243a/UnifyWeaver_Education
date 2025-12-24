<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 4: JSON Processing

The Go target provides native support for JSON Lines (JSONL) processing, allowing you to build high-performance JSON transformation pipelines.

## JSON Input

To process JSON input, use the `json_input(true)` option. The generated program will parse each line of stdin as a JSON object.

### Field Extraction

Use `json_get/3` to extract fields from the JSON record.

```prolog
% Input: {"name": "alice", "age": 25, "address": {"city": "New York"}}
% Rule: Extract name and city
user_city(Name, City) :-
    json_get([name], Name),
    json_get([address, city], City).
```

### Compilation

```prolog
compile_predicate_to_go(user_city/2, [json_input(true)], Code).
```

### Nested Fields

You can extract deeply nested fields using a list path:

```prolog
% Extract user.address.zipcode
zip_code(Zip) :-
    json_get([user, address, zipcode], Zip).
```

### Array Iteration

Use `json_array_member/2` to iterate over JSON arrays. This generates a Go loop that processes each item in the array.

```prolog
% Input: {"users": [{"name": "alice"}, {"name": "bob"}]}
% Output: alice, bob
user_name(Name) :-
    json_get([users], UserList),
    json_array_member(UserList, User),
    json_get(User, [name], Name).
```

This supports nested iteration as well (e.g., lists of objects containing lists).

## JSON Output

You can also output JSON by using `json_output(true)`.

```prolog
% Transform input JSON to output JSON
% Input: {"id": 1, "val": 100}
% Output: {"id": 1, "status": "high"}
transform(Id, Status) :-
    json_get([id], Id),
    json_get([val], Val),
    Val > 50,
    Status = 'high'.
```

When compiled with `json_output(true)`, the Go program will automatically marshal the result variables into a JSON object.

## Schema Validation

You can define a schema to validate input JSON and ensure type safety.

```prolog
:- json_schema(user, [
    field(name, string, [required]),
    field(age, integer, [min(0)]),
    field(email, string, [format(email)])
]).

valid_user(Name) :-
    json_validate(user),
    json_get([name], Name).
```

## Stream Observability

For large-scale data processing, you can enable observability features to track progress and capture errors.

### Error Aggregation

The `error_file(Path)` option captures malformed records and validation failures into a separate JSONL file instead of dropping them.

```prolog
compile_predicate_to_go(process/1, [
    json_input(true),
    error_file('errors.jsonl')
], Code).
```

### Progress Reporting

The `progress(interval(N))` option prints processing statistics to stderr every N records.

```prolog
compile_predicate_to_go(process/1, [
    json_input(true),
    progress(interval(1000))
], Code).
```

### Fail-Fast with Thresholds

Use `error_threshold(count(N))` to stop processing if the error count exceeds a specific limit.

```prolog
compile_predicate_to_go(process/1, [
    json_input(true),
    error_threshold(count(100))
], Code).
```

## Example: JSON ETL Pipeline

```prolog
% Read users, filter active ones, and output simplified record
active_user_summary(Id, Name) :-
    json_get([active], true),
    json_get([id], Id),
    json_get([name], Name).

compile_etl :-
    compile_predicate_to_go(active_user_summary/2, [
        json_input(true),
        json_output(true)
    ], Code),
    write_go_program(Code, 'etl.go').
```

Run:
```bash
go build etl.go
echo '{"id":1, "name":"alice", "active":true}' | ./etl
# Output: {"arg1":1, "arg2":"alice"}
```

---

## Navigation

**←** [Previous: Chapter 3: Advanced Features](03_advanced_features) | [📖 Book 6: Go Target](./) | [Next: Chapter 5: Semantic Crawling & RDF Processing →](05_semantic_crawling)
