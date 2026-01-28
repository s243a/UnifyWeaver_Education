<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 4: Pipe Protocols - Implementation Details

This document provides function-level documentation for cross-target communication protocols.

**Source**: `src/unifyweaver/glue/pipe_glue.pl`

---

## Overview: Data Format Options

| Protocol | Use Case | Parsing |
|----------|----------|---------|
| TSV | Flat records, maximum speed | Split on tabs |
| JSON Lines | Nested structures, self-describing | JSON parse |
| In-Process | Same runtime, zero serialization | Direct objects |

---

## TSV Protocol

### Specification

```
┌─────────────────────────────────────────────────────┐
│                   TSV PROTOCOL                       │
├─────────────────────────────────────────────────────┤
│ Encoding: UTF-8                                      │
│ Record delimiter: Newline (\n)                       │
│ Field delimiter: Tab (\t)                            │
│ Escape: Backslash for \t, \n, \\                    │
│ NULL: Empty field                                    │
│ EOF: Close pipe                                      │
└─────────────────────────────────────────────────────┘
```

### Escape Sequences

| Character | Escape |
|-----------|--------|
| Tab | `\t` |
| Newline | `\n` |
| Backslash | `\\` |

### generate_tsv_writer/3

```prolog
generate_tsv_writer(+Target, +Fields, -Code)
```

**Example (AWK)**:
```prolog
generate_tsv_writer(awk, [name, age, city], Code).
```

Generates:
```awk
{
    print name "\t" age "\t" city
}
```

### generate_tsv_reader/3

```prolog
generate_tsv_reader(+Target, +Fields, -Code)
```

**Example (Python)**:
```prolog
generate_tsv_reader(python, [name, age, city], Code).
```

Generates:
```python
def read_records():
    for line in sys.stdin:
        fields = line.rstrip('\n').split('\t')
        yield {
            'name': fields[0],
            'age': fields[1],
            'city': fields[2]
        }
```

---

## JSON Lines Protocol

### Specification

```
┌─────────────────────────────────────────────────────┐
│                  JSON LINES                          │
├─────────────────────────────────────────────────────┤
│ One JSON object per line                             │
│ Encoding: UTF-8                                      │
│ Record delimiter: Newline (\n)                       │
│ No trailing comma                                    │
│ NULL: JSON null                                      │
└─────────────────────────────────────────────────────┘
```

### generate_json_writer/3

```prolog
generate_json_writer(+Target, +Fields, -Code)
```

**Example (Python)**:
```python
import json
import sys

def write_record(record):
    print(json.dumps(record))
    sys.stdout.flush()
```

### generate_json_reader/3

```prolog
generate_json_reader(+Target, +Fields, -Code)
```

**Example (Go)**:
```go
type Record struct {
    Name string `json:"name"`
    Age  int    `json:"age"`
    City string `json:"city"`
}

func readRecords() <-chan Record {
    ch := make(chan Record)
    go func() {
        scanner := bufio.NewScanner(os.Stdin)
        for scanner.Scan() {
            var r Record
            json.Unmarshal(scanner.Bytes(), &r)
            ch <- r
        }
        close(ch)
    }()
    return ch
}
```

---

## When to Use Each Protocol

| Use TSV When... | Use JSON When... |
|-----------------|------------------|
| Flat records | Nested structures |
| Simple types | Mixed types in arrays |
| Maximum speed | Self-describing needed |
| Shell tools | API compatibility |

---

## Header Negotiation

### Header Format

```
#UNIFYWEAVER:v1:tsv
#FIELDS:name:string,age:int,salary:float
Alice	30	75000.00
Bob	25	65000.00
```

### Type Specifications

| Type | Description | Example |
|------|-------------|---------|
| `string` | Text value | `"Alice"` |
| `int` | Integer | `30` |
| `float` | Floating point | `75000.00` |
| `bool` | Boolean | `true`, `false` |
| `datetime` | ISO 8601 | `2024-01-15T10:30:00Z` |
| `json` | Embedded JSON | `{"key": "value"}` |

---

## Field Mapping

### declare_connection/3

```prolog
:- declare_connection(producer/2, consumer/2, [
    field_map([
        user_name -> name,
        user_age -> age,
        ignore(internal_id)
    ])
]).
```

### Generated Mapping Code

```python
def map_fields(record):
    return {
        'name': record['user_name'],
        'age': record['user_age']
        # internal_id is dropped
    }
```

---

## generate_pipeline_script/3

Generates shell script connecting multiple stages.

### Signature

```prolog
generate_pipeline_script(+Steps, +Options, -Script)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Steps` | `list` | List of `step(Name, Lang, Code, Opts)` |
| `Options` | `list` | `[input(File), output(File)]` |
| `Script` | `string` | Generated bash script |

### Example

```prolog
generate_pipeline_script(
    [
        step(filter, awk, 'filter.awk', []),
        step(transform, python, 'transform.py', [format(json)]),
        step(aggregate, awk, 'aggregate.awk', [])
    ],
    [input('data.tsv'), output('result.tsv')],
    Script
).
```

**Generated**:
```bash
#!/bin/bash
set -euo pipefail

cat "data.tsv" \
    | awk -f "filter.awk" \
    | python3 "transform.py" \
    | awk -f "aggregate.awk" \
    > "result.tsv"
```

---

## Format Conversion

When adjacent stages use different formats:

```prolog
:- declare_connection(tsv_producer/2, json_consumer/2, [
    format_in(tsv),
    format_out(json)
]).
```

**Generated Converter**:
```bash
awk -F'\t' '{
    printf "{\"name\":\"%s\",\"age\":%s}\n", $1, $2
}'
```

---

## Error Handling

### TSV Error Recovery

```python
def read_tsv_safe():
    for line_num, line in enumerate(sys.stdin, 1):
        try:
            fields = line.rstrip('\n').split('\t')
            if len(fields) != expected_count:
                print(f"Warning: line {line_num} has {len(fields)} fields",
                      file=sys.stderr)
                continue
            yield fields
        except Exception as e:
            print(f"Error on line {line_num}: {e}", file=sys.stderr)
```

### JSON Error Recovery

```python
def read_json_safe():
    for line_num, line in enumerate(sys.stdin, 1):
        try:
            yield json.loads(line)
        except json.JSONDecodeError as e:
            print(f"Invalid JSON on line {line_num}: {e}", file=sys.stderr)
```

---

## Performance

### TSV vs JSON

| Operation | TSV | JSON |
|-----------|-----|------|
| Parse simple record | ~1μs | ~5μs |
| Serialize simple record | ~0.5μs | ~3μs |
| Memory per record | ~100B | ~200B |

### Buffering Options

```prolog
% Line buffering (default)
[buffer(line)]

% Block buffering
[buffer(block(65536))]

% No buffering
[buffer(none)]
```

---

## Related Documentation

- [Book 7 Chapter 3: Target Registry](../03_target_registry.md)
- [Book 7 Chapter 5: Shell Glue](../05_shell_glue.md)
- [Pipe Glue Source](../../../../src/unifyweaver/glue/pipe_glue.pl)
