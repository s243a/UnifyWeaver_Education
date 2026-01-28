<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 4: Pipe Protocols - Questions

Q&A companion for [04_pipe_protocols_impl.md](./04_pipe_protocols_impl.md).

---

## Question Index

1. [What protocols are available for pipe communication?](#b07c04-q-protocols)
2. [What is the TSV protocol specification?](#b07c04-q-tsv-spec)
3. [What does generate_tsv_writer/3 do?](#b07c04-q-tsv-writer)
4. [What does generate_tsv_reader/3 do?](#b07c04-q-tsv-reader)
5. [What is the JSON Lines protocol?](#b07c04-q-json-lines)
6. [When should I use TSV vs JSON?](#b07c04-q-when-tsv-json)
7. [What is header negotiation?](#b07c04-q-header-negotiation)
8. [How does field mapping work?](#b07c04-q-field-mapping)
9. [What does generate_pipeline_script/3 do?](#b07c04-q-pipeline-script)
10. [How does format conversion work?](#b07c04-q-format-conversion)
11. [What are the performance characteristics?](#b07c04-q-performance)

---

## Questions and Answers

### <a id="b07c04-q-protocols"></a>Q1: What protocols are available for pipe communication?

**Answer**: Three protocols:

| Protocol | Use Case | Parsing |
|----------|----------|---------|
| TSV | Flat records, max speed | Split on tabs |
| JSON Lines | Nested structures | JSON parse |
| In-Process | Same runtime | Direct objects |

TSV is the default for simplicity and universality.

**See**: [Overview: Data Format Options](./04_pipe_protocols_impl.md#overview-data-format-options)

---

### <a id="b07c04-q-tsv-spec"></a>Q2: What is the TSV protocol specification?

**Answer**:
- **Encoding**: UTF-8
- **Record delimiter**: Newline (`\n`)
- **Field delimiter**: Tab (`\t`)
- **Escape sequences**: `\t`, `\n`, `\\`
- **NULL**: Empty field

TSV is universally parseable by any language.

**See**: [TSV Protocol](./04_pipe_protocols_impl.md#tsv-protocol)

---

### <a id="b07c04-q-tsv-writer"></a>Q3: What does generate_tsv_writer/3 do?

**Answer**: Generates code to output TSV records:

```prolog
generate_tsv_writer(awk, [name, age, city], Code).
```

AWK output:
```awk
{ print name "\t" age "\t" city }
```

**See**: [generate_tsv_writer/3](./04_pipe_protocols_impl.md#generate_tsv_writer3)

---

### <a id="b07c04-q-tsv-reader"></a>Q4: What does generate_tsv_reader/3 do?

**Answer**: Generates code to read TSV records:

```prolog
generate_tsv_reader(python, [name, age, city], Code).
```

Python output:
```python
def read_records():
    for line in sys.stdin:
        fields = line.rstrip('\n').split('\t')
        yield {'name': fields[0], 'age': fields[1], 'city': fields[2]}
```

**See**: [generate_tsv_reader/3](./04_pipe_protocols_impl.md#generate_tsv_reader3)

---

### <a id="b07c04-q-json-lines"></a>Q5: What is the JSON Lines protocol?

**Answer**: One JSON object per line, newline-delimited:

```json
{"name": "Alice", "age": 30, "address": {"city": "NYC"}}
{"name": "Bob", "age": 25, "address": {"city": "LA"}}
```

Advantages: nested structures, self-describing, API compatible.

**See**: [JSON Lines Protocol](./04_pipe_protocols_impl.md#json-lines-protocol)

---

### <a id="b07c04-q-when-tsv-json"></a>Q6: When should I use TSV vs JSON?

**Answer**:

| Use TSV When... | Use JSON When... |
|-----------------|------------------|
| Flat records | Nested structures |
| Simple types | Mixed type arrays |
| Maximum speed | Self-describing needed |
| Shell tools | API compatibility |

TSV is ~5x faster for simple records.

**See**: [When to Use Each Protocol](./04_pipe_protocols_impl.md#when-to-use-each-protocol)

---

### <a id="b07c04-q-header-negotiation"></a>Q7: What is header negotiation?

**Answer**: Headers specify schema before data:

```
#UNIFYWEAVER:v1:tsv
#FIELDS:name:string,age:int,salary:float
Alice	30	75000.00
```

Supported types: `string`, `int`, `float`, `bool`, `datetime`, `json`.

**See**: [Header Negotiation](./04_pipe_protocols_impl.md#header-negotiation)

---

### <a id="b07c04-q-field-mapping"></a>Q8: How does field mapping work?

**Answer**: `declare_connection/3` maps fields between stages:

```prolog
:- declare_connection(producer/2, consumer/2, [
    field_map([
        user_name -> name,
        user_age -> age,
        ignore(internal_id)
    ])
]).
```

This renames fields and drops unwanted ones.

**See**: [Field Mapping](./04_pipe_protocols_impl.md#field-mapping)

---

### <a id="b07c04-q-pipeline-script"></a>Q9: What does generate_pipeline_script/3 do?

**Answer**: Generates shell script connecting multiple stages:

```prolog
generate_pipeline_script(
    [step(filter, awk, 'filter.awk', []),
     step(transform, python, 'transform.py', [])],
    [input('data.tsv'), output('result.tsv')],
    Script
).
```

Generates:
```bash
cat "data.tsv" | awk -f "filter.awk" | python3 "transform.py" > "result.tsv"
```

**See**: [generate_pipeline_script/3](./04_pipe_protocols_impl.md#generate_pipeline_script3)

---

### <a id="b07c04-q-format-conversion"></a>Q10: How does format conversion work?

**Answer**: When adjacent stages use different formats, the glue layer inserts converters:

```prolog
:- declare_connection(tsv_producer/2, json_consumer/2, [
    format_in(tsv),
    format_out(json)
]).
```

Generates AWK converter:
```bash
awk -F'\t' '{ printf "{\"name\":\"%s\",\"age\":%s}\n", $1, $2 }'
```

**See**: [Format Conversion](./04_pipe_protocols_impl.md#format-conversion)

---

### <a id="b07c04-q-performance"></a>Q11: What are the performance characteristics?

**Answer**:

| Operation | TSV | JSON |
|-----------|-----|------|
| Parse record | ~1μs | ~5μs |
| Serialize record | ~0.5μs | ~3μs |
| Memory per record | ~100B | ~200B |

TSV is 5x faster for simple records. Choose JSON when you need nested data.

**See**: [Performance](./04_pipe_protocols_impl.md#performance)

---

## Summary

Pipe protocols provide:
- TSV for flat, high-performance streaming
- JSON Lines for structured/nested data
- Header negotiation for typed communication
- Field mapping for name translation
- Automatic format conversion between stages
