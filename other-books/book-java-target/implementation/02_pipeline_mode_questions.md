<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Pipeline Mode - Questions

Q&A companion for [02_pipeline_mode_impl.md](./02_pipeline_mode_impl.md).

---

## Question Index

1. [What does compile_predicate_to_java/3 do?](#bjava02-q-compile)
2. [What is the pipeline architecture?](#bjava02-q-architecture)
3. [How are filter predicates compiled?](#bjava02-q-filter)
4. [What does runPipeline() do?](#bjava02-q-run-pipeline)
5. [How is get_field compiled?](#bjava02-q-get-field)
6. [How are transforms compiled?](#bjava02-q-transform)
7. [What library is required?](#bjava02-q-dependencies)
8. [How do I run the generated code?](#bjava02-q-run)
9. [How is numeric comparison handled?](#bjava02-q-numeric)
10. [What happens when a predicate fails?](#bjava02-q-failure)

---

## Questions and Answers

### <a id="bjava02-q-compile"></a>Q1: What does compile_predicate_to_java/3 do?

**Answer**: Compiles a Prolog predicate to Java pipeline code:

```prolog
?- compile_predicate_to_java(filter/2, [pipeline_input(true)], Code).
```

Generates a complete Java class with `process()` and `runPipeline()`.

**See**: [compile_predicate_to_java/3](./02_pipeline_mode_impl.md#compile_predicate_to_java3)

---

### <a id="bjava02-q-architecture"></a>Q2: What is the pipeline architecture?

**Answer**: Stream-based JSONL processing:

```
stdin → BufferedReader → Stream<String> → parse → process → filter → stdout
```

Uses Java 8+ Streams API.

**See**: [Overview: Pipeline Architecture](./02_pipeline_mode_impl.md#overview-pipeline-architecture)

---

### <a id="bjava02-q-filter"></a>Q3: How are filter predicates compiled?

**Answer**: Filter conditions become `if` statements:

```prolog
filter(Input, Output) :- get_field(Input, "value", V), V > 50, Output = Input.
```

```java
if (value instanceof Number && ((Number) value).doubleValue() > 50) {
    return record;
}
return null;  // Filter out
```

**See**: [Filter Predicate Compilation](./02_pipeline_mode_impl.md#filter-predicate-compilation)

---

### <a id="bjava02-q-run-pipeline"></a>Q4: What does runPipeline() do?

**Answer**: Orchestrates the stream pipeline:

```java
reader.lines()
    .filter(line -> !line.isEmpty())
    .map(line -> gson.fromJson(line, mapType))
    .map(FilterPipeline::process)
    .filter(Objects::nonNull)
    .forEach(result -> System.out.println(gson.toJson(result)));
```

**See**: [Pipeline Runner](./02_pipeline_mode_impl.md#pipeline-runner)

---

### <a id="bjava02-q-get-field"></a>Q5: How is get_field compiled?

**Answer**: Becomes `Map.get()`:

| Prolog | Java |
|--------|------|
| `get_field(R, "key", V)` | `record.get("key")` |

**See**: [Compilation Rules](./02_pipeline_mode_impl.md#compilation-rules)

---

### <a id="bjava02-q-transform"></a>Q6: How are transforms compiled?

**Answer**: Create new map and add field:

```prolog
set_field(Input, "doubled", NewValue, Output)
```

```java
Map<String, Object> output = new HashMap<>(record);
output.put("doubled", newValue);
return output;
```

**See**: [Transform Predicate](./02_pipeline_mode_impl.md#transform-predicate)

---

### <a id="bjava02-q-dependencies"></a>Q7: What library is required?

**Answer**: Google Gson for JSON parsing:

```xml
<dependency>
    <groupId>com.google.code.gson</groupId>
    <artifactId>gson</artifactId>
    <version>2.10.1</version>
</dependency>
```

**See**: [Dependencies](./02_pipeline_mode_impl.md#dependencies)

---

### <a id="bjava02-q-run"></a>Q8: How do I run the generated code?

**Answer**:

```bash
javac -cp gson.jar FilterPipeline.java
echo '{"value": 75}' | java -cp .:gson.jar FilterPipeline
```

**See**: [Compilation](./02_pipeline_mode_impl.md#compilation)

---

### <a id="bjava02-q-numeric"></a>Q9: How is numeric comparison handled?

**Answer**: Cast to Number, then compare:

```java
if (value instanceof Number && ((Number) value).doubleValue() > 50)
```

Handles both Integer and Double JSON values.

**See**: [Filter Predicate Compilation](./02_pipeline_mode_impl.md#filter-predicate-compilation)

---

### <a id="bjava02-q-failure"></a>Q10: What happens when a predicate fails?

**Answer**: `process()` returns `null`, which is filtered by `Objects::nonNull`:

```java
.map(FilterPipeline::process)
.filter(Objects::nonNull)
```

**See**: [Stream Pipeline Steps](./02_pipeline_mode_impl.md#stream-pipeline-steps)

---

## Summary

Java pipeline mode provides:
- JSONL stdin/stdout processing
- Stream-based pipeline with method references
- Filter predicates returning null for exclusion
- Transform predicates creating new maps
- Gson library for JSON serialization
