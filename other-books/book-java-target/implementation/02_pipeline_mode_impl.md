<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Pipeline Mode - Implementation Details

This document provides function-level documentation for Java pipeline mode compilation.

**Source**: `src/unifyweaver/targets/java_target.pl`

---

## Overview: Pipeline Architecture

```java
BufferedReader → Stream<String> → parse → process → filter → output
```

Pipeline mode generates Java code that:
1. Reads JSONL from stdin
2. Parses each line to `Map<String, Object>`
3. Applies the compiled predicate
4. Writes results to stdout

---

## compile_predicate_to_java/3

Compiles a Prolog predicate to Java code.

### Signature

```prolog
compile_predicate_to_java(+Predicate/Arity, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicate/Arity` | `atom/integer` | Predicate to compile |
| `Options` | `list` | Compilation options |
| `Code` | `string` | Generated Java code |

### Options

| Option | Description |
|--------|-------------|
| `pipeline_input(true)` | Generate stdin reader |
| `class_name(Name)` | Custom class name |

### Example

```prolog
?- compile_predicate_to_java(filter/2, [pipeline_input(true)], Code),
   write_c_program(Code, 'FilterPipeline.java').
```

---

## Generated Code Structure

### Imports

```java
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import java.io.*;
import java.lang.reflect.Type;
import java.util.Map;
import java.util.Objects;
```

### Class Structure

```java
public class FilterPipeline {
    private static final Gson gson = new Gson();
    private static final Type mapType = new TypeToken<Map<String, Object>>(){}.getType();

    public static Map<String, Object> process(Map<String, Object> record) {
        // Compiled predicate logic
    }

    public static void runPipeline() {
        // Stream processing
    }

    public static void main(String[] args) {
        runPipeline();
    }
}
```

---

## Filter Predicate Compilation

### Prolog Pattern

```prolog
filter(Input, Output) :-
    get_field(Input, "value", Value),
    Value > 50,
    Output = Input.
```

### Generated Java

```java
public static Map<String, Object> process(Map<String, Object> record) {
    Object value = record.get("value");
    if (value instanceof Number && ((Number) value).doubleValue() > 50) {
        return record;
    }
    return null;  // Filter out
}
```

### Compilation Rules

| Prolog | Java |
|--------|------|
| `get_field(R, "key", V)` | `record.get("key")` |
| `V > N` | `((Number) v).doubleValue() > N` |
| `Output = Input` | `return record` |
| Failure | `return null` |

---

## Pipeline Runner

### Generated runPipeline()

```java
public static void runPipeline() {
    BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
    reader.lines()
        .filter(line -> !line.isEmpty())
        .map(line -> gson.fromJson(line, mapType))
        .map(FilterPipeline::process)
        .filter(Objects::nonNull)
        .forEach(result -> System.out.println(gson.toJson(result)));
}
```

### Stream Pipeline Steps

| Step | Operation |
|------|-----------|
| `reader.lines()` | Read lines from stdin |
| `.filter(line -> !line.isEmpty())` | Skip empty lines |
| `.map(line -> gson.fromJson(...))` | Parse JSON |
| `.map(FilterPipeline::process)` | Apply predicate |
| `.filter(Objects::nonNull)` | Remove filtered records |
| `.forEach(...)` | Output results |

---

## Transform Predicate

### Prolog Pattern

```prolog
transform(Input, Output) :-
    get_field(Input, "value", V),
    NewValue is V * 2,
    set_field(Input, "doubled", NewValue, Output).
```

### Generated Java

```java
public static Map<String, Object> process(Map<String, Object> record) {
    Object v = record.get("value");
    if (v instanceof Number) {
        double newValue = ((Number) v).doubleValue() * 2;
        Map<String, Object> output = new HashMap<>(record);
        output.put("doubled", newValue);
        return output;
    }
    return null;
}
```

---

## Dependencies

### Gson Library

```xml
<!-- Maven -->
<dependency>
    <groupId>com.google.code.gson</groupId>
    <artifactId>gson</artifactId>
    <version>2.10.1</version>
</dependency>
```

### Compilation

```bash
javac -cp gson.jar FilterPipeline.java
java -cp .:gson.jar FilterPipeline
```

---

## Related Documentation

- [Book Java Chapter 3: Generator Mode](../03_generator_mode.md)
- [Book Java Chapter 4: Recursive Queries](../04_recursive_queries.md)
- [Java Target Source](../../../../../src/unifyweaver/targets/java_target.pl)
