<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Pipeline Mode - Implementation Details

This document provides function-level documentation for Kotlin pipeline mode compilation.

**Source**: `src/unifyweaver/targets/kotlin_target.pl`

---

## Overview: Pipeline Architecture

```kotlin
BufferedReader → lineSequence() → mapNotNull → process → forEach
```

Uses Kotlin sequences and lambda expressions for streaming.

---

## compile_predicate_to_kotlin/3

Compiles a Prolog predicate to Kotlin code.

### Signature

```prolog
compile_predicate_to_kotlin(+Predicate/Arity, +Options, -Code)
```

### Options

| Option | Description |
|--------|-------------|
| `pipeline_input(true)` | Generate stdin reader |

### Example

```prolog
?- compile_predicate_to_kotlin(filter/2, [pipeline_input(true)], Code).
```

---

## Generated Code Structure

### Object Declaration

```kotlin
import com.google.gson.Gson
import java.io.BufferedReader
import java.io.InputStreamReader

object FilterPipeline {
    private val gson = Gson()

    fun process(record: MutableMap<String, Any?>): MutableMap<String, Any?>? {
        // Compiled predicate
    }

    fun runPipeline() {
        // Stream processing
    }
}

fun main() = FilterPipeline.runPipeline()
```

---

## Filter Compilation

### Prolog Pattern

```prolog
filter(Input, Output) :-
    get_field(Input, "value", Value),
    Value > 50,
    Output = Input.
```

### Generated Kotlin

```kotlin
fun process(record: MutableMap<String, Any?>): MutableMap<String, Any?>? {
    val value = record["value"] as? Number
    return if (value != null && value.toDouble() > 50) record else null
}
```

### Compilation Rules

| Prolog | Kotlin |
|--------|--------|
| `get_field(R, "key", V)` | `record["key"] as? Type` |
| `V > N` | `value.toDouble() > N` |
| `Output = Input` | `return record` |
| Failure | `return null` |

---

## Pipeline Runner

### Generated runPipeline()

```kotlin
fun runPipeline() {
    BufferedReader(InputStreamReader(System.`in`))
        .lineSequence()
        .filter { it.isNotBlank() }
        .mapNotNull { line ->
            val record: MutableMap<String, Any?> = gson.fromJson(line, mapType)
            process(record)
        }
        .forEach { result ->
            println(gson.toJson(result))
        }
}
```

### Sequence Operations

| Operation | Purpose |
|-----------|---------|
| `lineSequence()` | Lazy line iterator |
| `.filter { it.isNotBlank() }` | Skip empty |
| `.mapNotNull { ... }` | Process, skip null |
| `.forEach { ... }` | Output results |

---

## Kotlin Idioms

### Safe Cast

```kotlin
val value = record["value"] as? Number
```

Returns `null` if cast fails instead of throwing.

### Elvis Operator

```kotlin
return if (value != null && value.toDouble() > 50) record else null
```

Could also use: `value?.takeIf { it.toDouble() > 50 }?.let { record }`

### Object Singleton

```kotlin
object FilterPipeline { }
```

Kotlin's singleton pattern, replaces Java's `public static` methods.

---

## Dependencies

### Gson Library

```kotlin
// build.gradle.kts
dependencies {
    implementation("com.google.code.gson:gson:2.10.1")
}
```

### Running

```bash
./gradlew run < input.jsonl
# or
echo '{"value": 75}' | kotlin FilterPipeline.kt
```

---

## Related Documentation

- [Book Kotlin Chapter 3: Generator Mode](../03_generator_mode.md)
- [Kotlin Target Source](../../../../../src/unifyweaver/targets/kotlin_target.pl)
