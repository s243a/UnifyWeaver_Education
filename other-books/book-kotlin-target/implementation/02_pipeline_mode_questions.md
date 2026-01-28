<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Pipeline Mode - Questions

Q&A companion for [02_pipeline_mode_impl.md](./02_pipeline_mode_impl.md).

---

## Question Index

1. [What does compile_predicate_to_kotlin/3 do?](#bkotlin02-q-compile)
2. [What is the pipeline architecture?](#bkotlin02-q-architecture)
3. [How are filter predicates compiled?](#bkotlin02-q-filter)
4. [What is lineSequence()?](#bkotlin02-q-sequence)
5. [What is safe cast (as?)?](#bkotlin02-q-cast)
6. [What is mapNotNull?](#bkotlin02-q-mapnotnull)
7. [What is object singleton?](#bkotlin02-q-object)
8. [How do I run the generated code?](#bkotlin02-q-run)
9. [What library is required?](#bkotlin02-q-deps)
10. [How does null handling work?](#bkotlin02-q-null)

---

## Questions and Answers

### <a id="bkotlin02-q-compile"></a>Q1: What does compile_predicate_to_kotlin/3 do?

**Answer**: Compiles a Prolog predicate to Kotlin pipeline code:

```prolog
?- compile_predicate_to_kotlin(filter/2, [pipeline_input(true)], Code).
```

Generates object with `process()` and `runPipeline()`.

**See**: [compile_predicate_to_kotlin/3](./02_pipeline_mode_impl.md#compile_predicate_to_kotlin3)

---

### <a id="bkotlin02-q-architecture"></a>Q2: What is the pipeline architecture?

**Answer**: Lazy sequence processing:

```
BufferedReader → lineSequence() → mapNotNull → process → forEach
```

**See**: [Overview: Pipeline Architecture](./02_pipeline_mode_impl.md#overview-pipeline-architecture)

---

### <a id="bkotlin02-q-filter"></a>Q3: How are filter predicates compiled?

**Answer**: Safe cast and conditional:

```kotlin
val value = record["value"] as? Number
return if (value != null && value.toDouble() > 50) record else null
```

**See**: [Filter Compilation](./02_pipeline_mode_impl.md#filter-compilation)

---

### <a id="bkotlin02-q-sequence"></a>Q4: What is lineSequence()?

**Answer**: Lazy iterator over lines from reader:

```kotlin
BufferedReader(InputStreamReader(System.`in`))
    .lineSequence()  // Lazy, reads on demand
```

**See**: [Pipeline Runner](./02_pipeline_mode_impl.md#pipeline-runner)

---

### <a id="bkotlin02-q-cast"></a>Q5: What is safe cast (as?)?

**Answer**: Returns null instead of throwing on failed cast:

```kotlin
val value = record["value"] as? Number
// value is Number? (nullable)
```

**See**: [Safe Cast](./02_pipeline_mode_impl.md#safe-cast)

---

### <a id="bkotlin02-q-mapnotnull"></a>Q6: What is mapNotNull?

**Answer**: Maps and filters null results in one step:

```kotlin
.mapNotNull { line ->
    val record = gson.fromJson(line, mapType)
    process(record)  // null filtered out
}
```

**See**: [Sequence Operations](./02_pipeline_mode_impl.md#sequence-operations)

---

### <a id="bkotlin02-q-object"></a>Q7: What is object singleton?

**Answer**: Kotlin's singleton pattern:

```kotlin
object FilterPipeline {
    // All members are effectively static
}
```

Replaces Java's `public static` methods.

**See**: [Object Singleton](./02_pipeline_mode_impl.md#object-singleton)

---

### <a id="bkotlin02-q-run"></a>Q8: How do I run the generated code?

**Answer**:

```bash
./gradlew run < input.jsonl
# or
echo '{"value": 75}' | kotlin FilterPipeline.kt
```

**See**: [Running](./02_pipeline_mode_impl.md#running)

---

### <a id="bkotlin02-q-deps"></a>Q9: What library is required?

**Answer**: Gson for JSON:

```kotlin
// build.gradle.kts
implementation("com.google.code.gson:gson:2.10.1")
```

**See**: [Dependencies](./02_pipeline_mode_impl.md#dependencies)

---

### <a id="bkotlin02-q-null"></a>Q10: How does null handling work?

**Answer**: Kotlin null safety:

- `as?` returns null on bad cast
- `mapNotNull` filters nulls
- `process()` returns `null` to filter out

**See**: [Safe Cast](./02_pipeline_mode_impl.md#safe-cast)

---

## Summary

Kotlin pipeline mode provides:
- Lazy sequences via `lineSequence()`
- Safe casts with `as?`
- Null filtering via `mapNotNull`
- Object singleton for static context
- Gson for JSON serialization
