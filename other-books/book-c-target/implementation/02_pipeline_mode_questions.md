<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Pipeline Mode - Questions

Q&A companion for [02_pipeline_mode_impl.md](./02_pipeline_mode_impl.md).

---

## Question Index

1. [What does compile_predicate_to_c/3 do?](#bc02-q-compile)
2. [What is the pipeline architecture?](#bc02-q-architecture)
3. [What cJSON functions are used?](#bc02-q-cjson)
4. [How are filter predicates compiled?](#bc02-q-filter)
5. [How is memory managed?](#bc02-q-memory)
6. [How do I install cJSON?](#bc02-q-install)
7. [How do I compile the generated code?](#bc02-q-build)
8. [How are JSON parse errors handled?](#bc02-q-errors)
9. [How is type checking done?](#bc02-q-types)
10. [What happens when process returns NULL?](#bc02-q-null)

---

## Questions and Answers

### <a id="bc02-q-compile"></a>Q1: What does compile_predicate_to_c/3 do?

**Answer**: Compiles a Prolog predicate to C pipeline code:

```prolog
?- compile_predicate_to_c(filter/2, [pipeline_input(true)], Code).
```

Generates `process()` function and `run_pipeline()` main loop.

**See**: [compile_predicate_to_c/3](./02_pipeline_mode_impl.md#compile_predicate_to_c3)

---

### <a id="bc02-q-architecture"></a>Q2: What is the pipeline architecture?

**Answer**: Line-by-line JSONL processing:

```
stdin → fgets() → cJSON_Parse() → process() → cJSON_Print() → stdout
```

Uses 64KB line buffer.

**See**: [Overview: Pipeline Architecture](./02_pipeline_mode_impl.md#overview-pipeline-architecture)

---

### <a id="bc02-q-cjson"></a>Q3: What cJSON functions are used?

**Answer**:

| Function | Purpose |
|----------|---------|
| `cJSON_Parse()` | Parse JSON string |
| `cJSON_GetObjectItem()` | Get field |
| `cJSON_IsNumber()` | Type check |
| `cJSON_GetNumberValue()` | Get value |
| `cJSON_PrintUnformatted()` | Serialize |
| `cJSON_Delete()` | Free memory |

**See**: [cJSON API Reference](./02_pipeline_mode_impl.md#cjson-api-reference)

---

### <a id="bc02-q-filter"></a>Q4: How are filter predicates compiled?

**Answer**: Become `if` statements with cJSON calls:

```c
cJSON* value = cJSON_GetObjectItem(record, "value");
if (value && cJSON_IsNumber(value) && cJSON_GetNumberValue(value) > 50) {
    return record;
}
return NULL;
```

**See**: [Filter Compilation](./02_pipeline_mode_impl.md#filter-compilation)

---

### <a id="bc02-q-memory"></a>Q5: How is memory managed?

**Answer**: Manual allocation/free:

```c
cJSON* record = cJSON_Parse(line);     // Allocated
cJSON* result = process(record);        // May be record or new
if (result != record) cJSON_Delete(result);  // Free if new
cJSON_Delete(record);                   // Always free original
```

**See**: [Memory Management](./02_pipeline_mode_impl.md#memory-management)

---

### <a id="bc02-q-install"></a>Q6: How do I install cJSON?

**Answer**:

```bash
# Ubuntu/Debian
sudo apt install libcjson-dev

# macOS
brew install cjson
```

**See**: [Dependencies](./02_pipeline_mode_impl.md#dependencies)

---

### <a id="bc02-q-build"></a>Q7: How do I compile the generated code?

**Answer**:

```bash
gcc -o filter filter.c -lcjson
```

With include path if needed:
```bash
gcc -o filter filter.c -I/usr/include/cjson -lcjson
```

**See**: [Compilation](./02_pipeline_mode_impl.md#compilation)

---

### <a id="bc02-q-errors"></a>Q8: How are JSON parse errors handled?

**Answer**: Print to stderr and continue:

```c
if (!record) {
    fprintf(stderr, "JSON parse error: %s\n", cJSON_GetErrorPtr());
    continue;
}
```

**See**: [Error Handling](./02_pipeline_mode_impl.md#error-handling)

---

### <a id="bc02-q-types"></a>Q9: How is type checking done?

**Answer**: Use cJSON type functions before access:

```c
if (value && cJSON_IsNumber(value)) {
    double v = cJSON_GetNumberValue(value);
}
```

**See**: [Type Checking](./02_pipeline_mode_impl.md#type-checking)

---

### <a id="bc02-q-null"></a>Q10: What happens when process returns NULL?

**Answer**: Record is filtered out (not printed):

```c
cJSON* result = process(record);
if (result) {
    // Only print non-NULL results
    printf("%s\n", cJSON_PrintUnformatted(result));
}
```

**See**: [Pipeline Runner](./02_pipeline_mode_impl.md#pipeline-runner)

---

## Summary

C pipeline mode provides:
- JSONL stdin/stdout processing via cJSON
- `process()` returns record or NULL
- Manual memory management
- Type-safe field access with cJSON_Is* checks
- Error reporting to stderr
