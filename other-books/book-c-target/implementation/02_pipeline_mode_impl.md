<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Pipeline Mode - Implementation Details

This document provides function-level documentation for C pipeline mode compilation.

**Source**: `src/unifyweaver/targets/c_target.pl`

---

## Overview: Pipeline Architecture

```c
stdin → fgets() → cJSON_Parse() → process() → cJSON_Print() → stdout
```

Pipeline mode generates C code that:
1. Reads JSONL line-by-line from stdin
2. Parses using cJSON library
3. Applies the compiled predicate
4. Writes results to stdout

---

## compile_predicate_to_c/3

Compiles a Prolog predicate to C code.

### Signature

```prolog
compile_predicate_to_c(+Predicate/Arity, +Options, -Code)
```

### Options

| Option | Description |
|--------|-------------|
| `pipeline_input(true)` | Generate stdin reader |

### Example

```prolog
?- compile_predicate_to_c(filter/2, [pipeline_input(true)], Code).
```

---

## Generated Code Structure

### Includes

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cJSON.h"
```

### Process Function

```c
cJSON* process(cJSON* record) {
    /* Process and return, or NULL to filter out */
    return record;
}
```

### Pipeline Runner

```c
void run_pipeline(void) {
    char line[65536];

    while (fgets(line, sizeof(line), stdin)) {
        size_t len = strlen(line);
        if (len > 0 && line[len-1] == '\n') line[len-1] = '\0';
        if (strlen(line) == 0) continue;

        cJSON* record = cJSON_Parse(line);
        if (!record) {
            fprintf(stderr, "JSON parse error: %s\n", cJSON_GetErrorPtr());
            continue;
        }

        cJSON* result = process(record);
        if (result) {
            char* json_str = cJSON_PrintUnformatted(result);
            if (json_str) {
                printf("%s\n", json_str);
                free(json_str);
            }
            if (result != record) cJSON_Delete(result);
        }
        cJSON_Delete(record);
    }
}
```

---

## cJSON API Reference

| Function | Purpose |
|----------|---------|
| `cJSON_Parse(str)` | Parse JSON string |
| `cJSON_Delete(obj)` | Free JSON object |
| `cJSON_GetObjectItem(obj, key)` | Get field by name |
| `cJSON_IsNumber(item)` | Type check |
| `cJSON_GetNumberValue(item)` | Get numeric value |
| `cJSON_PrintUnformatted(obj)` | Serialize to string |

---

## Filter Compilation

### Prolog Pattern

```prolog
filter(Input, Output) :-
    get_field(Input, "value", Value),
    Value > 50,
    Output = Input.
```

### Generated C

```c
cJSON* process(cJSON* record) {
    cJSON* value = cJSON_GetObjectItem(record, "value");
    if (value && cJSON_IsNumber(value) && cJSON_GetNumberValue(value) > 50) {
        return record;
    }
    return NULL;  /* Filter out */
}
```

### Compilation Rules

| Prolog | C |
|--------|---|
| `get_field(R, "key", V)` | `cJSON_GetObjectItem(record, "key")` |
| `V > N` | `cJSON_GetNumberValue(value) > N` |
| `Output = Input` | `return record` |
| Failure | `return NULL` |

---

## Memory Management

### Record Lifecycle

```c
cJSON* record = cJSON_Parse(line);     // Allocated
cJSON* result = process(record);        // May return record or new
if (result != record) cJSON_Delete(result);  // Free if new
cJSON_Delete(record);                   // Always free original
```

### String Output

```c
char* json_str = cJSON_PrintUnformatted(result);
printf("%s\n", json_str);
free(json_str);  // Must free after print
```

---

## Dependencies

### cJSON Library

```bash
# Ubuntu/Debian
sudo apt install libcjson-dev

# macOS
brew install cjson

# Header location
#include <cjson/cJSON.h>
# or
#include "cJSON.h"
```

### Compilation

```bash
gcc -o filter filter.c -lcjson
# or with header path
gcc -o filter filter.c -I/usr/include/cjson -lcjson
```

---

## Error Handling

### JSON Parse Errors

```c
cJSON* record = cJSON_Parse(line);
if (!record) {
    fprintf(stderr, "JSON parse error: %s\n", cJSON_GetErrorPtr());
    continue;
}
```

### Type Checking

```c
if (value && cJSON_IsNumber(value)) {
    // Safe to use cJSON_GetNumberValue
}
```

---

## Related Documentation

- [Book C Chapter 3: Generator Mode](../03_generator_mode.md)
- [Book C Chapter 4: Build Systems](../04_build_systems.md)
- [C Target Source](../../../../../src/unifyweaver/targets/c_target.pl)
