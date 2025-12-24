# Chapter 2: Pipeline Mode

Pipeline mode generates C code that processes JSONL using cJSON.

## Generated Code Structure

```c
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "cJSON.h"

cJSON* process(cJSON* record) {
    /* Process and return, or NULL to filter out */
    return record;
}

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

int main(int argc, char** argv) {
    (void)argc; (void)argv;
    run_pipeline();
    return 0;
}
```

## cJSON Basics

| Function | Purpose |
|----------|---------|
| `cJSON_Parse()` | Parse JSON string |
| `cJSON_Delete()` | Free JSON object |
| `cJSON_GetObjectItem()` | Get field by name |
| `cJSON_PrintUnformatted()` | Serialize to string |

## Example Filter

```c
cJSON* process(cJSON* record) {
    cJSON* value = cJSON_GetObjectItem(record, "value");
    if (value && cJSON_IsNumber(value) && cJSON_GetNumberValue(value) > 50) {
        return record;
    }
    return NULL;  /* Filter out */
}
```

## Generating Pipeline Code

```prolog
?- compile_predicate_to_c(filter/2, [pipeline_input(true)], Code).
```

## Next Steps

- [Chapter 3: Generator Mode](03_generator_mode.md) - Callback-based iteration
