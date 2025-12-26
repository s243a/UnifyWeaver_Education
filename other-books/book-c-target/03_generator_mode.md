# Chapter 3: Generator Mode

Generator mode uses a callback pattern since C doesn't have coroutines.

## Generated Code Structure

```c
typedef void (*record_callback)(cJSON* record, void* user_data);

int generate(cJSON* record, record_callback callback, void* user_data) {
    /* Call callback for each result */
    callback(record, user_data);
    return 1;  /* Number of results */
}

void run_generator(record_callback callback, void* user_data) {
    char line[65536];
    
    while (fgets(line, sizeof(line), stdin)) {
        cJSON* record = cJSON_Parse(line);
        if (record) {
            generate(record, callback, user_data);
            cJSON_Delete(record);
        }
    }
}

void print_result(cJSON* record, void* user_data) {
    (void)user_data;
    char* json_str = cJSON_PrintUnformatted(record);
    if (json_str) {
        printf("%s\n", json_str);
        free(json_str);
    }
}

int main(void) {
    run_generator(print_result, NULL);
    return 0;
}
```

## Callback Pattern

Since C lacks generators, we use callbacks:

| Prolog | C Callback |
|--------|------------|
| Multiple solutions | Call callback multiple times |
| Backtracking | Iterate and call |
| Lazy evaluation | Process on demand |

## Example: Expanding Arrays

```c
/* This function requires the record_callback typedef from earlier.
   See the "Generated Code Structure" section above. */
int generate(cJSON* record, record_callback callback, void* user_data) {
    cJSON* items = cJSON_GetObjectItem(record, "items");
    if (!cJSON_IsArray(items)) return 0;
    
    int count = 0;
    cJSON* item;
    cJSON_ArrayForEach(item, items) {
        cJSON* result = cJSON_Duplicate(record, 1);
        cJSON_DeleteItemFromObject(result, "items");
        cJSON_AddItemToObject(result, "item", cJSON_Duplicate(item, 1));
        callback(result, user_data);
        cJSON_Delete(result);
        count++;
    }
    return count;
}
```

## Recursive Generators

For recursive predicates, use explicit iteration:

```c
/* This function also requires the record_callback typedef.
   It demonstrates explicit iteration for recursive predicates.
   See the "Generated Code Structure" section above. */
int generate(cJSON* record, record_callback callback, void* user_data) {
    cJSON* current = cJSON_Duplicate(record, 1);
    int count = 0;
    
    for (int i = 0; i < 10000; i++) {
        callback(current, user_data);
        count++;
        
        if (is_base_case(current)) break;
        
        cJSON* next = transform(current);
        cJSON_Delete(current);
        current = next;
    }
    
    cJSON_Delete(current);
    return count;
}
```

## Generating Generator Code

```prolog
?- compile_predicate_to_c(expand/2, [generator_mode(true)], Code).
```
