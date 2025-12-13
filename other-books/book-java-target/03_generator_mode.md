# Chapter 3: Generator Mode

Generator mode produces multiple output records for each input using Java's Stream API with `flatMap`.

## Generated Code Structure

```java
public class Generator {
    private static final Gson gson = new Gson();

    public static Stream<Map<String, Object>> process(Map<String, Object> record) {
        // Return Stream.of() for single result
        // Return Stream.empty() to filter out
        // Return multiple values for generator behavior
        return Stream.of(record);
    }

    public static Stream<Map<String, Object>> processAll(Stream<Map<String, Object>> records) {
        return records.flatMap(Generator::process);
    }

    public static void runPipeline() {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        processAll(reader.lines()
            .filter(line -> !line.isEmpty())
            .map(line -> gson.fromJson(line, mapType)))
            .forEach(result -> System.out.println(gson.toJson(result)));
    }
}
```

## Use Cases

| Scenario | Return Value |
|----------|--------------|
| Keep record | `Stream.of(record)` |
| Filter out | `Stream.empty()` |
| Expand to multiple | `Stream.of(r1, r2, r3)` |
| Recursive generation | `Stream.iterate(...)` |

## Generating Generator Code

```prolog
?- compile_predicate_to_java(expand/2, [generator_mode(true)], Code).
```

## Example: Expanding Records

```java
public static Stream<Map<String, Object>> process(Map<String, Object> record) {
    List<String> items = (List<String>) record.get("items");
    return items.stream().map(item -> {
        Map<String, Object> result = new HashMap<>(record);
        result.put("item", item);
        result.remove("items");
        return result;
    });
}
```

Input:
```json
{"id": 1, "items": ["a", "b", "c"]}
```

Output:
```json
{"id": 1, "item": "a"}
{"id": 1, "item": "b"}
{"id": 1, "item": "c"}
```

## Recursive Generators

For recursive predicates, UnifyWeaver uses `Stream.iterate`:

```java
public static Stream<Map<String, Object>> process(Map<String, Object> record) {
    return Stream.iterate(record, 
        current -> !isBaseCase(current),
        current -> transform(current));
}
```
