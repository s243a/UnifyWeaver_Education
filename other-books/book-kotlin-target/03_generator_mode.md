# Chapter 3: Generator Mode

Generator mode uses Kotlin's `sequence { yield() }` for lazy generation.

## Generated Code Structure

```kotlin
object Generator {
    fun process(record: MutableMap<String, Any?>): Sequence<MutableMap<String, Any?>> = sequence {
        // Yield multiple results
        yield(record)
    }

    fun processAll(records: Sequence<MutableMap<String, Any?>>): Sequence<MutableMap<String, Any?>> {
        return records.flatMap { process(it) }
    }

    fun runPipeline() {
        processAll(readJsonl())
            .forEach { result ->
                println(gson.toJson(result))
            }
    }
}
```

## Kotlin Sequences

Sequences are lazy like Prolog's backtracking:

```kotlin
sequence {
    yield(value1)    // First solution
    yield(value2)    // Second solution
    yieldAll(more)   // All solutions from another sequence
}
```

## Example: Expanding Lists

```kotlin
fun process(record: MutableMap<String, Any?>): Sequence<MutableMap<String, Any?>> = sequence {
    val items = record["items"] as? List<*> ?: emptyList<Any>()
    for (item in items) {
        val result = record.toMutableMap()
        result["item"] = item
        result.remove("items")
        yield(result)
    }
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

```kotlin
fun process(record: MutableMap<String, Any?>): Sequence<MutableMap<String, Any?>> = sequence {
    var current = record.toMutableMap()
    repeat(10000) {
        yield(current.toMutableMap())
        if (isBaseCase(current)) return@sequence
        current = transform(current)
    }
}
```

## Generating Generator Code

```prolog
?- compile_predicate_to_kotlin(expand/2, [generator_mode(true)], Code).
```
