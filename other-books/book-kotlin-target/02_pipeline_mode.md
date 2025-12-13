# Chapter 2: Pipeline Mode

Pipeline mode generates Kotlin code using sequences and lambda expressions.

## Generated Code Structure

```kotlin
import com.google.gson.Gson
import java.io.BufferedReader
import java.io.InputStreamReader

object FilterPipeline {
    private val gson = Gson()
    private val mapType = object : TypeToken<MutableMap<String, Any?>>() {}.type

    fun process(record: MutableMap<String, Any?>): MutableMap<String, Any?>? {
        // Your predicate logic here
        // Return null to filter out
        return record
    }

    fun runPipeline() {
        BufferedReader(InputStreamReader(System.`in`))
            .lineSequence()
            .filter { it.isNotBlank() }
            .mapNotNull { line ->
                try {
                    val record: MutableMap<String, Any?> = gson.fromJson(line, mapType)
                    process(record)
                } catch (e: Exception) {
                    System.err.println("JSON parse error: ${e.message}")
                    null
                }
            }
            .forEach { result ->
                println(gson.toJson(result))
            }
    }
}

fun main() = FilterPipeline.runPipeline()
```

## Kotlin Features Used

| Feature | Purpose |
|---------|---------|
| `lineSequence()` | Lazy line reading |
| `mapNotNull` | Filter + transform |
| `?.` | Safe null access |
| `?:` | Elvis operator for defaults |

## Generating Pipeline Code

```prolog
?- compile_predicate_to_kotlin(filter/2, [pipeline_input(true)], Code).
```

## Running the Pipeline

```bash
# With Gradle
./gradlew run < input.jsonl

# Direct
kotlinc -include-runtime -d pipeline.jar FilterPipeline.kt
java -jar pipeline.jar < input.jsonl
```

## Next Steps

- [Chapter 3: Generator Mode](03_generator_mode.md) - sequence { yield() }
