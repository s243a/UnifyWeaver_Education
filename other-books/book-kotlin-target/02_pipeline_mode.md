# Chapter 2: Pipeline Mode

Pipeline mode generates Kotlin code using sequences and lambda expressions.

## Source Prolog

```prolog
% filter.pl - Define your filter predicate
:- module(filter, [filter/2]).

% Keep records where value > 50
filter(Input, Output) :-
    get_field(Input, "value", Value),
    Value > 50,
    Output = Input.
```

## Generating Kotlin Code

```prolog
?- use_module('src/unifyweaver/targets/kotlin_target').
?- use_module('filter').

% Generate pipeline Kotlin code
?- compile_predicate_to_kotlin(filter/2, [pipeline_input(true)], Code),
   write_to_file('FilterPipeline.kt', Code).
```

## Generated Kotlin Code

```kotlin
import com.google.gson.Gson
import java.io.BufferedReader
import java.io.InputStreamReader

object FilterPipeline {
    private val gson = Gson()

    fun process(record: MutableMap<String, Any?>): MutableMap<String, Any?>? {
        // Generated from: filter(Input, Output) :- get_field(Input, "value", Value), Value > 50, Output = Input.
        val value = record["value"] as? Number
        return if (value != null && value.toDouble() > 50) record else null
    }

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
}

fun main() = FilterPipeline.runPipeline()
```

## Running the Pipeline

```bash
# Compile and run with Gradle
./gradlew run < input.jsonl

# Or direct
echo '{"value": 75}' | kotlin FilterPipeline.kt
```

## Next Steps

- [Chapter 3: Generator Mode](03_generator_mode.md) - sequence { yield() }
