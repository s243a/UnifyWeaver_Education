# Chapter 3: Generator Mode

Generator mode uses Kotlin's `sequence { yield() }` for lazy generation.

## Source Prolog

```prolog
% expand.pl - Expand items into separate records
:- module(expand, [expand/2]).

expand(Input, Output) :-
    get_field(Input, "items", Items),
    member(Item, Items),
    set_field(Input, "item", Item, Temp),
    remove_field(Temp, "items", Output).
```

## Generating Kotlin Code

```prolog
?- use_module('src/unifyweaver/targets/kotlin_target').
?- use_module('expand').

?- compile_predicate_to_kotlin(expand/2, [generator_mode(true)], Code),
   write_to_file('ExpandGenerator.kt', Code).
```

## Generated Kotlin Code

```kotlin
object ExpandGenerator {
    fun process(record: MutableMap<String, Any?>): Sequence<MutableMap<String, Any?>> = sequence {
        // Generated from: expand(Input, Output) :- member(Item, Items), ...
        val items = record["items"] as? List<*> ?: emptyList<Any>()
        for (item in items) {
            val result = record.toMutableMap()
            result["item"] = item
            result.remove("items")
            yield(result)
        }
    }

    fun processAll(records: Sequence<MutableMap<String, Any?>>): Sequence<MutableMap<String, Any?>> {
        return records.flatMap { process(it) }
    }
}
```

## Running the Generator

```bash
echo '{"id": 1, "items": ["a", "b", "c"]}' | kotlin ExpandGenerator.kt

# Output:
{"id": 1, "item": "a"}
{"id": 1, "item": "b"}
{"id": 1, "item": "c"}
```

## More Prolog Examples

### Recursive countdown
```prolog
countdown(Input, Output) :-
    get_field(Input, "n", N),
    N > 0,
    Output = Input.
countdown(Input, Output) :-
    get_field(Input, "n", N),
    N > 0,
    N1 is N - 1,
    set_field(Input, "n", N1, Next),
    countdown(Next, Output).
```

Generated Kotlin:
```kotlin
fun process(record: MutableMap<String, Any?>): Sequence<MutableMap<String, Any?>> = sequence {
    var current = record.toMutableMap()
    repeat(10000) {
        yield(current.toMutableMap())
        val n = (current["n"] as Number).toInt()
        if (n <= 0) return@sequence
        current["n"] = n - 1
    }
}
```
