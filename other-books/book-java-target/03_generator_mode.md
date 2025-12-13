# Chapter 3: Generator Mode

Generator mode produces multiple output records for each input using Java's Stream API with `flatMap`.

## Source Prolog

Define a predicate that can produce multiple outputs:

```prolog
% expand.pl - Expand items list into separate records
:- module(expand, [expand/2]).

% Generator: each item becomes a separate record
expand(Input, Output) :-
    get_field(Input, "items", Items),
    member(Item, Items),  % Non-deterministic: generates multiple outputs
    set_field(Input, "item", Item, Temp),
    remove_field(Temp, "items", Output).
```

## Generating Java Code

```prolog
?- use_module('src/unifyweaver/targets/java_target').
?- use_module('expand').

% Generate generator Java code
?- compile_predicate_to_java(expand/2, [generator_mode(true)], Code),
   write_c_program(Code, 'ExpandGenerator.java').
```

## Generated Java Code

The above Prolog generates:

```java
import com.google.gson.Gson;
import java.io.*;
import java.util.*;
import java.util.stream.*;

public class ExpandGenerator {
    private static final Gson gson = new Gson();

    public static Stream<Map<String, Object>> process(Map<String, Object> record) {
        // Generated from: expand(Input, Output) :- get_field(Input, "items", Items), member(Item, Items), ...
        List<?> items = (List<?>) record.get("items");
        if (items == null) return Stream.empty();
        
        return items.stream().map(item -> {
            Map<String, Object> result = new HashMap<>(record);
            result.put("item", item);
            result.remove("items");
            return result;
        });
    }

    public static Stream<Map<String, Object>> processAll(Stream<Map<String, Object>> records) {
        return records.flatMap(ExpandGenerator::process);
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

## Running the Generator

```bash
# Input: one record with items list
echo '{"id": 1, "items": ["a", "b", "c"]}' | java -cp .:gson.jar ExpandGenerator

# Output: three separate records
{"id": 1, "item": "a"}
{"id": 1, "item": "b"}
{"id": 1, "item": "c"}
```

## More Prolog Examples

### Recursive countdown
```prolog
% Generate countdown sequence
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

Generated Java uses `Stream.iterate`:
```java
public static Stream<Map<String, Object>> process(Map<String, Object> record) {
    return Stream.iterate(record,
        current -> ((Number) current.get("n")).intValue() > 0,
        current -> {
            Map<String, Object> next = new HashMap<>(current);
            next.put("n", ((Number) current.get("n")).intValue() - 1);
            return next;
        });
}
```

### Multiple clause alternatives
```prolog
% Generate both uppercased and lowercased versions
cases(Input, Output) :-
    get_field(Input, "text", Text),
    upcase(Text, Upper),
    set_field(Input, "case", "upper", Temp),
    set_field(Temp, "text", Upper, Output).
cases(Input, Output) :-
    get_field(Input, "text", Text),
    downcase(Text, Lower),
    set_field(Input, "case", "lower", Temp),
    set_field(Temp, "text", Lower, Output).
```

## Use Cases

| Prolog Pattern | Java Translation |
|----------------|------------------|
| `member(X, List)` | `list.stream().map(...)` |
| Multiple clauses | `Stream.concat(...)` |
| Recursive calls | `Stream.iterate(...)` |
| Backtracking | `flatMap` composition |
