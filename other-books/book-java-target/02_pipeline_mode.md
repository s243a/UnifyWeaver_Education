# Chapter 2: Pipeline Mode

Pipeline mode generates Java code that reads JSONL from stdin, processes each record, and writes results to stdout.

## Source Prolog

First, define your predicate in Prolog:

```prolog
% facts.pl - Define your filter predicate
:- module(facts, [filter/2]).

% Keep records where value > 50
filter(Input, Output) :-
    get_field(Input, "value", Value),
    Value > 50,
    Output = Input.
```

## Generating Java Code

```prolog
?- use_module('src/unifyweaver/targets/java_target').
?- use_module('facts').

% Generate pipeline Java code
?- compile_predicate_to_java(filter/2, [pipeline_input(true)], Code),
   write_c_program(Code, 'FilterPipeline.java').
```

## Generated Java Code

The above Prolog generates:

```java
import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;
import java.io.*;
import java.lang.reflect.Type;
import java.util.Map;
import java.util.Objects;

public class FilterPipeline {
    private static final Gson gson = new Gson();
    private static final Type mapType = new TypeToken<Map<String, Object>>(){}.getType();

    public static Map<String, Object> process(Map<String, Object> record) {
        // Generated from: filter(Input, Output) :- get_field(Input, "value", Value), Value > 50, Output = Input.
        Object value = record.get("value");
        if (value instanceof Number && ((Number) value).doubleValue() > 50) {
            return record;
        }
        return null;  // Filter out
    }

    public static void runPipeline() {
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        reader.lines()
            .filter(line -> !line.isEmpty())
            .map(line -> gson.fromJson(line, mapType))
            .map(FilterPipeline::process)
            .filter(Objects::nonNull)
            .forEach(result -> System.out.println(gson.toJson(result)));
    }

    public static void main(String[] args) {
        runPipeline();
    }
}
```

## Running the Pipeline

```bash
# Compile
javac -cp gson.jar FilterPipeline.java

# Run - value 75 passes the filter
echo '{"name": "alice", "value": 75}' | java -cp .:gson.jar FilterPipeline
# Output: {"name":"alice","value":75}

# value 25 is filtered out
echo '{"name": "bob", "value": 25}' | java -cp .:gson.jar FilterPipeline
# (no output)
```

## More Examples

### Transform predicate
```prolog
% Double the value field
transform(Input, Output) :-
    get_field(Input, "value", V),
    NewValue is V * 2,
    set_field(Input, "doubled", NewValue, Output).
```

### Filter with multiple conditions
```prolog
% Keep active users over 18
eligible(Input, Output) :-
    get_field(Input, "age", Age),
    get_field(Input, "active", true),
    Age >= 18,
    Output = Input.
```

## Next Steps

- [Chapter 3: Generator Mode](03_generator_mode.md) - Multiple outputs per input
