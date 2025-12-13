# Chapter 2: Pipeline Mode

Pipeline mode generates Java code that reads JSONL from stdin, processes each record, and writes results to stdout.

## Generated Code Structure

```java
public class FilterPipeline {
    private static final Gson gson = new Gson();
    private static final Type mapType = new TypeToken<Map<String, Object>>(){}.getType();

    public static Map<String, Object> process(Map<String, Object> record) {
        // Your predicate logic here
        return record; // or null to filter out
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

## Generating Pipeline Code

```prolog
% Define your predicate
filter(Input, Output) :-
    Input = record(Name, Value),
    Value > 50,
    Output = record(Name, Value).

% Generate Java code
?- compile_predicate_to_java(filter/2, [pipeline_input(true)], Code).
```

## Running the Pipeline

```bash
# Compile
javac -cp gson.jar FilterPipeline.java

# Run
echo '{"name": "alice", "value": 75}' | java -cp .:gson.jar FilterPipeline
# Output: {"name":"alice","value":75}

echo '{"name": "bob", "value": 25}' | java -cp .:gson.jar FilterPipeline
# (no output - filtered out)
```

## Gradle Integration

UnifyWeaver can generate a `build.gradle`:

```groovy
plugins {
    id 'java'
    id 'application'
}

dependencies {
    implementation 'com.google.code.gson:gson:2.10.1'
}

application {
    mainClass = 'FilterPipeline'
}
```

## Next Steps

- [Chapter 3: Generator Mode](03_generator_mode.md) - Multiple outputs per input
