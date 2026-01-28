<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Pipeline Mode - Implementation Details

Technical deep-dive for Jython pipeline code generation with Java I/O integration.

## compile_predicate_to_jython/3

### Signature

```prolog
compile_predicate_to_jython(+PredicateSpec, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `PredicateSpec` | `Name/Arity` | Predicate to compile (e.g., `filter/2`) |
| `Options` | `list` | Generation options |
| `Code` | `string` | Generated Jython code |

### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `pipeline_input(Bool)` | `boolean` | `false` | Enable stdin JSONL processing |
| `java_compat(Bool)` | `boolean` | `true` | Use Java I/O classes |

### Return Value

Returns the complete Jython code as a string, ready for file output.

### Algorithm

1. **Shebang**: Add `#!/usr/bin/env jython`
2. **Future Import**: Add `from __future__ import print_function`
3. **Java Imports**: Import BufferedReader, InputStreamReader
4. **Process Function**: Compile predicate body to Python function
5. **Pipeline Runner**: Generate loop with Java I/O
6. **Main Guard**: Add `if __name__ == '__main__'`

### Generated Code Structure

```python
#!/usr/bin/env jython
from __future__ import print_function
import sys
import json
from java.io import BufferedReader, InputStreamReader
from java.lang import System as JavaSystem

def process(record):
    # Generated from: filter(Input, Output) :- ...
    value = record.get('value')
    if value is not None and value > 50:
        return record
    return None

def run_pipeline():
    reader = BufferedReader(InputStreamReader(JavaSystem.in))
    line = reader.readLine()
    while line is not None:
        if line.strip():
            record = json.loads(line)
            result = process(record)
            if result is not None:
                print(json.dumps(result))
        line = reader.readLine()

if __name__ == '__main__':
    run_pipeline()
```

## Jython vs CPython

### Key Differences

| Feature | Jython | CPython |
|---------|--------|---------|
| Runtime | JVM | Native |
| I/O | Java classes | Python built-ins |
| Libraries | Java + limited Python | Full Python ecosystem |
| stdin | `JavaSystem.in` | `sys.stdin` |

### Why Java I/O?

Jython's Python stdin may have encoding issues. Java I/O provides:
- Consistent encoding handling
- Better performance on JVM
- Direct access to Java streams

## Java I/O Integration

### BufferedReader Pattern

```python
from java.io import BufferedReader, InputStreamReader
from java.lang import System as JavaSystem

reader = BufferedReader(InputStreamReader(JavaSystem.in))
line = reader.readLine()
while line is not None:
    # Process line
    line = reader.readLine()
```

### Comparison with Python I/O

```python
# Pure Python (not recommended in Jython)
import sys
for line in sys.stdin:
    process(line)

# Jython with Java I/O (recommended)
reader = BufferedReader(InputStreamReader(JavaSystem.in))
while True:
    line = reader.readLine()
    if line is None:
        break
    process(line)
```

## Filter Compilation

### Prolog Source

```prolog
filter(Input, Output) :-
    get_field(Input, "value", Value),
    Value > 50,
    Output = Input.
```

### Compilation Rules

| Prolog Pattern | Jython Output |
|----------------|---------------|
| `get_field(R, F, V)` | `V = R.get('F')` |
| `V > N` | `V is not None and V > N` |
| `Output = Input` | `return record` |
| Failure | `return None` |

### Generated Process Function

```python
def process(record):
    # Generated from: filter(Input, Output) :- ...
    value = record.get('value')
    if value is not None and value > 50:
        return record
    return None
```

### Safe Field Access

The `.get()` method returns `None` for missing keys:

```python
value = record.get('value')      # None if missing
value = record.get('value', 0)   # 0 if missing
```

## Future Imports

### print_function

```python
from __future__ import print_function
```

Required for Python 2.x compatibility:
- Jython 2.7 uses Python 2.x syntax
- `print()` as function instead of statement
- Consistent with Python 3.x

## JSON Handling

### Standard json Module

```python
import json

# Parse
record = json.loads(line)

# Stringify
output = json.dumps(result)
```

The `json` module works identically in Jython and CPython.

## Running Generated Code

### Direct Execution

```bash
echo '{"value": 75}' | jython filter_pipeline.py
# Output: {"value": 75}
```

### With Classpath

```bash
CLASSPATH=deps/* jython filter_pipeline.py < input.jsonl > output.jsonl
```

### Jython Installation

```bash
# Install Jython
curl -O https://repo1.maven.org/maven2/org/python/jython-standalone/2.7.3/jython-standalone-2.7.3.jar

# Run
java -jar jython-standalone-2.7.3.jar filter_pipeline.py
```

## Performance Considerations

### JVM Warm-up

Jython has startup overhead:
- First run includes JVM startup
- Subsequent runs benefit from JIT compilation
- Consider `jython -jar` for better startup

### Memory

```bash
# Increase heap for large files
java -Xmx2g -jar jython-standalone.jar filter_pipeline.py
```

## Source Files

- `src/unifyweaver/targets/jython_target.pl`
