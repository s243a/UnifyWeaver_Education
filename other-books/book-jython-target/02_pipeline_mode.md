# Chapter 2: Pipeline Mode

Pipeline mode generates Jython code using Python-style loops with Java I/O integration.

## Generated Code Structure

```python
#!/usr/bin/env jython
from __future__ import print_function
import sys
import json
from java.io import BufferedReader, InputStreamReader
from java.lang import System as JavaSystem

def process(record):
    """Process a single record. Return record or None to filter."""
    # Your predicate logic here
    return record

def run_pipeline():
    reader = BufferedReader(InputStreamReader(JavaSystem.in))
    line = reader.readLine()
    while line is not None:
        if line.strip():
            try:
                record = json.loads(line)
                result = process(record)
                if result is not None:
                    print(json.dumps(result))
            except ValueError as e:
                print >> sys.stderr, 'JSON parse error:', e
        line = reader.readLine()

if __name__ == '__main__':
    run_pipeline()
```

## Generating Pipeline Code

```prolog
% Define your predicate
filter(Input, Output) :-
    Input = record(Name, Value),
    Value > 50,
    Output = record(Name, Value).

% Generate Jython code
?- compile_predicate_to_jython(filter/2, [pipeline_input(true)], Code).
```

## Running the Pipeline

```bash
echo '{"name": "alice", "value": 75}' | jython filter_pipeline.py
# Output: {"name": "alice", "value": 75}
```

## Java Interoperability

Jython can call Java directly:

```python
from java.util import ArrayList, HashMap

def process(record):
    # Use Java collections
    jmap = HashMap()
    jmap.put("name", record["name"])
    return dict(jmap)
```

## Next Steps

- [Chapter 3: Generator Mode](03_generator_mode.md) - Python generators with yield
