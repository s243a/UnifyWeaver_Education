# Chapter 2: Pipeline Mode

Pipeline mode generates Jython code using Python-style loops with Java I/O integration.

## Source Prolog

```prolog
% filter.pl - Define your filter predicate
:- module(filter, [filter/2]).

filter(Input, Output) :-
    get_field(Input, "value", Value),
    Value > 50,
    Output = Input.
```

## Generating Jython Code

```prolog
?- use_module('src/unifyweaver/targets/jython_target').
?- use_module('filter').

?- compile_predicate_to_jython(filter/2, [pipeline_input(true)], Code),
   write_to_file('filter_pipeline.py', Code).
```

## Generated Jython Code

```python
#!/usr/bin/env jython
from __future__ import print_function
import sys
import json
from java.io import BufferedReader, InputStreamReader
from java.lang import System as JavaSystem

def process(record):
    # Generated from: filter(Input, Output) :- get_field(Input, "value", Value), Value > 50, ...
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

## Running the Pipeline

```bash
echo '{"value": 75}' | jython filter_pipeline.py
# Output: {"value": 75}
```

## Next Steps

- [Chapter 3: Generator Mode](03_generator_mode.md) - Python generators with yield
