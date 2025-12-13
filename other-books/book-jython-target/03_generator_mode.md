# Chapter 3: Generator Mode

Generator mode uses Python's native `yield` keyword for lazy evaluation.

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

## Generating Jython Code

```prolog
?- use_module('src/unifyweaver/targets/jython_target').
?- use_module('expand').

?- compile_predicate_to_jython(expand/2, [generator_mode(true)], Code),
   write_to_file('expand_generator.py', Code).
```

## Generated Jython Code

```python
#!/usr/bin/env jython
from __future__ import print_function
import json

def process(record):
    # Generated from: expand(Input, Output) :- member(Item, Items), ...
    items = record.get('items', [])
    for item in items:
        result = dict(record)
        result['item'] = item
        del result['items']
        yield result

def process_all(records):
    for record in records:
        for result in process(record):
            yield result

def run_pipeline():
    for result in process_all(read_jsonl()):
        print(json.dumps(result))
```

## Running the Generator

```bash
echo '{"id": 1, "items": ["a", "b", "c"]}' | jython expand_generator.py

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

Generated Jython:
```python
def process(record):
    current = dict(record)
    for _ in xrange(10000):
        yield dict(current)
        n = current.get('n', 0)
        if n <= 0:
            return
        current['n'] = n - 1
```
