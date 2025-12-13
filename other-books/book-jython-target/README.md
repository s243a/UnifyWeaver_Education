# Book: Jython Target

A guide to compiling Prolog predicates to Jython (Python on JVM) with Java interoperability.

## Status: 🚧 Initial

## Contents

1. [Introduction](01_introduction.md) - Jython target basics
2. [Pipeline Mode](02_pipeline_mode.md) - Python-style JSONL processing
3. [Generator Mode](03_generator_mode.md) - Python generators with yield

## Prerequisites

- Jython 2.7+
- Java Runtime Environment

## Quick Start

```prolog
?- use_module('src/unifyweaver/targets/jython_target').
?- compile_predicate_to_jython(filter/2, [pipeline_input(true)], Code).
```

## Key Features

- **Python generators** with `yield`
- **Java interop** - call Java from Python
- **Python 2.7** compatible syntax
- **Tail recursion** → while loop optimization
- **66 bindings** (Python + Java I/O, Collections)

## See Also

- [JVM_TARGET.md](../../../docs/JVM_TARGET.md) - JVM family documentation
