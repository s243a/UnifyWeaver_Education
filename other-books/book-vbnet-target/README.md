# VB.NET Target Education Book

This book covers compiling Prolog predicates to VB.NET using UnifyWeaver.

## Chapters

1. [Introduction](01_introduction.md) - Getting started with VB.NET target
2. [Pipeline Mode](02_pipeline_mode.md) - Stream processing
3. [Recursive Queries](03_recursive_queries.md) - Tail, linear, and mutual recursion

## Prerequisites

- SWI-Prolog 8.0+
- .NET SDK 6.0+ (for running generated code)
- Basic VB.NET knowledge

## Quick Example

```prolog
?- use_module('src/unifyweaver/targets/vbnet_target').
?- init_vbnet_target.
?- compile_predicate_to_vbnet(person/2, [], Code).
```
