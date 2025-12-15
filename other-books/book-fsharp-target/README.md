# F# Target Education Book

This book covers compiling Prolog predicates to F# using UnifyWeaver's functional programming approach.

## Chapters

1. [Introduction](01_introduction.md) - Getting started with F# target
2. [Pipeline Mode](02_pipeline_mode.md) - Stream processing with Seq
3. [Recursive Queries](03_recursive_queries.md) - Tail, linear, and mutual recursion

## Prerequisites

- SWI-Prolog 8.0+
- .NET SDK 6.0+ (for running generated code)
- Basic F# knowledge

## F# Advantages

- **Immutable data** - Record types are immutable by default
- **Pattern matching** - Elegant base case handling
- **Pipeline operator** - Natural data flow: `|> Seq.map |> Seq.filter`
- **Mutual recursion** - Native `and` keyword support

## Quick Example

```prolog
?- use_module('src/unifyweaver/targets/fsharp_target').
?- init_fsharp_target.
?- compile_predicate_to_fsharp(person/2, [], Code).
```
