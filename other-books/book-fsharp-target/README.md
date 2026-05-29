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

## Generating F# Hybrid WAM Code

Use the WAM F# target for WAM-backed F# generation:

```prolog
?- use_module('src/unifyweaver/targets/wam_fsharp_target').
?- write_wam_fsharp_project([ancestor/2],
       [emit_mode(auto), runtime_parser(off)],
       'out/wam_fsharp_ancestor').
```

Use `runtime_parser(compiled)` only for programs that need runtime Prolog term
parsing. Ordinary recursive predicates do not need to bundle the parser.

## Hybrid WAM Role

F# is useful for discussing compiled parser support, .NET-friendly WAM state,
and functional update tradeoffs. Book 17 covers the shared Hybrid WAM model;
this book should show the functional .NET view of those concepts.

## Quick Example

```prolog
?- use_module('src/unifyweaver/targets/fsharp_target').
?- init_fsharp_target.
?- compile_predicate_to_fsharp(person/2, [], Code).
```
