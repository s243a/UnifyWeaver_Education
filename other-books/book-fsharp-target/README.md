# F# Target Education Book

This book covers compiling Prolog predicates to F# using UnifyWeaver. F# is supported by **two distinct compilation variants**:

- The **native F# target** (`fsharp_target`) lowers predicates to idiomatic F# — records, `Seq` pipelines, `let rec`/`and` recursion.
- The **F# WAM target** (`fsharp_wam`) hosts UnifyWeaver's symbolic WAM instruction set on F# functions, with kernel-template support for disk-resident graph data via LightningDB.

The two variants are not mutually exclusive: a single program can compile some predicates with the native target and others with the WAM-hosted target, mixing them in the same project. See chapter 4 for when each is the right pick.

## Chapters

1. [Introduction](01_introduction.md) - Getting started with the native F# target
2. *Pipeline Mode (planned)* - Stream processing with `Seq`
3. [Recursive Queries](03_recursive_queries.md) - Tail, linear, and mutual recursion in the native target
4. [The F# WAM Target](04_fsharp_wam_target.md) - When to use the WAM-hosted variant, LightningDB integration, the bidirectional ancestor kernel

## Prerequisites

- SWI-Prolog 8.0+
- .NET SDK 6.0+ (for running generated code; .NET 9 for the kernel-template examples in chapter 4)
- Basic F# knowledge

## F# Advantages

- **Immutable data** - Record types are immutable by default
- **Pattern matching** - Elegant base case handling
- **Pipeline operator** - Natural data flow: `|> Seq.map |> Seq.filter`
- **Mutual recursion** - Native `and` keyword support

## Hybrid WAM Role

F# is a useful lens on the shared Hybrid WAM model: compiled parser support,
.NET-friendly WAM state, and functional update tradeoffs. Book 17 covers the
model itself; chapter 4 of this book shows the functional .NET view of those
concepts through the `fsharp_wam` variant.

## Quick Example (native target)

```prolog
?- use_module('src/unifyweaver/targets/fsharp_target').
?- init_fsharp_target.
?- compile_predicate_to_fsharp(person/2, [], Code).
```

## Quick Example (WAM target)

```prolog
?- use_module('src/unifyweaver/targets/wam_fsharp_target').
?- write_wam_fsharp_project([ancestor/2],
       [emit_mode(auto), runtime_parser(off)],
       'out/wam_fsharp_ancestor').
```

Use `runtime_parser(compiled)` only for programs that need runtime Prolog term
parsing. Ordinary recursive predicates do not need to bundle the parser.

## Related Books

- [Book 17: WAM Target](../../book-17-wam-target/) - the symbolic WAM instruction set the `fsharp_wam` variant hosts.
- [book-18-graph-algorithms](../../book-18-graph-algorithms/) - graph-algorithm content using F# (both variants) as the example language. The bidirectional kernel from chapter 4 of *this* book is the worked example throughout book-18.
