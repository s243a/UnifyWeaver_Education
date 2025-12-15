# Chapter 1: Introduction to the F# Target

This chapter introduces the F# target for UnifyWeaver.

## Overview

The F# target compiles Prolog predicates to idiomatic F# code:
- Fact export to record types
- Stream processing with `Seq`
- Recursion with `let rec` and `and` keyword

## Loading the Target

```prolog
?- use_module('src/unifyweaver/targets/fsharp_target').
?- init_fsharp_target.
```

## Your First Compilation

Define facts in Prolog:
```prolog
person(john, 25).
person(jane, 30).
```

Compile to F#:
```prolog
?- compile_predicate_to_fsharp(person/2, [], Code),
   write_fsharp_program(Code, 'Person.fs').
```

## Generated Code Structure

```fsharp
type PERSON = {
    Arg1: string
    Arg2: string
}

let getAllPERSON () =
    [
        { Arg1 = "john"; Arg2 = "25" }
        { Arg1 = "jane"; Arg2 = "30" }
    ]

let streamPERSON () =
    getAllPERSON () |> Seq.ofList

let containsPERSON target =
    getAllPERSON () |> List.exists ((=) target)
```

## Functional Idioms

The generated F# uses idiomatic patterns:

| Pattern | Usage |
|---------|-------|
| `|>` | Pipeline data through transformations |
| `Seq.ofList` | Lazy evaluation for streaming |
| `List.exists` | Predicate-based searching |
| Record types | Immutable structured data |

## Running the Generated Code

```bash
dotnet new console -lang F# -o PersonApp
cp Person.fs PersonApp/
cd PersonApp
dotnet run
```

---

**→** [Next: Chapter 2: Pipeline Mode](02_pipeline_mode)
