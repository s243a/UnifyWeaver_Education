# Chapter 1: Introduction to the VB.NET Target

This chapter introduces the VB.NET target for UnifyWeaver.

## Overview

The VB.NET target compiles Prolog predicates to VB.NET programs, enabling:
- Fact export to typed classes
- Stream processing with .NET collections
- Recursion optimization with Do While loops

## Loading the Target

```prolog
?- use_module('src/unifyweaver/targets/vbnet_target').
?- init_vbnet_target.
```

## Your First Compilation

Define facts in Prolog:
```prolog
person(john, 25).
person(jane, 30).
```

Compile to VB.NET:
```prolog
?- compile_predicate_to_vbnet(person/2, [], Code),
   write_vbnet_program(Code, 'Person.vb').
```

## Generated Code Structure

```vb
Public Class PERSON
    Public Property Arg1 As String
    Public Property Arg2 As String
End Class

Module Facts
    Public Function GetAllPERSON() As List(Of PERSON)
        Return New List(Of PERSON) From { ... }
    End Function
    
    Public Function StreamPERSON() As IEnumerable(Of PERSON)
        Return GetAllPERSON()
    End Function
End Module
```

## Running the Generated Code

```bash
dotnet new console -lang VB -o PersonApp
cp Person.vb PersonApp/
cd PersonApp
dotnet run
```

---

**→** [Next: Chapter 2: Pipeline Mode](02_pipeline_mode)
