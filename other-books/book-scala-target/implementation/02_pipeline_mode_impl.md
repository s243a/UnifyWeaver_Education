<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Pipeline Mode - Implementation Details

This document provides function-level documentation for Scala pipeline mode compilation.

**Source**: `src/unifyweaver/targets/scala_target.pl`

---

## Overview: Pipeline Architecture

```scala
Source.stdin.getLines() → filter → flatMap(parse) → flatMap(process) → foreach
```

Uses `Option[T]` and `flatMap` for filtering.

---

## compile_predicate_to_scala/3

Compiles a Prolog predicate to Scala code.

### Signature

```prolog
compile_predicate_to_scala(+Predicate/Arity, +Options, -Code)
```

### Options

| Option | Description |
|--------|-------------|
| `pipeline_input(true)` | Generate stdin reader |

### Example

```prolog
?- compile_predicate_to_scala(filter/2, [pipeline_input(true)], Code).
```

---

## Generated Code Structure

### Object Declaration

```scala
import scala.io.Source

object FilterPipeline {
  type Record = Map[String, Any]

  def process(record: Record): Option[Record] = {
    // Compiled predicate
  }

  def runPipeline(): Unit = {
    // Stream processing
  }

  def main(args: Array[String]): Unit = runPipeline()
}
```

---

## Filter Compilation

### Prolog Pattern

```prolog
filter(Input, Output) :-
    get_field(Input, "value", Value),
    Value > 50,
    Output = Input.
```

### Generated Scala

```scala
def process(record: Record): Option[Record] = {
  record.get("value") match {
    case Some(v: Double) if v > 50 => Some(record)
    case Some(v: Int) if v > 50 => Some(record)
    case _ => None
  }
}
```

### Compilation Rules

| Prolog | Scala |
|--------|-------|
| `get_field(R, "key", V)` | `record.get("key")` returns `Option` |
| `V > N` | Guard clause `if v > N` |
| `Output = Input` | `Some(record)` |
| Failure | `None` |

---

## Pattern Matching

### Type-Safe Extraction

```scala
record.get("value") match {
  case Some(v: Double) if v > 50 => Some(record)
  case Some(v: Int) if v > 50 => Some(record)
  case _ => None
}
```

Handles both `Double` and `Int` JSON values.

---

## Pipeline Runner

### Generated runPipeline()

```scala
def runPipeline(): Unit = {
  Source.stdin.getLines()
    .filter(_.nonEmpty)
    .flatMap(line => parseJson(line).flatMap(process))
    .foreach(result => println(toJson(result)))
}
```

### Stream Operations

| Operation | Purpose |
|-----------|---------|
| `Source.stdin.getLines()` | Iterator over stdin |
| `.filter(_.nonEmpty)` | Skip empty lines |
| `.flatMap(parse.flatMap(process))` | Parse and process |
| `.foreach(...)` | Output results |

---

## Option Monad

### Why Option[Record]?

- `Some(record)` = predicate succeeded
- `None` = predicate failed (filter out)

### flatMap Chaining

```scala
parseJson(line).flatMap(process)
```

If `parseJson` returns `None`, `process` is never called.

---

## Type Alias

```scala
type Record = Map[String, Any]
```

Makes signatures cleaner and enables change in one place.

---

## Running

```bash
echo '{"value": 75}' | scala FilterPipeline.scala
# Output: {"value": 75}
```

---

## Related Documentation

- [Book Scala Chapter 3: Generator Mode](../03_generator_mode.md)
- [Scala Target Source](../../../../../src/unifyweaver/targets/scala_target.pl)
