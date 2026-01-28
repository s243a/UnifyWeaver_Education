<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Pipeline Mode - Questions

Q&A companion for [02_pipeline_mode_impl.md](./02_pipeline_mode_impl.md).

---

## Question Index

1. [What does compile_predicate_to_scala/3 do?](#bscala02-q-compile)
2. [What is the pipeline architecture?](#bscala02-q-architecture)
3. [How are filter predicates compiled?](#bscala02-q-filter)
4. [Why use Option[Record]?](#bscala02-q-option)
5. [How does pattern matching work?](#bscala02-q-match)
6. [What does flatMap do?](#bscala02-q-flatmap)
7. [What is the Record type alias?](#bscala02-q-type)
8. [How does the pipeline handle None?](#bscala02-q-none)
9. [How do I run the generated code?](#bscala02-q-run)
10. [How are guards used?](#bscala02-q-guards)

---

## Questions and Answers

### <a id="bscala02-q-compile"></a>Q1: What does compile_predicate_to_scala/3 do?

**Answer**: Compiles a Prolog predicate to Scala pipeline code:

```prolog
?- compile_predicate_to_scala(filter/2, [pipeline_input(true)], Code).
```

Generates object with `process()` returning `Option[Record]`.

**See**: [compile_predicate_to_scala/3](./02_pipeline_mode_impl.md#compile_predicate_to_scala3)

---

### <a id="bscala02-q-architecture"></a>Q2: What is the pipeline architecture?

**Answer**: Iterator with flatMap for filtering:

```
stdin.getLines() → filter → flatMap(parse) → flatMap(process) → foreach
```

**See**: [Overview: Pipeline Architecture](./02_pipeline_mode_impl.md#overview-pipeline-architecture)

---

### <a id="bscala02-q-filter"></a>Q3: How are filter predicates compiled?

**Answer**: Pattern matching with guards:

```scala
record.get("value") match {
  case Some(v: Double) if v > 50 => Some(record)
  case _ => None
}
```

**See**: [Filter Compilation](./02_pipeline_mode_impl.md#filter-compilation)

---

### <a id="bscala02-q-option"></a>Q4: Why use Option[Record]?

**Answer**: Models success/failure:

- `Some(record)` = predicate succeeded
- `None` = predicate failed (filter out)

**See**: [Option Monad](./02_pipeline_mode_impl.md#option-monad)

---

### <a id="bscala02-q-match"></a>Q5: How does pattern matching work?

**Answer**: Type-safe extraction with cases:

```scala
case Some(v: Double) if v > 50 => Some(record)
case Some(v: Int) if v > 50 => Some(record)
case _ => None
```

Handles multiple numeric types.

**See**: [Pattern Matching](./02_pipeline_mode_impl.md#pattern-matching)

---

### <a id="bscala02-q-flatmap"></a>Q6: What does flatMap do?

**Answer**: Maps and flattens Options:

```scala
parseJson(line).flatMap(process)
```

If `parseJson` returns `None`, `process` is never called.

**See**: [flatMap Chaining](./02_pipeline_mode_impl.md#flatmap-chaining)

---

### <a id="bscala02-q-type"></a>Q7: What is the Record type alias?

**Answer**: Type alias for cleaner signatures:

```scala
type Record = Map[String, Any]
```

**See**: [Type Alias](./02_pipeline_mode_impl.md#type-alias)

---

### <a id="bscala02-q-none"></a>Q8: How does the pipeline handle None?

**Answer**: `flatMap` filters out `None`:

```scala
.flatMap(line => parseJson(line).flatMap(process))
```

Only `Some` values pass through.

**See**: [Stream Operations](./02_pipeline_mode_impl.md#stream-operations)

---

### <a id="bscala02-q-run"></a>Q9: How do I run the generated code?

**Answer**:

```bash
echo '{"value": 75}' | scala FilterPipeline.scala
```

**See**: [Running](./02_pipeline_mode_impl.md#running)

---

### <a id="bscala02-q-guards"></a>Q10: How are guards used?

**Answer**: `if` clauses in pattern matching:

```scala
case Some(v: Double) if v > 50 => ...
```

Only matches when condition is true.

**See**: [Compilation Rules](./02_pipeline_mode_impl.md#compilation-rules)

---

## Summary

Scala pipeline mode provides:
- `Option[Record]` for success/failure
- Pattern matching with guards
- `flatMap` for monadic chaining
- Automatic None filtering
- Type aliases for clarity
