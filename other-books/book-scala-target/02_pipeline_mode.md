# Chapter 2: Pipeline Mode

Pipeline mode generates Scala code using `Option[T]` and `flatMap` for filtering.

## Generated Code Structure

```scala
import scala.io.Source
import scala.util.parsing.json.JSON

object FilterPipeline {
  type Record = Map[String, Any]

  def process(record: Record): Option[Record] = {
    // Return Some(record) to keep
    // Return None to filter out
    Some(record)
  }

  def runPipeline(): Unit = {
    Source.stdin.getLines()
      .filter(_.nonEmpty)
      .flatMap(line => parseJson(line).flatMap(process))
      .foreach(result => println(toJson(result)))
  }

  def main(args: Array[String]): Unit = runPipeline()
}
```

## Option[T] for Filtering

| Prolog | Scala |
|--------|-------|
| Success | `Some(value)` |
| Failure | `None` |
| Filter | `flatMap` |

## Pattern Matching

Scala's pattern matching excels at JSON processing:

```scala
def process(record: Record): Option[Record] = {
  record.get("value") match {
    case Some(v: Double) if v > 50 => Some(record)
    case _ => None
  }
}
```

## Generating Pipeline Code

```prolog
?- compile_predicate_to_scala(filter/2, [pipeline_input(true)], Code).
```

## Running the Pipeline

```bash
# With SBT
sbt run < input.jsonl

# Direct
scala FilterPipeline.scala < input.jsonl
```

## Next Steps

- [Chapter 3: Generator Mode](03_generator_mode.md) - LazyList with #::
