# Chapter 2: Pipeline Mode

Pipeline mode generates Scala code using `Option[T]` and `flatMap` for filtering.

## Source Prolog

```prolog
% filter.pl - Define your filter predicate
:- module(filter, [filter/2]).

filter(Input, Output) :-
    get_field(Input, "value", Value),
    Value > 50,
    Output = Input.
```

## Generating Scala Code

```prolog
?- use_module('src/unifyweaver/targets/scala_target').
?- use_module('filter').

?- compile_predicate_to_scala(filter/2, [pipeline_input(true)], Code),
   write_to_file('FilterPipeline.scala', Code).
```

## Generated Scala Code

```scala
import scala.io.Source

object FilterPipeline {
  type Record = Map[String, Any]

  def process(record: Record): Option[Record] = {
    // Generated from: filter(Input, Output) :- get_field(Input, "value", Value), Value > 50, ...
    record.get("value") match {
      case Some(v: Double) if v > 50 => Some(record)
      case Some(v: Int) if v > 50 => Some(record)
      case _ => None
    }
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

## Running the Pipeline

```bash
echo '{"value": 75}' | scala FilterPipeline.scala
# Output: {"value": 75}
```

## Next Steps

- [Chapter 3: Generator Mode](03_generator_mode.md) - LazyList with #::
