# Chapter 3: Generator Mode

Generator mode uses Scala's `LazyList` with the `#::` cons operator.

## Source Prolog

```prolog
% expand.pl - Expand items into separate records
:- module(expand, [expand/2]).

expand(Input, Output) :-
    get_field(Input, "items", Items),
    member(Item, Items),
    set_field(Input, "item", Item, Temp),
    remove_field(Temp, "items", Output).
```

## Generating Scala Code

```prolog
?- use_module('src/unifyweaver/targets/scala_target').
?- use_module('expand').

?- compile_predicate_to_scala(expand/2, [generator_mode(true)], Code),
   write_to_file('ExpandGenerator.scala', Code).
```

## Generated Scala Code

```scala
object ExpandGenerator {
  type Record = Map[String, Any]

  def process(record: Record): LazyList[Record] = {
    // Generated from: expand(Input, Output) :- member(Item, Items), ...
    record.get("items") match {
      case Some(items: List[_]) =>
        items.to(LazyList).map { item =>
          record - "items" + ("item" -> item)
        }
      case _ => LazyList.empty
    }
  }

  def processAll(records: Iterator[Record]): LazyList[Record] = {
    records.to(LazyList).flatMap(process)
  }
}
```

## Running the Generator

```bash
echo '{"id": 1, "items": ["a", "b", "c"]}' | scala ExpandGenerator.scala

# Output:
{"id": 1, "item": "a"}
{"id": 1, "item": "b"}
{"id": 1, "item": "c"}
```

## More Prolog Examples

### Recursive countdown
```prolog
countdown(Input, Output) :-
    get_field(Input, "n", N),
    N > 0,
    Output = Input.
countdown(Input, Output) :-
    get_field(Input, "n", N),
    N > 0,
    N1 is N - 1,
    set_field(Input, "n", N1, Next),
    countdown(Next, Output).
```

Generated Scala with @tailrec:
```scala
import scala.annotation.tailrec

def process(record: Record): LazyList[Record] = {
  @tailrec
  def loop(current: Record, acc: LazyList[Record]): LazyList[Record] = {
    val n = current.getOrElse("n", 0).asInstanceOf[Int]
    if (n <= 0) current #:: acc
    else loop(current + ("n" -> (n - 1)), current #:: acc)
  }
  loop(record, LazyList.empty)
}
```
