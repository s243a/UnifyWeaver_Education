# Chapter 3: Generator Mode

Generator mode uses Scala's `LazyList` with the `#::` cons operator.

## Generated Code Structure

```scala
object Generator {
  type Record = Map[String, Any]

  def process(record: Record): LazyList[Record] = {
    // Return LazyList for multiple results
    record #:: LazyList.empty
  }

  def processAll(records: Iterator[Record]): LazyList[Record] = {
    records.to(LazyList).flatMap(process)
  }

  def runPipeline(): Unit = {
    processAll(readJsonl())
      .foreach(result => println(toJson(result)))
  }
}
```

## LazyList

`LazyList` provides lazy, potentially infinite sequences:

```scala
// Finite
val finite = 1 #:: 2 #:: 3 #:: LazyList.empty

// Infinite (evaluated lazily)
val infinite = LazyList.from(1)
```

## Example: Expanding Records

```scala
def process(record: Record): LazyList[Record] = {
  record.get("items") match {
    case Some(items: List[_]) =>
      items.to(LazyList).map { item =>
        record - "items" + ("item" -> item)
      }
    case _ => LazyList.empty
  }
}
```

Input:
```json
{"id": 1, "items": ["a", "b", "c"]}
```

Output:
```json
{"id": 1, "item": "a"}
{"id": 1, "item": "b"}
{"id": 1, "item": "c"}
```

## Recursive Generators with @tailrec

For tail-recursive predicates, Scala uses `@tailrec`:

```scala
@tailrec
def generate(current: Record, acc: LazyList[Record]): LazyList[Record] = {
  if (isBaseCase(current)) current #:: acc
  else generate(transform(current), current #:: acc)
}
```

## Generating Generator Code

```prolog
?- compile_predicate_to_scala(expand/2, [generator_mode(true)], Code).
```
