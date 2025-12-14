# Chapter 4: Recursive Queries

This chapter covers compiling recursive predicates to Scala.

## Compiling to Scala

```prolog
?- compile_recursive(ancestor/2, [target(scala)], Code).
```

## Generated Scala Code

```scala
package generated

import scala.collection.mutable
import scala.io.StdIn

object ANCESTORQuery {
  private val baseRelation = mutable.Map[String, mutable.Set[String]]()

  def addFact(from: String, to: String): Unit = {
    baseRelation.getOrElseUpdate(from, mutable.Set()) += to
  }

  def findAll(start: String): Set[String] = {
    val visited = mutable.Set[String]()
    val queue = mutable.Queue[String]()
    val results = mutable.LinkedHashSet[String]()

    queue.enqueue(start)
    visited += start

    while (queue.nonEmpty) {
      val current = queue.dequeue()
      baseRelation.getOrElse(current, Set()).foreach { next =>
        if (!visited.contains(next)) {
          visited += next
          queue.enqueue(next)
          results += next
        }
      }
    }
    results.toSet
  }
}
```

## Running

```bash
scalac AncestorQuery.scala
echo 'abraham:isaac' | scala generated.ANCESTORQuery abraham
```

## Scala-Specific Features

- `mutable.Map` with `getOrElseUpdate`
- `mutable.Queue` for BFS
- `mutable.LinkedHashSet` for ordered results
- Pattern matching on args
