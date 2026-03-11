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
      baseRelation.getOrElse(current, mutable.Set.empty[String]).foreach { next =>
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

## Advanced Recursion Patterns

The Scala target also supports tail and linear recursion via multifile dispatch:

```prolog
?- compile_tail_recursion(test_sum/3, [target(scala)], Code).
?- compile_linear_recursion(factorial/2, [target(scala)], Code).
```

| Pattern | Multifile Predicate | Scala Idiom |
|---------|-------------------|-------------|
| Tail Recursion | `tail_recursion:compile_tail_pattern/9` | `@tailrec def loop(...)` with annotation |
| Linear Recursion | `linear_recursion:compile_linear_pattern/8` | `foldLeft` + `mutable.Map` memoization |

### Tail Recursion Example

```scala
import scala.annotation.tailrec

object Main {
  def test_sum(items: List[Int]): Int = {
    @tailrec
    def loop(remaining: List[Int], acc: Int): Int = remaining match {
      case Nil => acc
      case item :: rest => loop(rest, acc + item)
    }
    loop(items, 0)
  }
}
```

The `@tailrec` annotation ensures the Scala compiler verifies tail call optimization.
