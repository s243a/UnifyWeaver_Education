# Chapter 4: Recursive Queries

This chapter covers compiling recursive predicates like `ancestor/2` to Kotlin.

## The Ancestor Problem

The classic transitive closure example works in Kotlin:

```prolog
% family_tree.pl
parent(abraham, isaac).
parent(isaac, jacob).

ancestor(A, D) :- parent(A, D).
ancestor(A, D) :- parent(A, P), ancestor(P, D).
```

## Compiling to Kotlin

```prolog
?- ['education/book-02-bash-target/examples/family_tree'].
?- use_module('src/unifyweaver/core/recursive_compiler').

?- compile_recursive(ancestor/2, [target(kotlin)], Code),
   open('AncestorQuery.kt', write, S),
   write(S, Code),
   close(S).
```

## Generated Kotlin Code

```kotlin
package generated

object ANCESTORQuery {
    private val baseRelation = mutableMapOf<String, MutableSet<String>>()

    fun addFact(from: String, to: String) {
        baseRelation.getOrPut(from) { mutableSetOf() }.add(to)
    }

    fun findAll(start: String): Set<String> {
        val visited = mutableSetOf<String>()
        val queue = ArrayDeque<String>()
        val results = linkedSetOf<String>()

        queue.add(start)
        visited.add(start)

        while (queue.isNotEmpty()) {
            val current = queue.removeFirst()
            baseRelation[current]?.forEach { next ->
                if (next !in visited) {
                    visited.add(next)
                    queue.add(next)
                    results.add(next)
                }
            }
        }
        return results
    }

    fun check(start: String, target: String): Boolean { /* BFS check */ }
}

fun main(args: Array<String>) {
    generateSequence(::readLine).filter { it.isNotBlank() }.forEach { line ->
        val parts = line.split(":")
        if (parts.size >= 2) ANCESTORQuery.addFact(parts[0].trim(), parts[1].trim())
    }
    // Handle args...
}
```

## Running the Kotlin Code

```bash
# Compile
kotlinc AncestorQuery.kt -include-runtime -d ancestor.jar

# Run with facts from stdin
cat << 'EOF' | java -jar ancestor.jar abraham
abraham:isaac
isaac:jacob
EOF

# Output:
abraham:isaac
abraham:jacob
```

## Kotlin-Specific Features

- `mutableMapOf`/`mutableSetOf` for efficient mutation
- `ArrayDeque` for BFS queue
- `linkedSetOf` preserves insertion order
- `?.forEach` safe navigation for null maps
- `generateSequence` for stdin processing
