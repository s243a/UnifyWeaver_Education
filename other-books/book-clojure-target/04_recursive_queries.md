# Chapter 4: Recursive Queries

This chapter covers compiling recursive predicates to Clojure.

## Compiling to Clojure

```prolog
?- compile_recursive(ancestor/2, [target(clojure)], Code).
```

## Generated Clojure Code

```clojure
(ns generated.ancestor-query)

(def base-relation (atom {}))

(defn add-fact [from to]
  (swap! base-relation update from (fnil conj #{}) to))

(defn find-all [start]
  (loop [visited #{start}
         queue [start]
         results []]
    (if (empty? queue)
      results
      (let [current (first queue)
            nexts (get @base-relation current #{})]
        (recur
          (into visited nexts)
          (into (rest queue) (remove visited nexts))
          (into results (remove visited nexts)))))))

(defn check-path [start target]
  (loop [visited #{start}
         queue [start]]
    (cond
      (empty? queue) false
      :else
      (let [current (first queue)
            nexts (get @base-relation current #{})]
        (if (contains? nexts target)
          true
          (recur
            (into visited nexts)
            (into (rest queue) (remove visited nexts))))))))
```

## Running

```bash
clojure -M -m generated.ancestor-query abraham < facts.txt
```

## Clojure-Specific Features

- `atom` for mutable state
- `loop/recur` for tail-recursive BFS
- `fnil` for nil-safe update
- `into/remove` for set operations

## Advanced Recursion Patterns

The Clojure target also supports tail and linear recursion via multifile dispatch:

```prolog
?- compile_tail_recursion(test_sum/3, [target(clojure)], Code).
?- compile_linear_recursion(factorial/2, [target(clojure)], Code).
```

| Pattern | Multifile Predicate | Clojure Idiom |
|---------|-------------------|---------------|
| Tail Recursion | `tail_recursion:compile_tail_pattern/9` | `(loop [...] (recur ...))` |
| Linear Recursion | `linear_recursion:compile_linear_pattern/8` | `(reduce ...)` + `(atom {})` with `swap!` |

### Tail Recursion Example

```clojure
(defn test_sum [items]
  (loop [remaining items
         acc 0]
    (if (empty? remaining)
      acc
      (let [item (first remaining)]
        (recur (rest remaining) (+ acc item))))))
```

Clojure requires explicit `recur` for tail-call optimization — the compiler verifies recur is in tail position.
