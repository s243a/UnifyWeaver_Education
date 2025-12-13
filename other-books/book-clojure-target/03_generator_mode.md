# Chapter 3: Generator Mode

Generator mode uses Clojure's `lazy-seq` and `cons` for lazy evaluation.

## Generated Code Structure

```clojure
(ns generator
  (:require [clojure.data.json :as json]))

(defn process [record]
  "Generator: returns lazy sequence of results."
  (lazy-seq
    (cons record nil)))

(defn process-all [records]
  "Flatten all generator results."
  (mapcat process records))

(defn run-pipeline []
  (doseq [result (process-all (read-jsonl))]
    (println (json/write-str result))))
```

## Lazy Sequences

Clojure sequences are lazy by default:

```clojure
;; lazy-seq + cons for building
(defn generate [n]
  (lazy-seq
    (when (pos? n)
      (cons n (generate (dec n))))))

;; Infinite sequences
(def naturals (iterate inc 1))
```

## Example: Expanding Records

```clojure
(defn process [record]
  (let [items (:items record)]
    (map (fn [item]
           (-> record
               (dissoc :items)
               (assoc :item item)))
         items)))
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

## Recursive Generators with loop/recur

Tail recursion uses `loop/recur`:

```clojure
(defn generate [record]
  (lazy-seq
    (loop [current record
           results []]
      (if (base-case? current)
        (cons current results)
        (recur (transform current)
               (cons current results))))))
```

## Generating Generator Code

```prolog
?- compile_predicate_to_clojure(expand/2, [generator_mode(true)], Code).
```
