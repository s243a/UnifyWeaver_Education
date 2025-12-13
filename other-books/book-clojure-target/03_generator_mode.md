# Chapter 3: Generator Mode

Generator mode uses Clojure's `lazy-seq` and `cons` for lazy evaluation.

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

## Generating Clojure Code

```prolog
?- use_module('src/unifyweaver/targets/clojure_target').
?- use_module('expand').

?- compile_predicate_to_clojure(expand/2, [generator_mode(true)], Code),
   write_to_file('expand_generator.clj', Code).
```

## Generated Clojure Code

```clojure
(ns expand-generator
  (:require [clojure.data.json :as json]))

(defn process [record]
  ;; Generated from: expand(Input, Output) :- member(Item, Items), ...
  (let [items (:items record)]
    (map (fn [item]
           (-> record
               (dissoc :items)
               (assoc :item item)))
         items)))

(defn process-all [records]
  (mapcat process records))

(defn run-pipeline []
  (doseq [result (process-all (read-jsonl))]
    (println (json/write-str result))))
```

## Running the Generator

```bash
echo '{"id": 1, "items": ["a", "b", "c"]}' | clojure expand_generator.clj

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

Generated Clojure with loop/recur:
```clojure
(defn process [record]
  (lazy-seq
    (loop [current record
           results []]
      (let [n (:n current 0)]
        (if (<= n 0)
          (cons current results)
          (recur (assoc current :n (dec n))
                 (cons current results)))))))
```
