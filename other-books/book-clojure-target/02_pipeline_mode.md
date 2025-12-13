# Chapter 2: Pipeline Mode

Pipeline mode generates Clojure code using `keep` for filtering.

## Source Prolog

```prolog
% filter.pl - Define your filter predicate
:- module(filter, [filter/2]).

filter(Input, Output) :-
    get_field(Input, "value", Value),
    Value > 50,
    Output = Input.
```

## Generating Clojure Code

```prolog
?- use_module('src/unifyweaver/targets/clojure_target').
?- use_module('filter').

?- compile_predicate_to_clojure(filter/2, [pipeline_input(true)], Code),
   write_to_file('filter_pipeline.clj', Code).
```

## Generated Clojure Code

```clojure
(ns filter-pipeline
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]))

(defn process [record]
  ;; Generated from: filter(Input, Output) :- get_field(Input, "value", Value), Value > 50, ...
  (when (> (:value record 0) 50)
    record))

(defn run-pipeline []
  (doseq [result (->> (line-seq (io/reader *in*))
                      (filter seq)
                      (map #(json/read-str % :key-fn keyword))
                      (keep process))]
    (println (json/write-str result))))

(defn -main [& args]
  (run-pipeline))
```

## Running the Pipeline

```bash
echo '{"value": 75}' | clojure -M -m filter-pipeline
# Output: {"value":75}
```

## Next Steps

- [Chapter 3: Generator Mode](03_generator_mode.md) - lazy-seq with cons
