# Chapter 2: Pipeline Mode

Pipeline mode generates Clojure code using `keep` for filtering.

## Generated Code Structure

```clojure
(ns filter-pipeline
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]))

(defn process [record]
  "Process a single record. Return record or nil to filter."
  ;; Your predicate logic here
  record)

(defn run-pipeline []
  (doseq [result (->> (line-seq (io/reader *in*))
                      (filter seq)
                      (map #(json/read-str % :key-fn keyword))
                      (keep process))]
    (println (json/write-str result))))

(defn -main [& args]
  (run-pipeline))
```

## Threading Macros

Clojure's threading macros make pipelines readable:

```clojure
;; ->> threads through last position
(->> input
     (filter pred?)
     (map transform)
     (keep process))
```

| Macro | Threading |
|-------|-----------|
| `->` | First position |
| `->>` | Last position |
| `as->` | Named position |

## Generating Pipeline Code

```prolog
?- compile_predicate_to_clojure(filter/2, [pipeline_input(true)], Code).
```

## Running the Pipeline

```bash
# With deps.edn
clojure -M -m filter-pipeline < input.jsonl

# Direct
clojure filter_pipeline.clj < input.jsonl
```

## Next Steps

- [Chapter 3: Generator Mode](03_generator_mode.md) - lazy-seq with cons
