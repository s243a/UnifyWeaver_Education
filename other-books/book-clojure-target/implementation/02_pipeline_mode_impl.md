<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Pipeline Mode - Implementation Details

Technical deep-dive for Clojure pipeline code generation.

## compile_predicate_to_clojure/3

### Signature

```prolog
compile_predicate_to_clojure(+PredicateSpec, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `PredicateSpec` | `Name/Arity` | Predicate to compile (e.g., `filter/2`) |
| `Options` | `list` | Generation options |
| `Code` | `string` | Generated Clojure code |

### Options

| Option | Type | Default | Description |
|--------|------|---------|-------------|
| `pipeline_input(Bool)` | `boolean` | `false` | Enable stdin JSONL processing |
| `namespace(Name)` | `atom` | derived | Clojure namespace name |

### Return Value

Returns the complete Clojure code as a string, ready for file output.

### Algorithm

1. **Namespace Setup**: Generate `(ns ...)` form with required imports
2. **Process Function**: Compile predicate body to `when` expression
3. **Pipeline Runner**: Generate `run-pipeline` with threading macro
4. **Main Entry**: Add `-main` function for CLI execution

### Generated Code Structure

```clojure
(ns filter-pipeline
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]))

(defn process [record]
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

## Clojure Pipeline Idioms

### Threading Macro (->>)

The thread-last macro chains transformations:

```clojure
(->> (line-seq (io/reader *in*))  ; Read lines lazily
     (filter seq)                   ; Skip empty lines
     (map #(json/read-str % :key-fn keyword))  ; Parse JSON
     (keep process))                ; Filter with process fn
```

### keep vs filter

| Function | Behavior |
|----------|----------|
| `filter` | Keeps truthy values |
| `keep` | Keeps non-nil return values |

`keep` is preferred because it works with functions that return `nil` to filter:

```clojure
(defn process [record]
  (when (> (:value record) 50)  ; Returns nil if condition fails
    record))

(keep process records)  ; Only non-nil results included
```

### Keyword Access with Default

```clojure
(:value record 0)  ; Access :value, default to 0 if missing
```

This is safe for missing keys:

```clojure
(:missing-key {} 42)  ; Returns 42
```

## Filter Compilation

### Prolog Source

```prolog
filter(Input, Output) :-
    get_field(Input, "value", Value),
    Value > 50,
    Output = Input.
```

### Compilation Rules

| Prolog Pattern | Clojure Output |
|----------------|----------------|
| `get_field(R, F, V)` | `(:F R)` or `(get R :F)` |
| `V > N` | `(> V N)` |
| `Output = Input` | Return `record` |
| Failure | Return `nil` |

### Generated Process Function

```clojure
(defn process [record]
  ;; Generated from: filter(Input, Output) :- ...
  (when (> (:value record 0) 50)
    record))
```

## Lazy Evaluation

Clojure sequences are lazy by default:

```clojure
(line-seq reader)  ; Returns lazy sequence of lines
```

Benefits:
- Memory efficient for large files
- Only reads lines as needed
- Works with infinite streams

## JSON Handling

### clojure.data.json

```clojure
(:require [clojure.data.json :as json])

;; Parse with keyword keys
(json/read-str line :key-fn keyword)
;; {"value": 75} -> {:value 75}

;; Write JSON
(json/write-str {:value 75})
;; -> "{\"value\":75}"
```

### Key Conversion

| Option | Input | Output |
|--------|-------|--------|
| Default | `{"value": 1}` | `{"value" 1}` |
| `:key-fn keyword` | `{"value": 1}` | `{:value 1}` |

## Running Generated Code

### Direct Execution

```bash
echo '{"value": 75}' | clojure -M -m filter-pipeline
# Output: {"value":75}
```

### With deps.edn

```clojure
;; deps.edn
{:deps {org.clojure/data.json {:mvn/version "2.4.0"}}
 :paths ["src"]}
```

```bash
clojure -M -m filter-pipeline < input.jsonl > output.jsonl
```

## Source Files

- `src/unifyweaver/targets/clojure_target.pl`
