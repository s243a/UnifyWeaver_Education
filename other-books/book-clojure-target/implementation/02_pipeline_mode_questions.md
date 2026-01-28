<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Pipeline Mode - Questions

Q&A companion for Clojure pipeline code generation.

## Question Index

1. [What is the API for compiling predicates to Clojure?](#bclj02-q-api)
2. [How does the threading macro work?](#bclj02-q-threading)
3. [Why use keep instead of filter?](#bclj02-q-keep)
4. [How are Prolog predicates translated to Clojure?](#bclj02-q-translation)
5. [How does keyword access with defaults work?](#bclj02-q-keyword-default)
6. [What makes Clojure sequences lazy?](#bclj02-q-lazy)
7. [How is JSON handled in the pipeline?](#bclj02-q-json)
8. [What imports are required for the pipeline?](#bclj02-q-imports)
9. [How do I run the generated Clojure code?](#bclj02-q-running)
10. [How does the process function signal filtering?](#bclj02-q-filtering)

---

<a id="bclj02-q-api"></a>
## Q1: What is the API for compiling predicates to Clojure?

**Question:** What is the main predicate for compiling Prolog to Clojure pipeline code?

**Answer:** Use `compile_predicate_to_clojure/3`:

```prolog
?- compile_predicate_to_clojure(filter/2, [pipeline_input(true)], Code).
```

Parameters:
- `filter/2`: Predicate to compile
- `[pipeline_input(true)]`: Enable stdin JSONL processing
- `Code`: Output variable for generated Clojure

**See:** [02_pipeline_mode_impl.md#compile_predicate_to_clojure3](02_pipeline_mode_impl.md#compile_predicate_to_clojure3)

---

<a id="bclj02-q-threading"></a>
## Q2: How does the threading macro work?

**Question:** What does the `->>` (thread-last) macro do in the pipeline?

**Answer:** The thread-last macro chains transformations, passing each result as the last argument to the next form:

```clojure
(->> (line-seq (io/reader *in*))  ; Read lines
     (filter seq)                   ; Skip empty
     (map #(json/read-str % :key-fn keyword))  ; Parse
     (keep process))                ; Filter
```

This is equivalent to nested calls:
```clojure
(keep process
  (map #(json/read-str % :key-fn keyword)
    (filter seq
      (line-seq (io/reader *in*)))))
```

**See:** [02_pipeline_mode_impl.md#threading-macro](02_pipeline_mode_impl.md#threading-macro)

---

<a id="bclj02-q-keep"></a>
## Q3: Why use keep instead of filter?

**Question:** Why does the pipeline use `keep` instead of `filter`?

**Answer:** `keep` is designed for functions that return `nil` to exclude items:

| Function | Behavior |
|----------|----------|
| `filter` | Keeps items where predicate returns truthy |
| `keep` | Keeps non-nil return values from function |

With `keep`, the process function returns the record or `nil`:
```clojure
(defn process [record]
  (when (> (:value record) 50)
    record))  ; Returns record or nil
```

**See:** [02_pipeline_mode_impl.md#keep-vs-filter](02_pipeline_mode_impl.md#keep-vs-filter)

---

<a id="bclj02-q-translation"></a>
## Q4: How are Prolog predicates translated to Clojure?

**Question:** How does the compiler translate Prolog patterns to Clojure?

**Answer:** Key translation rules:

| Prolog Pattern | Clojure Output |
|----------------|----------------|
| `get_field(R, "value", V)` | `(:value R)` |
| `V > 50` | `(> V 50)` |
| `Output = Input` | Return `record` |
| Failure (no match) | Return `nil` |

The predicate body becomes a `when` expression:
```clojure
(defn process [record]
  (when (> (:value record 0) 50)
    record))
```

**See:** [02_pipeline_mode_impl.md#filter-compilation](02_pipeline_mode_impl.md#filter-compilation)

---

<a id="bclj02-q-keyword-default"></a>
## Q5: How does keyword access with defaults work?

**Question:** How do I safely access map keys with a default value?

**Answer:** Use keyword as function with third argument:

```clojure
(:value record 0)     ; Returns 0 if :value missing
(:missing-key {} 42)  ; Returns 42
```

This is idiomatic Clojure for safe field access:
```clojure
;; Equivalent to:
(get record :value 0)
```

**See:** [02_pipeline_mode_impl.md#keyword-access-with-default](02_pipeline_mode_impl.md#keyword-access-with-default)

---

<a id="bclj02-q-lazy"></a>
## Q6: What makes Clojure sequences lazy?

**Question:** How does Clojure handle lazy evaluation in pipelines?

**Answer:** Clojure sequences are lazy by default:

```clojure
(line-seq reader)  ; Lazy sequence - reads on demand
```

Benefits:
- **Memory efficient**: Only holds needed elements
- **Streaming**: Processes data as it arrives
- **Composable**: Chains without intermediate collections

The entire pipeline is lazy until `doseq` forces evaluation.

**See:** [02_pipeline_mode_impl.md#lazy-evaluation](02_pipeline_mode_impl.md#lazy-evaluation)

---

<a id="bclj02-q-json"></a>
## Q7: How is JSON handled in the pipeline?

**Question:** How does the generated code parse and emit JSON?

**Answer:** Uses `clojure.data.json`:

```clojure
;; Parse with keyword keys
(json/read-str line :key-fn keyword)
;; {"value": 75} -> {:value 75}

;; Write JSON
(json/write-str {:value 75})
;; -> "{\"value\":75}"
```

The `:key-fn keyword` option converts string keys to keywords for idiomatic access.

**See:** [02_pipeline_mode_impl.md#json-handling](02_pipeline_mode_impl.md#json-handling)

---

<a id="bclj02-q-imports"></a>
## Q8: What imports are required for the pipeline?

**Question:** What namespaces must be required for the generated pipeline?

**Answer:** The generated code requires:

```clojure
(ns filter-pipeline
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]))
```

- `clojure.data.json`: JSON parsing/writing
- `clojure.java.io`: Reader for stdin (`*in*`)

For deps.edn:
```clojure
{:deps {org.clojure/data.json {:mvn/version "2.4.0"}}}
```

**See:** [02_pipeline_mode_impl.md#clojuredatajson](02_pipeline_mode_impl.md#clojuredatajson)

---

<a id="bclj02-q-running"></a>
## Q9: How do I run the generated Clojure code?

**Question:** How do I execute the generated Clojure pipeline?

**Answer:** Use the Clojure CLI:

```bash
echo '{"value": 75}' | clojure -M -m filter-pipeline
# Output: {"value":75}
```

For batch processing:
```bash
clojure -M -m filter-pipeline < input.jsonl > output.jsonl
```

The `-m` flag specifies the namespace with `-main` function.

**See:** [02_pipeline_mode_impl.md#running-generated-code](02_pipeline_mode_impl.md#running-generated-code)

---

<a id="bclj02-q-filtering"></a>
## Q10: How does the process function signal filtering?

**Question:** How does the process function indicate whether to include or exclude a record?

**Answer:** Return the record to include, `nil` to exclude:

```clojure
(defn process [record]
  (when (> (:value record 0) 50)
    record))  ; nil if condition false
```

The `when` form:
- Returns `record` if condition is true
- Returns `nil` if condition is false

`keep` then filters out all `nil` values.

**See:** [02_pipeline_mode_impl.md#filter-compilation](02_pipeline_mode_impl.md#filter-compilation)
