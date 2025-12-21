<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Appendix A: Go Target API Reference

This appendix documents all compilation predicates available in the Go target.

## Core Compilation APIs

### compile_predicate_to_go/3

**Module:** `src/unifyweaver/targets/go_target`

**Signature:**
```prolog
compile_predicate_to_go(+Predicate/Arity, +Options, -Code)
```

**Description:** Main compilation predicate for both streaming and generator modes.

**Parameters:**
- `Predicate/Arity`: The predicate to compile (e.g., `ancestor/2`)
- `Options`: List of compilation options
- `Code`: Output variable for generated Go code

**Options:**

| Option | Default | Description |
|--------|---------|-------------|
| `mode(streaming)` | ✓ | Generate stdin→stdout processor |
| `mode(generator)` | - | Generate fixpoint Datalog engine |
| `json_input(true)` | false | Read facts from stdin as JSONL |
| `json_output(true)` | false | Output results as JSONL |
| `field_delimiter(D)` | `:` | Field separator for text I/O |
| `workers(N)` | 1 | Parallel goroutines (generator mode) |
| `db_backend(bbolt)` | - | Enable BoltDB persistence |
| `db_file(Path)` | `'facts.db'` | Database file path |
| `db_bucket(Name)` | `'facts'` | Database bucket name |

**Example:**
```prolog
?- use_module('src/unifyweaver/targets/go_target').
?- compile_predicate_to_go(ancestor/2, [mode(generator), workers(4)], Code).
```

---

### compile_facts_to_go/3

**Module:** `src/unifyweaver/targets/go_target`

**Signature:**
```prolog
compile_facts_to_go(+Functor, +Arity, -Code)
```

**Description:** Exports asserted facts as Go struct slices with lookup functions.

**Parameters:**
- `Functor`: The fact functor name (e.g., `user`)
- `Arity`: Number of arguments
- `Code`: Output variable for generated Go code

**Generated Functions:**
- `GetAll<NAME>() []<NAME>` - Returns all facts
- `Stream<NAME>(fn func(<NAME>))` - Iterates with callback
- `Contains<NAME>(target <NAME>) bool` - Membership check

**Example:**
```prolog
?- assertz(user(alice, 25)).
?- assertz(user(bob, 30)).
?- go_target:compile_facts_to_go(user, 2, Code).
```

---

## Recursion APIs

### compile_recursive/3

**Module:** `src/unifyweaver/core/recursive_compiler`

**Signature:**
```prolog
compile_recursive(+Predicate/Arity, +Options, -Code)
```

**Description:** Compiles recursive predicates using BFS-based transitive closure.

**Options:**

| Option | Description |
|--------|-------------|
| `target(go)` | Generate Go code |
| `format(struct)` | Use struct-based output (default) |
| `delimiter(colon)` | Field delimiter for output |

**Complexity:** O(V + E) where V = vertices, E = edges

**Example:**
```prolog
?- use_module('src/unifyweaver/core/recursive_compiler').
?- compile_recursive(ancestor/2, [target(go)], Code).
```

---

### compile_tail_recursion_go/3

**Module:** `src/unifyweaver/targets/go_target`

**Signature:**
```prolog
compile_tail_recursion_go(+Predicate/Arity, +Options, -Code)
```

**Description:** Compiles tail-recursive predicates to iterative Go loops.

**Complexity:** O(n) time, O(1) stack space

**Supported Patterns:**
- Accumulator-based recursion
- Step operations: `+`, `*`, `-`, `/`

**Example:**
```prolog
?- assertz((sum_list([], Acc, Acc))).
?- assertz((sum_list([H|T], Acc, S) :- Acc1 is Acc + H, sum_list(T, Acc1, S))).
?- go_target:compile_tail_recursion_go(sum_list/3, [], Code).
```

---

### compile_linear_recursion_go/3

**Module:** `src/unifyweaver/targets/go_target`

**Signature:**
```prolog
compile_linear_recursion_go(+Predicate/Arity, +Options, -Code)
```

**Description:** Compiles linear recursive predicates with memoization.

**Complexity:** O(n) time via memoization, O(n) space for memo table

**Generated Structure:**
- `map[int]int` for integer memoization
- Base case detection
- Automatic recursive call wrapping

**Example:**
```prolog
?- assertz((triangular(0, 0))).
?- assertz((triangular(N, F) :- N > 0, N1 is N - 1, triangular(N1, F1), F is F1 + N)).
?- go_target:compile_linear_recursion_go(triangular/2, [], Code).
```

---

### compile_mutual_recursion_go/3

**Module:** `src/unifyweaver/targets/go_target`

**Signature:**
```prolog
compile_mutual_recursion_go(+PredicateList, +Options, -Code)
```

**Description:** Compiles mutually recursive predicates with shared memoization.

**Parameters:**
- `PredicateList`: List of predicates that call each other (e.g., `[is_even/1, is_odd/1]`)

**Generated Structure:**
- Shared `map[string]bool` for all predicates
- Key format: `"predicate_name:arg_value"`

**Example:**
```prolog
?- assertz((is_even(0))).
?- assertz((is_even(N) :- N > 0, N1 is N - 1, is_odd(N1))).
?- assertz((is_odd(N) :- N > 0, N1 is N - 1, is_even(N1))).
?- go_target:compile_mutual_recursion_go([is_even/1, is_odd/1], [], Code).
```

---

## Semantic APIs

### semantic_search/3

**Runtime predicate** (compiled into generated code)

**Signature:**
```prolog
semantic_search(+Query, +TopK, -Results)
```

**Description:** Performs vector similarity search using embeddings.

**Requirements:**
- ONNX Runtime (`github.com/owulveryck/onnx-go`)
- Model file in `models/` directory

**Example:**
```prolog
find_docs(Query) :-
    semantic_search(Query, 5, _Results).
```

---

### crawler_run/2

**Runtime predicate** (compiled into generated code)

**Signature:**
```prolog
crawler_run(+Seeds, +MaxDepth)
```

**Description:** Crawls RDF/XML documents and stores with embeddings.

**Parameters:**
- `Seeds`: List of starting URLs
- `MaxDepth`: Maximum crawl depth

**Example:**
```prolog
start_crawl :-
    crawler_run(["http://example.org/data.xml"], 3).
```

---

## API Selection Guide

| Use Case | API | Mode |
|----------|-----|------|
| Export facts to Go structs | `compile_facts_to_go/3` | - |
| Simple stream transform | `compile_predicate_to_go/3` | streaming |
| Full Datalog with joins | `compile_predicate_to_go/3` | generator |
| Transitive closure (BFS) | `compile_recursive/3` | - |
| Accumulator loops | `compile_tail_recursion_go/3` | - |
| Memoized recursion | `compile_linear_recursion_go/3` | - |
| Mutually recursive predicates | `compile_mutual_recursion_go/3` | - |
| Semantic search | Use `semantic_search/3` in rules | generator |

---

## Navigation

[📖 Book 6: Go Target](./) | [Appendix B: Complexity Guide →](A2_complexity_guide)
