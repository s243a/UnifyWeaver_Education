<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 7: Recursive Queries

This chapter covers compiling recursive predicates like `ancestor/2` to Go using BFS-based transitive closure.

## The Ancestor Problem

The classic transitive closure example demonstrates recursive query compilation:

```prolog
% family_tree.pl
parent(abraham, isaac).
parent(isaac, jacob).
parent(jacob, joseph).

ancestor(A, D) :- parent(A, D).
ancestor(A, D) :- parent(A, P), ancestor(P, D).
```

## Compiling to Go

Use `compile_recursive/3` from the recursive compiler module:

```prolog
?- ['education/book-02-bash-target/examples/family_tree'].
?- use_module('src/unifyweaver/core/recursive_compiler').

?- compile_recursive(ancestor/2, [target(go)], Code),
   open('ancestor.go', write, S),
   write(S, Code),
   close(S).
```

## Generated Go Code

The compiler generates BFS-based Go code using maps and slices:

```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
)

// ANCESTORQuery manages transitive closure for ancestor/2
type ANCESTORQuery struct {
    baseRelation map[string][]string
}

// NewANCESTORQuery creates a new query instance
func NewANCESTORQuery() *ANCESTORQuery {
    return &ANCESTORQuery{baseRelation: make(map[string][]string)}
}

// AddFact adds a base relation fact
func (q *ANCESTORQuery) AddFact(from, to string) {
    q.baseRelation[from] = append(q.baseRelation[from], to)
}

// FindAll finds all reachable nodes from start using BFS
func (q *ANCESTORQuery) FindAll(start string) []string {
    visited := make(map[string]bool)
    queue := []string{start}
    visited[start] = true
    var results []string

    for len(queue) > 0 {
        current := queue[0]
        queue = queue[1:]

        for _, next := range q.baseRelation[current] {
            if !visited[next] {
                visited[next] = true
                queue = append(queue, next)
                results = append(results, next)
            }
        }
    }
    return results
}

// Check checks if target is reachable from start
func (q *ANCESTORQuery) Check(start, target string) bool {
    for _, r := range q.FindAll(start) {
        if r == target {
            return true
        }
    }
    return false
}

func main() {
    q := NewANCESTORQuery()

    // Load facts from stdin
    scanner := bufio.NewScanner(os.Stdin)
    for scanner.Scan() {
        line := scanner.Text()
        parts := strings.Split(line, ":")
        if len(parts) >= 2 {
            q.AddFact(strings.TrimSpace(parts[0]), strings.TrimSpace(parts[1]))
        }
    }

    // Query mode (if args provided)
    if len(os.Args) >= 2 {
        start := os.Args[1]
        if len(os.Args) >= 3 {
            // Check mode
            target := os.Args[2]
            if q.Check(start, target) {
                fmt.Printf("%s:%s\n", start, target)
            }
        } else {
            // FindAll mode
            for _, result := range q.FindAll(start) {
                fmt.Printf("%s:%s\n", start, result)
            }
        }
    }
}
```

## Running the Go Code

```bash
# Compile
go build ancestor.go

# Run with facts from stdin
cat << 'EOF' | ./ancestor abraham
abraham:isaac
isaac:jacob
jacob:joseph
EOF

# Output:
# abraham:isaac
# abraham:jacob
# abraham:joseph
```

## Go-Specific Features

The generated code leverages Go idioms:

| Feature | Go Construct |
|---------|--------------|
| **Adjacency List** | `map[string][]string` |
| **BFS Queue** | `[]string` slice |
| **Visited Set** | `map[string]bool` |
| **Stream Input** | `bufio.Scanner` |
| **String Split** | `strings.Split` |

## Options

The `compile_recursive/3` predicate accepts these options for Go:

| Option | Description |
|--------|-------------|
| `target(go)` | Generate Go code |
| `format(struct)` | Use struct-based output (default) |
| `delimiter(colon)` | Field delimiter for output |

## When to Use Recursive Compilation

Use `compile_recursive/3` when:
- You have transitive closure queries (ancestor, path finding)
- The recursion depth is unknown
- You need efficient BFS-based evaluation

For simple non-recursive rules, use `compile_predicate_to_go/3` instead.

## Tail Recursion Optimization

For predicates with accumulator patterns, use `compile_tail_recursion_go/3`:

```prolog
% Define tail recursive sum
assertz((sum_list([], Acc, Acc))).
assertz((sum_list([H|T], Acc, S) :- Acc1 is Acc + H, sum_list(T, Acc1, S))).

% Compile
?- go_target:compile_tail_recursion_go(sum_list/3, [], Code).
```

**Generated Go (O(1) stack space):**
```go
func sum_list(items []int, acc int) int {
    for _, item := range items {
        acc += item
    }
    return acc
}
```

**Benefits:**
- O(1) stack space (no recursion)
- Automatically detects step operation (+, *, etc.)
- Supports arity 2 and 3 predicates

## Linear Recursion with Memoization

For predicates with overlapping subproblems, use `compile_linear_recursion_go/3`:

```prolog
% Define linear recursive triangular numbers
assertz((triangular(0, 0))).
assertz((triangular(1, 1))).
assertz((triangular(N, F) :- N > 1, N1 is N - 1, triangular(N1, F1), F is F1 + N)).

% Compile
?- go_target:compile_linear_recursion_go(triangular/2, [], Code).
```

**Generated Go (with memoization):**
```go
var triangularMemo = make(map[int]int)

func triangular(n int) int {
    if result, ok := triangularMemo[n]; ok {
        return result
    }
    
    if n <= 0 { return 0 }
    if n == 1 { return 1 }
    
    result := triangular(n-1) + n
    triangularMemo[n] = result
    return result
}
```

**Benefits:**
- O(n) time via memoization
- `map[int]int` for efficient lookup
- Base case detection

## Mutual Recursion

For predicates that call each other (like `is_even`/`is_odd`), use `compile_mutual_recursion_go/3`:

```prolog
assertz((is_even(0))).
assertz((is_even(N) :- N > 0, N1 is N - 1, is_odd(N1))).
assertz((is_odd(1))).
assertz((is_odd(N) :- N > 1, N1 is N - 1, is_even(N1))).

?- go_target:compile_mutual_recursion_go([is_even/1, is_odd/1], [], Code).
```

**Generated Go (shared memo):**
```go
var is_even_is_odd_Memo = make(map[string]bool)

func is_even(n int) bool {
    key := fmt.Sprintf("is_even:%d", n)
    if result, ok := is_even_is_odd_Memo[key]; ok {
        return result
    }
    
    if n == 0 {
        is_even_is_odd_Memo[key] = true
        return true
    }
    if n > 0 {
        result := is_odd(n - 1)
        is_even_is_odd_Memo[key] = result
        return result
    }
    return false
}

func is_odd(n int) bool {
    // Similar pattern...
}
```

## Recursion Pattern Summary

| Pattern | API | Generated Code | Use Case |
|---------|-----|----------------|----------|
| Transitive Closure | `compile_recursive/3` | BFS with queue | Graph reachability |
| Tail Recursion | `compile_tail_recursion_go/3` | for loop | Accumulators |
| Linear Recursion | `compile_linear_recursion_go/3` | Memoized function | Overlapping subproblems |
| Mutual Recursion | `compile_mutual_recursion_go/3` | Shared memo map | is_even/is_odd, state machines |

---

## How Go Can Host Hybrid WAM

A Go hybrid WAM target can keep the runtime model explicit. Registers become
fields or slices on a `WamState`; choice points become structs on a stack; fact
indexes become maps from keys to candidate rows. That makes Go a good target
for teaching the mechanical path from WAM items to host code.

Consider the symbolic item:

```wam
get_constant alice, A1
```

In a direct interpreter this is a runtime operation: read argument register
`A1`, compare or bind it to the atom `alice`, and trail any binding that must
be undone on backtracking. In a lowered deterministic helper, the same work can
be emitted as straight Go code inside a generated method:

```go
func (vm *WamState) PredParent2() bool {
    if !vm.GetConstant(vm.Atom("alice"), 1) {
        return false
    }
    if !vm.GetConstant(vm.Atom("bob"), 2) {
        return false
    }
    return true
}
```

For multi-clause predicates, Go can mix approaches. A first deterministic
clause may be lowered into a helper, while later alternatives remain in an
instruction array with explicit choice points. The important point is that both
paths operate on the same WAM state contract.

| WAM concept | Go representation | Effect |
|---|---|---|
| Argument registers | `[]Value` or fields on `WamState` | Carry call inputs and outputs. |
| Choice point | struct with next label, trail mark, register snapshot | Restores search state on failure. |
| Indexed facts | `map[Value][]Fact` | Avoids scanning when an argument is bound. |
| Lowered helper | method on `*WamState` | Executes common deterministic paths without the dispatch loop. |

## Hybrid WAM Role

Go is a useful example of a target that can keep WAM state explicit while
still lowering selected predicates into ordinary host-language functions.
The shared Hybrid WAM concepts are covered in Book 17; the Go-specific point
is that registers, choice points, indexed fact access, and deterministic
predicate helpers can be represented with simple structs, slices, maps, and
methods.

- Default generation path: structured WAM items or target-ready WAM data
  should feed the Go emitter directly.
- Symbolic WAM text: useful as a readable debug listing, not as the normal
  internal bridge.
- Target-specific emphasis: explicit state, indexed dispatch, and selective
  lowered helpers for deterministic pieces of a larger WAM-shaped program.

## Navigation

**←** [Previous: Chapter 6: Generator Mode](06_generator_mode) | [📖 Book 6: Go Target](./) | [Next: Book 7: Cross-Target Glue →](../book-07-cross-target-glue/)
