# Chapter 3: Recursive Queries

All recursion patterns supported by the LLVM target.

## Pattern Summary

| Pattern | API | LLVM Feature |
|---------|-----|--------------|
| Tail Recursion | `compile_tail_recursion_llvm/3` | `musttail` |
| Linear Recursion | `compile_linear_recursion_llvm/3` | Static memo |
| Mutual Recursion | `compile_mutual_recursion_llvm/3` | Cross `musttail` |
| Transitive Closure | `compile_transitive_closure_llvm/3` | BFS worklist |

## Tail Recursion

Guaranteed O(1) stack space via `musttail`:

```prolog
?- compile_tail_recursion_llvm(sum/2, [], Code).
```

**Generated:**
```llvm
define i64 @sum(i64 %n, i64 %acc) {
recurse:
  %result = musttail call i64 @sum(i64 %n1, i64 %acc1)
  ret i64 %result
}
```

## Linear Recursion

With static memoization table:

```prolog
?- compile_linear_recursion_llvm(fib/2, [], Code).
```

**Generated:**
```llvm
@memo = internal global [1000 x i64] zeroinitializer
@memo_valid = internal global [1000 x i1] zeroinitializer

define i64 @fib(i64 %n) {
check_memo:
  %is_valid = load i1, i1* %valid_ptr
  br i1 %is_valid, label %return_memo, label %compute
  ...
}
```

## Mutual Recursion

For is_even/is_odd patterns:

```prolog
?- compile_mutual_recursion_llvm([is_even/1, is_odd/1], [], Code).
```

**Generated:**
```llvm
define i1 @is_even(i64 %n) {
recurse:
  %result = musttail call i1 @is_odd(i64 %n1)
  ret i1 %result
}

define i1 @is_odd(i64 %n) {
recurse:
  %result = musttail call i1 @is_even(i64 %n1)
  ret i1 %result
}
```

## Transitive Closure

BFS worklist for graph reachability:

```prolog
?- compile_transitive_closure_llvm(reachable/2, [], Code).
```

**Generated Data Structures:**
```llvm
@edges = internal global [100 x i64] zeroinitializer
@edge_count = internal global i64 0
@visited = internal global [1000 x i1] zeroinitializer
@queue = internal global [1000 x i64] zeroinitializer
```

**Algorithm:**
1. Add edges via `@add_edge(from, to)`
2. Call `@reachable(source, target)`
3. BFS explores neighbors, marks visited
4. Returns `true` if target reachable

**Test:**
```bash
# Graph: 0 → 1 → 2 → 3
./reachable  # Exit 0 = reachable
```

## Comparison with Other Targets

| Pattern | LLVM | Go | Rust |
|---------|------|-----|------|
| Tail Call Opt | **musttail (guaranteed)** | No | Yes |
| Memo | Static array | sync.Map | HashMap |
| Transitive | BFS | BFS | BFS |

---

**←** [Previous: Integration](02_integration.md) | [📖 Book: LLVM Target](./)
