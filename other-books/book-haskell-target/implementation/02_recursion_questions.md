<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Recursion Patterns - Questions

Q&A companion for [02_recursion_impl.md](./02_recursion_impl.md).

---

## Question Index

1. [What does compile_module_to_haskell/3 do?](#bhask02-q-compile)
2. [How is list fold compiled?](#bhask02-q-fold)
3. [What are BangPatterns?](#bhask02-q-bang)
4. [Why use strict accumulator?](#bhask02-q-strict)
5. [How is factorial compiled?](#bhask02-q-factorial)
6. [How is Fibonacci compiled?](#bhask02-q-fib)
7. [What does compile_predicate_to_haskell/3 do?](#bhask02-q-pred)
8. [What type annotations are generated?](#bhask02-q-types)
9. [What is foldr?](#bhask02-q-foldr)
10. [What patterns are supported?](#bhask02-q-patterns)

---

## Questions and Answers

### <a id="bhask02-q-compile"></a>Q1: What does compile_module_to_haskell/3 do?

**Answer**: Compiles predicates to a Haskell module:

```prolog
?- compile_module_to_haskell(
       [pred(listSum, 2, list_fold)],
       [module_name('ListSum')],
       Code).
```

**See**: [compile_module_to_haskell/3](./02_recursion_impl.md#compile_module_to_haskell3)

---

### <a id="bhask02-q-fold"></a>Q2: How is list fold compiled?

**Answer**: Uses `foldr`:

```haskell
listSum :: [Int] -> Int
listSum = foldr (+) 0
```

Or explicit pattern matching.

**See**: [List Fold with foldr](./02_recursion_impl.md#list-fold-with-foldr)

---

### <a id="bhask02-q-bang"></a>Q3: What are BangPatterns?

**Answer**: GHC extension for strict evaluation:

```haskell
{-# LANGUAGE BangPatterns #-}

sumAcc [] !acc = acc  -- ! forces evaluation
```

**See**: [Why BangPatterns?](./02_recursion_impl.md#why-bangpatterns)

---

### <a id="bhask02-q-strict"></a>Q4: Why use strict accumulator?

**Answer**: Prevents thunk buildup:

Without `!`: builds chain of unevaluated additions
With `!acc`: evaluates at each step, O(1) memory

**See**: [Why BangPatterns?](./02_recursion_impl.md#why-bangpatterns)

---

### <a id="bhask02-q-factorial"></a>Q5: How is factorial compiled?

**Answer**: Pattern matching with base cases:

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)
```

**See**: [Factorial](./02_recursion_impl.md#factorial)

---

### <a id="bhask02-q-fib"></a>Q6: How is Fibonacci compiled?

**Answer**: Tree recursion:

```haskell
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

**See**: [Fibonacci (Tree Recursion)](./02_recursion_impl.md#fibonacci-tree-recursion)

---

### <a id="bhask02-q-pred"></a>Q7: What does compile_predicate_to_haskell/3 do?

**Answer**: Alternative API with type option:

```prolog
?- compile_predicate_to_haskell(sum/3, [type(recursion)], Code).
```

Types: `facts`, `rules`, `recursion`.

**See**: [compile_predicate_to_haskell/3](./02_recursion_impl.md#compile_predicate_to_haskell3)

---

### <a id="bhask02-q-types"></a>Q8: What type annotations are generated?

**Answer**: Full Haskell type signatures:

| Prolog | Haskell |
|--------|---------|
| Integer | `Int` |
| List | `[Int]` |
| Multiple args | `a -> b -> c` |

**See**: [Type Annotations](./02_recursion_impl.md#type-annotations)

---

### <a id="bhask02-q-foldr"></a>Q9: What is foldr?

**Answer**: Right fold over a list:

```haskell
foldr (+) 0 [1,2,3]
= 1 + (2 + (3 + 0))
= 6
```

**See**: [List Fold with foldr](./02_recursion_impl.md#list-fold-with-foldr)

---

### <a id="bhask02-q-patterns"></a>Q10: What patterns are supported?

**Answer**:

| Pattern | Implementation |
|---------|----------------|
| `list_fold` | `foldr` |
| `list_tail_recursion` | `BangPatterns` |
| `factorial` | Direct recursion |
| `linear_recursion` | Tree recursion |

**See**: [Overview: Recursion Patterns](./02_recursion_impl.md#overview-recursion-patterns)

---

## Summary

Haskell recursion compilation provides:
- List fold via `foldr`
- Tail recursion with `BangPatterns` for O(1) memory
- Pattern matching for base cases
- Full type signatures
- Multiple compilation APIs
