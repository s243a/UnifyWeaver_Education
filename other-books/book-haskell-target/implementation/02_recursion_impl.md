<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Recursion Patterns - Implementation Details

This document provides function-level documentation for Haskell recursion compilation.

**Source**: `src/unifyweaver/targets/haskell_target.pl`

---

## Overview: Recursion Patterns

| Pattern | Haskell Implementation |
|---------|------------------------|
| `list_fold` | `foldr` or pattern matching |
| `list_tail_recursion` | `BangPatterns` strict accumulator |
| `factorial` | Direct recursion |
| `linear_recursion` | Tree recursion (e.g., Fibonacci) |

---

## compile_module_to_haskell/3

Compiles predicates to a Haskell module.

### Signature

```prolog
compile_module_to_haskell(+Predicates, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicates` | `list` | List of `pred(Name, Arity, Type)` |
| `Options` | `list` | `module_name(Name)` |
| `Code` | `string` | Generated Haskell module |

### Example

```prolog
?- compile_module_to_haskell(
       [pred(listSum, 2, list_fold)],
       [module_name('ListSum')],
       Code).
```

---

## List Fold with foldr

### Prolog Pattern

```prolog
list_sum([], 0).
list_sum([H|T], Sum) :- list_sum(T, Rest), Sum is H + Rest.
```

### Generated Haskell

```haskell
listSum :: [Int] -> Int
listSum = foldr (+) 0
```

### Explicit Recursive Version

```haskell
listSum :: [Int] -> Int
listSum [] = 0
listSum (h:t) = h + listSum t
```

---

## Tail Recursion with Strict Accumulator

### Prolog Pattern

```prolog
sum_acc([], Acc, Acc).
sum_acc([H|T], Acc, Sum) :- Acc1 is Acc + H, sum_acc(T, Acc1, Sum).
```

### Generated Haskell

```haskell
{-# LANGUAGE BangPatterns #-}

sumAcc :: [Int] -> Int -> Int
sumAcc [] !acc = acc
sumAcc (h:t) !acc = sumAcc t (acc + h)
```

### Why BangPatterns?

Without strict evaluation, Haskell builds thunks:

```
sumAcc [1,2,3] 0
= sumAcc [2,3] (0 + 1)        -- thunk
= sumAcc [3] ((0 + 1) + 2)    -- thunk chain
= sumAcc [] (((0 + 1) + 2) + 3)
= ((0 + 1) + 2) + 3           -- finally evaluate
```

With `!acc`, evaluation is forced immediately:

```
sumAcc [1,2,3] 0
= sumAcc [2,3] 1   -- evaluated
= sumAcc [3] 3     -- evaluated
= sumAcc [] 6
= 6
```

---

## Factorial

### Prolog Pattern

```prolog
factorial(0, 1).
factorial(N, R) :- N > 0, N1 is N - 1, factorial(N1, R1), R is N * R1.
```

### Generated Haskell

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)
```

---

## Fibonacci (Tree Recursion)

### Prolog Pattern

```prolog
fib(0, 0).
fib(1, 1).
fib(N, R) :- N > 1, N1 is N - 1, N2 is N - 2, fib(N1, R1), fib(N2, R2), R is R1 + R2.
```

### Generated Haskell

```haskell
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

---

## compile_predicate_to_haskell/3

Alternative API with type option.

### Signature

```prolog
compile_predicate_to_haskell(+Predicate/Arity, +Options, -Code)
```

### Options

| Option | Values |
|--------|--------|
| `type(facts)` | Compile as fact database |
| `type(rules)` | Compile as rule |
| `type(recursion)` | Compile with pattern |

### Example

```prolog
?- compile_predicate_to_haskell(parent/2, [type(facts)], Code1).
?- compile_predicate_to_haskell(ancestor/2, [type(rules)], Code2).
?- compile_predicate_to_haskell(sum/3, [type(recursion)], Code3).
```

---

## Type Annotations

All functions include Haskell type signatures:

| Prolog | Haskell |
|--------|---------|
| Integer | `Int` |
| List of int | `[Int]` |
| Two args | `a -> b -> c` |

---

## Related Documentation

- [Book Haskell Chapter 1: Introduction](../01_introduction.md)
- [Book Haskell Chapter 3: Parsec Integration](../03_parsec.md)
- [Haskell Target Source](../../../../../src/unifyweaver/targets/haskell_target.pl)
