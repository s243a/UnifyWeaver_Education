# Chapter 2: Recursion Patterns

This chapter demonstrates different recursion patterns compiled to Haskell.

## List Sum with Fold

A common pattern: sum a list of integers using an accumulator.

```prolog
list_sum([], 0).
list_sum([H|T], Sum) :- list_sum(T, Rest), Sum is H + Rest.
```

After running:

```prolog
?- compile_module_to_haskell(
       [pred(listSum, 2, list_fold)],
       [module_name('ListSum')],
       Code),
   write(Code).
```

The generated Haskell (using `foldr`):

```haskell
-- | listSum using Haskell's foldr
listSum :: [Int] -> Int
listSum = foldr (+) 0
```

Or the explicit recursive version:

```haskell
listSum :: [Int] -> Int
listSum [] = 0
listSum (h:t) = h + listSum t
```

## Tail Recursion with Accumulator

For better performance, use tail recursion with strict accumulator:

```prolog
sum_acc([], Acc, Acc).
sum_acc([H|T], Acc, Sum) :- Acc1 is Acc + H, sum_acc(T, Acc1, Sum).
```

After running:

```prolog
?- compile_module_to_haskell(
       [pred(sumAcc, 3, tail_recursion)],
       [module_name('SumAcc')], Code).
```

The generated Haskell:

```haskell
{-# LANGUAGE BangPatterns #-}

sumAcc :: [Int] -> Int -> Int
sumAcc [] !acc = acc
sumAcc (h:t) !acc = sumAcc t (acc + h)
```

### Why `!acc`?

Without the bang pattern, Haskell would build a thunk chain:
```
sumAcc [1,2,3] 0
= sumAcc [2,3] (0 + 1)
= sumAcc [3] ((0 + 1) + 2)
= sumAcc [] (((0 + 1) + 2) + 3)
= ((0 + 1) + 2) + 3  -- Now evaluate!
```

With `!acc`, it evaluates immediately:
```
sumAcc [1,2,3] 0
= sumAcc [2,3] 1
= sumAcc [3] 3
= sumAcc [] 6
= 6
```

## Factorial

```prolog
factorial(0, 1).
factorial(N, R) :- N > 0, N1 is N - 1, factorial(N1, R1), R is N * R1.
```

After running:

```prolog
?- compile_module_to_haskell([pred(factorial, 1, factorial)],
       [module_name('Factorial')], Code).
```

The generated Haskell:

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)
```

## Fibonacci (Tree Recursion)

```prolog
fib(0, 0).
fib(1, 1).
fib(N, R) :- N > 1, N1 is N - 1, N2 is N - 2, fib(N1, R1), fib(N2, R2), R is R1 + R2.
```

After running:

```prolog
?- compile_module_to_haskell([pred(fib, 2, linear_recursion)],
       [module_name('Fibonacci')], Code).
```

The generated Haskell:

```haskell
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

## Integration Test Results

All functions compile and run correctly with GHC:

```
[PASS] listSum [1..10] = 55
[PASS] sumAcc [1..100] 0 = 5050
[PASS] factorial 5 = 120
[PASS] factorial 10 = 3628800
[PASS] fib 10 = 55
[PASS] fib 15 = 610
```

---

**←** [Previous: Introduction](01_introduction.md) | [📖 Book: Haskell Target](./)
