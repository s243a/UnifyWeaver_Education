# Chapter 2: Recursion Patterns

## Tail Recursion with BangPatterns

GHC can optimize tail recursion to loops when the accumulator is strict.

```prolog
sum(0, Acc, Acc).
sum(N, Acc, Result) :- N > 0, N1 is N - 1, Acc1 is Acc + N, sum(N1, Acc1, Result).
```

After running:

```prolog
?- compile_module_to_haskell(
       [pred(sumTo, 2, tail_recursion)],
       [module_name('Sum')],
       Code),
   write(Code).
```

The generated Haskell:

```haskell
{-# LANGUAGE BangPatterns #-}

sumTo :: Int -> Int -> Int
sumTo 0 !acc = acc           -- Return accumulator when N = 0
sumTo n !acc = sumTo (n - 1) (acc + n)  -- Recurse with updated acc
```

### Why `!acc`?

Without the bang pattern, Haskell would build a thunk chain:
```
sumTo 3 0
= sumTo 2 (0 + 3)
= sumTo 1 ((0 + 3) + 2)
= sumTo 0 (((0 + 3) + 2) + 1)
= ((0 + 3) + 2) + 1  -- Now evaluate!
```

With `!acc`, it evaluates immediately:
```
sumTo 3 0
= sumTo 2 3
= sumTo 1 5
= sumTo 0 6
= 6
```

## Factorial

```prolog
factorial(0, 1).
factorial(N, R) :- N > 0, N1 is N - 1, factorial(N1, R1), R is N * R1.
```

→

```haskell
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n - 1)
```

## Fibonacci (Linear Recursion)

```prolog
fib(0, 0).
fib(1, 1).
fib(N, R) :- N > 1, N1 is N - 1, N2 is N - 2, fib(N1, R1), fib(N2, R2), R is R1 + R2.
```

→

```haskell
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

## Integration Test Results

All functions compile and run correctly with GHC:

```
[PASS] sumTo 10 0 = 55
[PASS] sumTo 100 0 = 5050
[PASS] factorial 5 = 120
[PASS] factorial 10 = 3628800
[PASS] fib 10 = 55
[PASS] fib 15 = 610
```

---

**←** [Previous: Introduction](01_introduction.md) | [📖 Book: Haskell Target](./)
