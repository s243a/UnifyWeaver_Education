# Haskell Target for UnifyWeaver

Compile Prolog predicates to type-safe Haskell code.

## Chapters

1. **[Introduction](01_introduction.md)** - Why Haskell, pattern matching
2. **[Recursion](02_recursion.md)** - Tail recursion with BangPatterns
3. **[Parsec](03_parsec.md)** - DCG → Parser Combinators

## Prerequisites

- SWI-Prolog
- GHC (Glasgow Haskell Compiler)

```bash
sudo apt install ghc
```

## Quick Example

```prolog
?- compile_recursion_to_haskell(sum/2, [], Code).
```

→

```haskell
{-# LANGUAGE BangPatterns #-}
sumTo :: Int -> Int -> Int
sumTo 0 !acc = acc
sumTo n !acc = sumTo (n - 1) (acc + n)
```

## Why Haskell?

| Feature | Benefit |
|---------|---------|
| Pattern matching | Natural fit for Prolog clauses |
| Lazy evaluation | Good for streams/backtracking |
| Type safety | Catch errors at compile time |
| GHC optimizer | Excellent performance |
