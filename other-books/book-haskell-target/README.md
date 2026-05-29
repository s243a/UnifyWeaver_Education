# Haskell Target for UnifyWeaver

Compile Prolog predicates to type-safe Haskell code.

## Chapters

1. **[Introduction](01_introduction.md)** - Why Haskell, pattern matching
2. **[Recursion](02_recursion.md)** - Tail recursion with BangPatterns
3. **[Parsec](03_parsec.md)** - DCG → Parser Combinators
4. **[Performance](04_performance.md)** - How the Haskell WAM target beat Rust

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

## Generating Haskell Hybrid WAM Code

Use the WAM Haskell target when the chapter is discussing the WAM runtime,
foreign kernels, or fact-access experiments rather than the older Haskell-native
examples:

```prolog
?- use_module('src/unifyweaver/targets/wam_haskell_target').
?- write_wam_haskell_project([ancestor/2],
       [emit_mode(auto), lmdb_materialisation(auto)],
       'out/wam_haskell_ancestor').
```

The writer partitions predicates between interpreted WAM paths, lowered
functions, fact layouts, and kernel support according to the target's current
capabilities and options.

## Hybrid WAM Role

Haskell demonstrates how a target can keep WAM semantics while experimenting
with purity-aware routing, parallel search, FFI, and different state
representations. Book 17 covers the shared Hybrid WAM concepts; this book is
a target-specific case study in functional implementation tradeoffs.

## Why Haskell?

| Feature | Benefit |
|---------|---------|
| Pattern matching | Natural fit for Prolog clauses |
| Lazy evaluation | Good for streams/backtracking |
| Type safety | Catch errors at compile time |
| GHC optimizer | Excellent performance |
