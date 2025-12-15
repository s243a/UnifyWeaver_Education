# Chapter 1: Introduction to the Haskell Target

## Why Haskell for Prolog?

Haskell and Prolog share fundamental concepts:

| Prolog | Haskell |
|--------|---------|
| Facts + Rules | Data + Functions |
| Pattern matching (clauses) | Pattern matching (equations) |
| Backtracking | Lazy evaluation / List monad |
| Unification | Type inference |

## Architecture

```
┌───────────────────────────────────────┐
│        UnifyWeaver (Prolog)           │
│  compile_predicate_to_haskell/3       │
└─────────────────┬─────────────────────┘
                  │
                  ▼
            Module.hs (Haskell)
                  │
                  ▼
         ghc -O2 Module.hs
                  │
                  ▼
           Native binary
```

## Example: Facts to Data

```prolog
parent(tom, bob).
parent(bob, jim).
```

After running the compile predicate:

```prolog
?- compile_facts_to_haskell(parent, 2, Code),
   write(Code).
```

The generated Haskell:

```haskell
data Person = Tom | Bob | Jim
  deriving (Eq, Show)

parent :: [(Person, Person)]
parent = [(Tom, Bob), (Bob, Jim)]

isParent :: Person -> Person -> Bool
isParent x y = (x, y) `elem` parent
```

## Example: Recursion

```prolog
sum(0, Acc, Acc).
sum(N, Acc, Result) :- 
    N > 0, 
    N1 is N - 1, 
    Acc1 is Acc + N, 
    sum(N1, Acc1, Result).
```

After running the compile predicate:

```prolog
?- compile_recursion_to_haskell(sum/2, [], Code),
   write(Code).
```

The generated Haskell:

```haskell
{-# LANGUAGE BangPatterns #-}

sumTo :: Int -> Int -> Int
sumTo 0 !acc = acc
sumTo n !acc = sumTo (n - 1) (acc + n)
```

**Key difference**: The `!acc` (bang pattern) forces strict evaluation, enabling GHC to optimize to a loop - just like Prolog's tail recursion!

---

**→** [Next: Recursion Patterns](02_recursion.md)
