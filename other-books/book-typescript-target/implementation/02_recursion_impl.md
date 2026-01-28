<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Recursion Patterns - Implementation Details

This document provides function-level documentation for TypeScript recursion compilation.

**Source**: `src/unifyweaver/targets/typescript_target.pl`

---

## Overview: Recursion Patterns

| Pattern | Stack Space | TypeScript Implementation |
|---------|-------------|---------------------------|
| `tail_recursion` | O(1) | Accumulator + loop version |
| `list_fold` | O(1) | `Array.reduce()` |
| `linear_recursion` | O(n) | Memoized with `Map` |
| `factorial` | O(n) | Direct recursion |

---

## compile_recursion/3

Compiles a recursive predicate to TypeScript.

### Signature

```prolog
compile_recursion(+Predicate/Arity, +Options, -Code)
```

### Options

| Option | Values | Description |
|--------|--------|-------------|
| `pattern(P)` | `tail_recursion`, `list_fold`, `linear_recursion`, `factorial` | Recursion pattern |

### Example

```prolog
?- compile_recursion(sum/3, [pattern(tail_recursion)], Code).
```

---

## Tail Recursion

### Prolog Pattern

```prolog
sum(0, Acc, Acc).
sum(N, Acc, R) :- N > 0, N1 is N-1, Acc1 is Acc+N, sum(N1, Acc1, R).
```

### Generated TypeScript

```typescript
export const sum = (n: number, acc: number = 0): number => {
  if (n <= 0) return acc;
  return sum(n - 1, acc + n);
};
```

### Loop Version (Strict)

```typescript
export const sumStrict = (n: number): number => {
  let acc = 0;
  while (n > 0) {
    acc += n;
    n--;
  }
  return acc;
};
```

> Note: JavaScript engines may not optimize tail calls. The target generates both versions.

---

## List Fold

### Prolog Pattern

```prolog
list_sum([], 0).
list_sum([H|T], S) :- list_sum(T, R), S is H + R.
```

### Generated TypeScript

```typescript
export const listSum = (items: number[]): number => {
  return items.reduce((acc, item) => acc + item, 0);
};
```

### Generic Fold Version

```typescript
export const listSumFold = <T, R>(
  items: T[],
  initial: R,
  fn: (acc: R, item: T) => R
): R => {
  return items.reduce(fn, initial);
};
```

---

## Linear Recursion (Memoized)

### Prolog Pattern

```prolog
fib(0, 0).
fib(1, 1).
fib(N, R) :- N > 1, N1 is N-1, N2 is N-2, fib(N1, R1), fib(N2, R2), R is R1+R2.
```

### Generated TypeScript

```typescript
const fibMemo = new Map<number, number>();

export const fib = (n: number): number => {
  if (n <= 0) return 0;
  if (n === 1) return 1;

  if (fibMemo.has(n)) {
    return fibMemo.get(n)!;
  }

  const result = fib(n - 1) + fib(n - 2);
  fibMemo.set(n, result);
  return result;
};
```

### Memoization Details

| First call | Cached calls |
|------------|--------------|
| O(n) | O(1) |

Uses `Map<number, number>` for module-level cache.

---

## Factorial

### Prolog Pattern

```prolog
factorial(0, 1).
factorial(N, R) :- N > 0, N1 is N-1, factorial(N1, R1), R is N * R1.
```

### Generated TypeScript

```typescript
export const factorial = (n: number): number => {
  if (n <= 1) return 1;
  return n * factorial(n - 1);
};
```

---

## compile_module/3

Compiles multiple predicates to a TypeScript module.

### Signature

```prolog
compile_module(+Predicates, +Options, -Code)
```

### Example

```prolog
?- compile_module(
       [pred(sum, 2, tail_recursion),
        pred(listSum, 2, list_fold),
        pred(fib, 2, linear_recursion),
        pred(factorial, 1, factorial)],
       [module_name('PrologMath')],
       Code),
   write_typescript_module(Code, 'PrologMath.ts').
```

---

## Type Annotations

All generated functions include TypeScript types:

| Prolog | TypeScript |
|--------|------------|
| Integer arg | `number` |
| List arg | `T[]` |
| Return value | Explicit return type |

---

## Related Documentation

- [Book TypeScript Chapter 1: Introduction](../01_introduction.md)
- [Book TypeScript Chapter 3: Runtime Selection](../03_runtimes.md)
- [TypeScript Target Source](../../../../../src/unifyweaver/targets/typescript_target.pl)
