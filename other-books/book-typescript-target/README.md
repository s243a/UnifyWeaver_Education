# TypeScript Target Education Book

Learn to compile Prolog predicates to TypeScript for type-safe JavaScript.

## Chapters

1. [Introduction](01_introduction.md) - Setup, first example
2. [Recursion Patterns](02_recursion.md) - Tail, fold, memoization
3. [Runtime Selection](03_runtimes.md) - Node, Deno, Bun, Browser

## Prerequisites

- SWI-Prolog 8.0+
- Node.js or Deno or Bun

## Quick Example

```prolog
?- compile_recursion(sum/2, [pattern(tail_recursion)], Code).
```

Generates:

```typescript
export const sum = (n: number, acc: number = 0): number => {
  if (n <= 0) return acc;
  return sum(n - 1, acc + n);
};
```

## Why TypeScript?

- **Type Safety**: Catch errors at compile time
- **IDE Support**: IntelliSense, refactoring
- **JavaScript Ecosystem**: npm, React, Next.js
- **Multiple Runtimes**: Node, Deno, Bun, Browser
