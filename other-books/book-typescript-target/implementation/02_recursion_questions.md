<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Recursion Patterns - Questions

Q&A companion for [02_recursion_impl.md](./02_recursion_impl.md).

---

## Question Index

1. [What does compile_recursion/3 do?](#bts02-q-compile)
2. [What recursion patterns are supported?](#bts02-q-patterns)
3. [How is tail recursion compiled?](#bts02-q-tail)
4. [Why generate a loop version?](#bts02-q-loop)
5. [How is list fold compiled?](#bts02-q-fold)
6. [How is memoization implemented?](#bts02-q-memo)
7. [How is factorial compiled?](#bts02-q-factorial)
8. [What does compile_module/3 do?](#bts02-q-module)
9. [What types are generated?](#bts02-q-types)
10. [What is the generic fold version?](#bts02-q-generic)

---

## Questions and Answers

### <a id="bts02-q-compile"></a>Q1: What does compile_recursion/3 do?

**Answer**: Compiles a recursive predicate to TypeScript:

```prolog
?- compile_recursion(sum/3, [pattern(tail_recursion)], Code).
```

Generates typed arrow functions with explicit return types.

**See**: [compile_recursion/3](./02_recursion_impl.md#compile_recursion3)

---

### <a id="bts02-q-patterns"></a>Q2: What recursion patterns are supported?

**Answer**: Four patterns:

| Pattern | Use Case |
|---------|----------|
| `tail_recursion` | O(1) stack with accumulator |
| `list_fold` | Array operations |
| `linear_recursion` | Memoized (e.g., Fibonacci) |
| `factorial` | Simple recursion |

**See**: [Overview: Recursion Patterns](./02_recursion_impl.md#overview-recursion-patterns)

---

### <a id="bts02-q-tail"></a>Q3: How is tail recursion compiled?

**Answer**: Arrow function with default accumulator:

```typescript
export const sum = (n: number, acc: number = 0): number => {
  if (n <= 0) return acc;
  return sum(n - 1, acc + n);
};
```

**See**: [Tail Recursion](./02_recursion_impl.md#tail-recursion)

---

### <a id="bts02-q-loop"></a>Q4: Why generate a loop version?

**Answer**: JavaScript engines may not optimize tail calls:

```typescript
export const sumStrict = (n: number): number => {
  let acc = 0;
  while (n > 0) { acc += n; n--; }
  return acc;
};
```

The loop version guarantees O(1) stack.

**See**: [Loop Version (Strict)](./02_recursion_impl.md#loop-version-strict)

---

### <a id="bts02-q-fold"></a>Q5: How is list fold compiled?

**Answer**: Uses `Array.reduce()`:

```typescript
export const listSum = (items: number[]): number => {
  return items.reduce((acc, item) => acc + item, 0);
};
```

**See**: [List Fold](./02_recursion_impl.md#list-fold)

---

### <a id="bts02-q-memo"></a>Q6: How is memoization implemented?

**Answer**: Module-level `Map` cache:

```typescript
const fibMemo = new Map<number, number>();

export const fib = (n: number): number => {
  if (fibMemo.has(n)) return fibMemo.get(n)!;
  const result = fib(n - 1) + fib(n - 2);
  fibMemo.set(n, result);
  return result;
};
```

First call O(n), cached calls O(1).

**See**: [Linear Recursion (Memoized)](./02_recursion_impl.md#linear-recursion-memoized)

---

### <a id="bts02-q-factorial"></a>Q7: How is factorial compiled?

**Answer**: Direct recursion:

```typescript
export const factorial = (n: number): number => {
  if (n <= 1) return 1;
  return n * factorial(n - 1);
};
```

**See**: [Factorial](./02_recursion_impl.md#factorial)

---

### <a id="bts02-q-module"></a>Q8: What does compile_module/3 do?

**Answer**: Compiles multiple predicates to one module:

```prolog
?- compile_module(
       [pred(sum, 2, tail_recursion),
        pred(fib, 2, linear_recursion)],
       [module_name('PrologMath')],
       Code).
```

**See**: [compile_module/3](./02_recursion_impl.md#compile_module3)

---

### <a id="bts02-q-types"></a>Q9: What types are generated?

**Answer**: Full TypeScript type annotations:

| Prolog | TypeScript |
|--------|------------|
| Integer arg | `number` |
| List arg | `T[]` |
| Return | Explicit return type |

**See**: [Type Annotations](./02_recursion_impl.md#type-annotations)

---

### <a id="bts02-q-generic"></a>Q10: What is the generic fold version?

**Answer**: Type-parameterized fold:

```typescript
export const listSumFold = <T, R>(
  items: T[],
  initial: R,
  fn: (acc: R, item: T) => R
): R => items.reduce(fn, initial);
```

**See**: [Generic Fold Version](./02_recursion_impl.md#generic-fold-version)

---

## Summary

TypeScript recursion compilation provides:
- Tail recursion with accumulator pattern
- Loop version for guaranteed O(1) stack
- List fold via `Array.reduce()`
- Memoization via `Map`
- Full type annotations
- Multi-predicate module compilation
