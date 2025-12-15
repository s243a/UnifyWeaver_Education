# Chapter 2: Recursion Patterns

TypeScript target supports multiple recursion patterns.

## Tail Recursion

O(1) stack space via accumulator pattern.

```prolog
?- compile_recursion(sum/3, [pattern(tail_recursion)], Code).
```

```typescript
export const sum = (n: number, acc: number = 0): number => {
  if (n <= 0) return acc;
  return sum(n - 1, acc + n);
};
```

> **Note**: JavaScript engines may not optimize tail calls. The target also generates a `sumStrict` loop version.

## List Fold

Array operations using `reduce`.

```prolog
?- compile_recursion(listSum/2, [pattern(list_fold)], Code).
```

```typescript
export const listSum = (items: number[]): number => {
  return items.reduce((acc, item) => acc + item, 0);
};

// Generic fold version
export const listSumFold = <T, R>(
  items: T[],
  initial: R,
  fn: (acc: R, item: T) => R
): R => {
  return items.reduce(fn, initial);
};
```

## Linear Recursion (Memoized)

For patterns like Fibonacci that recompute values.

```prolog
?- compile_recursion(fib/2, [pattern(linear_recursion)], Code).
```

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

## Factorial

Simple recursive pattern.

```prolog
?- compile_module([pred(factorial, 1, factorial)],
       [module_name('Math')], Code).
```

```typescript
export const factorial = (n: number): number => {
  if (n <= 1) return 1;
  return n * factorial(n - 1);
};
```

## Multiple Patterns in One Module

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

**Previous**: [Chapter 1: Introduction](01_introduction.md) | **Next**: [Chapter 3: Runtime Selection](03_runtimes.md)
