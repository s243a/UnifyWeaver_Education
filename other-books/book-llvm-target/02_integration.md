# Chapter 2: Integration with C, Go, and Rust

Export LLVM-compiled predicates for use in other languages.

## Overview

The LLVM target supports C ABI export via:
```prolog
compile_tail_recursion_llvm(sum/2, [export(true)], Code).
```

This generates a `dllexport` wrapper function.

## C Integration

### Generate Shared Library

```bash
# Generate LLVM IR with export
swipl -g "..." > factorial.ll
sed -i 's/%%/%/g' factorial.ll

# Compile to shared library
llc -filetype=obj factorial.ll -o factorial.o
clang -shared factorial.o -o libmath.so
```

### C Header

```c
// prolog_math.h
#pragma once
#include <stdint.h>

int64_t sum_ext(int64_t n);
int64_t factorial_ext(int64_t n);
```

### C Program

```c
#include <stdio.h>
#include "prolog_math.h"

int main() {
    printf("sum(10) = %ld\n", sum_ext(10));
    return 0;
}
```

### Compile and Link

```bash
gcc -L. -lmath main.c -o main
LD_LIBRARY_PATH=. ./main
```

## Go Integration (cgo)

```go
package main

// #cgo LDFLAGS: -L. -lmath
// #include "prolog_math.h"
import "C"
import "fmt"

func main() {
    result := C.sum_ext(10)
    fmt.Println("sum(10) =", result)
}
```

```bash
CGO_ENABLED=1 go build
LD_LIBRARY_PATH=. ./program
```

## Rust Integration (FFI)

```rust
extern "C" {
    fn sum_ext(n: i64) -> i64;
}

fn main() {
    let result = unsafe { sum_ext(10) };
    println!("sum(10) = {}", result);
}
```

**Cargo.toml:**
```toml
[dependencies]

[build-dependencies]
cc = "1.0"
```

## Architecture

```
┌──────────────┐     ┌──────────────┐
│   Prolog     │────→│   LLVM IR    │
└──────────────┘     └──────┬───────┘
                            │
        ┌───────────────────┼───────────────────┐
        ▼                   ▼                   ▼
   libmath.so          main.go              main.rs
   (C ABI)             (cgo)                (FFI)
```

---

**←** [Previous: Introduction](01_introduction.md) | **→** [Next: Recursive Queries](03_recursive_queries.md)
