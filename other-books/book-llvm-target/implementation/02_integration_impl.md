<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Integration - Implementation Details

Technical deep-dive for LLVM-compiled predicates integration with C, Go, and Rust.

## Overview

The LLVM target supports exporting predicates as shared libraries with C ABI for use in other languages.

## compile_tail_recursion_llvm/3 (with export)

### Signature

```prolog
compile_tail_recursion_llvm(+PredicateSpec, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `PredicateSpec` | `Name/Arity` | Predicate to compile (e.g., `sum/2`) |
| `Options` | `list` | Generation options |
| `Code` | `string` | Generated LLVM IR |

### Export Option

```prolog
?- compile_tail_recursion_llvm(sum/2, [export(true)], Code).
```

The `export(true)` option generates a `dllexport` wrapper function for C ABI compatibility.

### Generated LLVM IR

```llvm
define dllexport i64 @sum_ext(i64 %n) {
entry:
    %result = call i64 @sum(i64 %n, i64 0)
    ret i64 %result
}

define private i64 @sum(i64 %current, i64 %acc) {
entry:
    %cmp = icmp sle i64 %current, 0
    br i1 %cmp, label %done, label %loop

loop:
    %next = sub i64 %current, 1
    %newacc = add i64 %acc, %current
    %result = call i64 @sum(i64 %next, i64 %newacc)
    ret i64 %result

done:
    ret i64 %acc
}
```

## Building Shared Libraries

### Generate LLVM IR

```bash
swipl -g "compile_tail_recursion_llvm(sum/2, [export(true)], Code), write(Code)" > math.ll

# Fix Prolog %% escaping
sed -i 's/%%/%/g' math.ll
```

### Compile to Object File

```bash
llc -filetype=obj math.ll -o math.o
```

### Create Shared Library

```bash
# Linux
clang -shared math.o -o libmath.so

# macOS
clang -shared math.o -o libmath.dylib

# Windows
clang -shared math.o -o math.dll
```

## C Integration

### Header File

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
    printf("factorial(5) = %ld\n", factorial_ext(5));
    return 0;
}
```

### Compile and Link

```bash
# Compile
gcc -c main.c -o main.o

# Link
gcc -L. -lmath main.o -o main

# Run (set library path)
LD_LIBRARY_PATH=. ./main
```

## Go Integration (cgo)

### Go Program

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

### Build and Run

```bash
# Build
CGO_ENABLED=1 go build -o program main.go

# Run
LD_LIBRARY_PATH=. ./program
```

### cgo Notes

- `// #cgo LDFLAGS` specifies linker flags
- `import "C"` enables C interop
- C types accessed via `C.` prefix
- Must have header file for function signatures

## Rust Integration (FFI)

### Rust Program

```rust
// main.rs
extern "C" {
    fn sum_ext(n: i64) -> i64;
    fn factorial_ext(n: i64) -> i64;
}

fn main() {
    let sum = unsafe { sum_ext(10) };
    println!("sum(10) = {}", sum);

    let fact = unsafe { factorial_ext(5) };
    println!("factorial(5) = {}", fact);
}
```

### Cargo.toml

```toml
[package]
name = "prolog-math"
version = "0.1.0"
edition = "2021"

[dependencies]

[build-dependencies]
cc = "1.0"
```

### build.rs

```rust
fn main() {
    println!("cargo:rustc-link-search=native=.");
    println!("cargo:rustc-link-lib=math");
}
```

### Build and Run

```bash
cargo build
LD_LIBRARY_PATH=. cargo run
```

### Rust FFI Notes

- `extern "C"` declares external C functions
- `unsafe` required for FFI calls
- Link library via build.rs or cargo config

## Architecture Diagram

```
┌──────────────┐     ┌──────────────┐
│   Prolog     │────→│   LLVM IR    │
│  Predicates  │     │  (.ll file)  │
└──────────────┘     └──────┬───────┘
                            │
                     ┌──────▼───────┐
                     │  Object File │
                     │  (.o file)   │
                     └──────┬───────┘
                            │
                     ┌──────▼───────┐
                     │ Shared Lib   │
                     │ (.so/.dylib) │
                     └──────┬───────┘
        ┌───────────────────┼───────────────────┐
        ▼                   ▼                   ▼
   C Program            Go (cgo)           Rust (FFI)
```

## Platform-Specific Notes

### Linux

```bash
LD_LIBRARY_PATH=. ./program
```

### macOS

```bash
DYLD_LIBRARY_PATH=. ./program
```

### Windows

Place DLL in same directory or system PATH.

## Source Files

- `src/unifyweaver/targets/llvm_target.pl`
