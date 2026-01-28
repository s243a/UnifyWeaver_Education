<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Integration - Questions

Q&A companion for LLVM integration with C, Go, and Rust.

## Question Index

1. [How do I export predicates for C ABI?](#bllvm02-q-export)
2. [What does the dllexport wrapper do?](#bllvm02-q-wrapper)
3. [How do I build a shared library from LLVM IR?](#bllvm02-q-build)
4. [How do I call exported functions from C?](#bllvm02-q-c)
5. [How does Go integrate via cgo?](#bllvm02-q-go)
6. [How do I use Rust FFI?](#bllvm02-q-rust)
7. [Why is unsafe required in Rust?](#bllvm02-q-unsafe)
8. [How do I set up the library path?](#bllvm02-q-libpath)
9. [What is the Prolog %% escaping fix?](#bllvm02-q-escape)
10. [What is the overall architecture?](#bllvm02-q-architecture)

---

<a id="bllvm02-q-export"></a>
## Q1: How do I export predicates for C ABI?

**Question:** How do I compile a predicate with C ABI export?

**Answer:** Use the `export(true)` option:

```prolog
?- compile_tail_recursion_llvm(sum/2, [export(true)], Code).
```

This generates a `dllexport` wrapper function (`sum_ext`) that can be called from C, Go, or Rust.

**See:** [02_integration_impl.md#compile_tail_recursion_llvm3-with-export](02_integration_impl.md#compile_tail_recursion_llvm3-with-export)

---

<a id="bllvm02-q-wrapper"></a>
## Q2: What does the dllexport wrapper do?

**Question:** Why is there a separate `_ext` wrapper function?

**Answer:** The wrapper provides C ABI compatibility:

```llvm
define dllexport i64 @sum_ext(i64 %n) {
    %result = call i64 @sum(i64 %n, i64 0)
    ret i64 %result
}
```

It:
- Exposes a simple public interface
- Hides accumulator parameters from callers
- Uses standard C calling convention

**See:** [02_integration_impl.md#generated-llvm-ir](02_integration_impl.md#generated-llvm-ir)

---

<a id="bllvm02-q-build"></a>
## Q3: How do I build a shared library from LLVM IR?

**Question:** What are the steps to build a shared library?

**Answer:** Three steps:

```bash
# 1. Fix Prolog escaping
sed -i 's/%%/%/g' math.ll

# 2. Compile to object file
llc -filetype=obj math.ll -o math.o

# 3. Create shared library
clang -shared math.o -o libmath.so
```

**See:** [02_integration_impl.md#building-shared-libraries](02_integration_impl.md#building-shared-libraries)

---

<a id="bllvm02-q-c"></a>
## Q4: How do I call exported functions from C?

**Question:** How do I use the library from a C program?

**Answer:** Create a header and link the library:

```c
// prolog_math.h
#pragma once
#include <stdint.h>
int64_t sum_ext(int64_t n);

// main.c
#include <stdio.h>
#include "prolog_math.h"

int main() {
    printf("sum(10) = %ld\n", sum_ext(10));
    return 0;
}
```

Compile: `gcc -L. -lmath main.c -o main`

**See:** [02_integration_impl.md#c-integration](02_integration_impl.md#c-integration)

---

<a id="bllvm02-q-go"></a>
## Q5: How does Go integrate via cgo?

**Question:** How do I call LLVM-compiled predicates from Go?

**Answer:** Use cgo with special comments:

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

Build: `CGO_ENABLED=1 go build`

**See:** [02_integration_impl.md#go-integration-cgo](02_integration_impl.md#go-integration-cgo)

---

<a id="bllvm02-q-rust"></a>
## Q6: How do I use Rust FFI?

**Question:** How do I call LLVM-compiled predicates from Rust?

**Answer:** Declare extern functions and link:

```rust
extern "C" {
    fn sum_ext(n: i64) -> i64;
}

fn main() {
    let result = unsafe { sum_ext(10) };
    println!("sum(10) = {}", result);
}
```

Configure linking in `build.rs` or Cargo config.

**See:** [02_integration_impl.md#rust-integration-ffi](02_integration_impl.md#rust-integration-ffi)

---

<a id="bllvm02-q-unsafe"></a>
## Q7: Why is unsafe required in Rust?

**Question:** Why must FFI calls be wrapped in `unsafe`?

**Answer:** Rust can't verify:

- The external function exists
- The signature is correct
- The function won't cause undefined behavior

`unsafe` tells Rust the programmer takes responsibility:

```rust
let result = unsafe { sum_ext(10) };  // Programmer guarantees safety
```

**See:** [02_integration_impl.md#rust-ffi-notes](02_integration_impl.md#rust-ffi-notes)

---

<a id="bllvm02-q-libpath"></a>
## Q8: How do I set up the library path?

**Question:** How do I tell the runtime where to find the shared library?

**Answer:** Platform-specific:

```bash
# Linux
LD_LIBRARY_PATH=. ./program

# macOS
DYLD_LIBRARY_PATH=. ./program

# Windows
# Place DLL in same directory or system PATH
```

Or install the library to a system path.

**See:** [02_integration_impl.md#platform-specific-notes](02_integration_impl.md#platform-specific-notes)

---

<a id="bllvm02-q-escape"></a>
## Q9: What is the Prolog %% escaping fix?

**Question:** Why is `sed 's/%%/%/g'` needed?

**Answer:** Prolog uses `%%` for comments, which escapes `%` in format strings:

```bash
# Prolog outputs: %%i64 instead of %i64
sed -i 's/%%/%/g' math.ll
```

This converts `%%` back to single `%` for valid LLVM IR.

**See:** [02_integration_impl.md#generate-llvm-ir](02_integration_impl.md#generate-llvm-ir)

---

<a id="bllvm02-q-architecture"></a>
## Q10: What is the overall architecture?

**Question:** How do the pieces fit together?

**Answer:** The compilation pipeline:

```
Prolog → LLVM IR → Object File → Shared Library
                                       ↓
                        ┌──────────────┼──────────────┐
                        ↓              ↓              ↓
                   C Program      Go (cgo)       Rust (FFI)
```

1. Prolog predicates compile to LLVM IR
2. LLVM tools create a shared library
3. Any language with C FFI can call the functions

**See:** [02_integration_impl.md#architecture-diagram](02_integration_impl.md#architecture-diagram)
