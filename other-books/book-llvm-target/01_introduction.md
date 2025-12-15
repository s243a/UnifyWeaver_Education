# Chapter 1: Introduction to the LLVM Target

Compile Prolog predicates to LLVM Intermediate Representation for native execution.

## Overview

The LLVM target generates `.ll` files that can be compiled to:
- Native executables (`./program`)
- Shared libraries (`.so`, `.dll`)
- WebAssembly (`.wasm`)

## Installation

```bash
# Ubuntu/Debian
sudo apt install llvm clang

# Verify
llc --version
clang --version
```

## Loading the Target

```prolog
?- use_module('src/unifyweaver/targets/llvm_target').
?- init_llvm_target.
```

## Your First Compilation

```prolog
% Generate tail-recursive sum
?- compile_tail_recursion_llvm(sum/2, [], Code),
   write_llvm_program(Code, 'sum.ll').
```

## Compilation Steps

```bash
# 1. Fix Prolog escaping
sed -i 's/%%/%/g' sum.ll

# 2. Compile to object file
llc -filetype=obj sum.ll -o sum.o

# 3. Link to executable
clang sum.o -o sum

# 4. Run
./sum
```

## Generated Code Structure

```llvm
; Header
source_filename = "sum.ll"
target triple = "x86_64-pc-linux-gnu"

; Function with musttail optimization
define i64 @sum(i64 %n, i64 %acc) {
entry:
  %cmp = icmp sle i64 %n, 0
  br i1 %cmp, label %base, label %recurse

base:
  ret i64 %acc

recurse:
  %n1 = sub i64 %n, 1
  %acc1 = add i64 %acc, %n
  %result = musttail call i64 @sum(i64 %n1, i64 %acc1)
  ret i64 %result
}
```

## Key LLVM IR Concepts

| Concept | Example |
|---------|---------|
| Type | `i64` (64-bit int), `i1` (bool) |
| Comparison | `icmp sle i64 %n, 0` |
| Branch | `br i1 %cmp, label %a, label %b` |
| Tail call | `musttail call i64 @fn(...)` |

---

**→** [Next: Chapter 2: Integration](02_integration.md)
