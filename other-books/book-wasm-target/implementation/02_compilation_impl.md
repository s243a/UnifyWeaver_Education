<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Compilation - Implementation Details

This document provides function-level documentation for WebAssembly compilation.

**Source**: `src/unifyweaver/targets/llvm_target.pl`

---

## Overview: Compilation Pipeline

```
Prolog → compile_wasm_module/3 → LLVM IR → llc → .o → wasm-ld → .wasm
```

The WASM target generates LLVM IR that compiles to WebAssembly.

---

## compile_wasm_module/3

Compiles predicates to LLVM IR targeting WASM.

### Signature

```prolog
compile_wasm_module(+Functions, +Options, -LLVMCode)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Functions` | `list` | List of `func(Name, Arity, Type)` |
| `Options` | `list` | Compilation options |
| `LLVMCode` | `string` | Generated LLVM IR |

### Function Types

| Type | Description |
|------|-------------|
| `tail_recursion` | O(1) stack via tail calls |
| `factorial` | Simple recursion |
| `linear_recursion` | Without memoization |

### Example

```prolog
?- compile_wasm_module(
       [func(sum, 2, tail_recursion),
        func(factorial, 1, factorial)],
       [module_name(prolog_wasm)],
       Code),
   write_llvm_program(Code, 'prolog_wasm.ll').
```

---

## Generated LLVM IR

### Target Triple

```llvm
target triple = "wasm32-unknown-unknown"
```

### Tail Recursion Pattern

```llvm
define i32 @sum_impl(i32 %n, i32 %acc) {
entry:
  %cmp = icmp sle i32 %n, 0
  br i1 %cmp, label %base, label %recurse
base:
  ret i32 %acc
recurse:
  %n1 = sub i32 %n, 1
  %acc1 = add i32 %acc, %n
  %result = tail call i32 @sum_impl(i32 %n1, i32 %acc1)
  ret i32 %result
}

define i32 @sum(i32 %n) {
  %result = call i32 @sum_impl(i32 %n, i32 0)
  ret i32 %result
}
```

### Key LLVM Instructions

| Instruction | Purpose |
|-------------|---------|
| `icmp sle` | Signed less-or-equal compare |
| `br i1` | Conditional branch |
| `tail call` | Tail call optimization hint |
| `sub`, `add` | Arithmetic operations |

---

## Build Process

### Step 1: Fix Prolog Escaping

```bash
sed -i 's/%%/%/g' prolog_wasm.ll
```

Prolog escapes `%` as `%%` which needs to be fixed for LLVM.

### Step 2: Compile to Object

```bash
llc -march=wasm32 -filetype=obj prolog_wasm.ll -o prolog_wasm.o
```

### Step 3: Link to WASM

```bash
wasm-ld --no-entry --export-all prolog_wasm.o -o prolog_wasm.wasm
```

### Linker Options

| Option | Purpose |
|--------|---------|
| `--no-entry` | No `_start` function |
| `--export-all` | Export all functions |

---

## Verification

```bash
file prolog_wasm.wasm
# prolog_wasm.wasm: WebAssembly (wasm) binary module version 0x1 (MVP)

ls -la prolog_wasm.wasm
# ~376 bytes for simple module
```

---

## Factorial Pattern

### Prolog

```prolog
factorial(0, 1).
factorial(N, R) :- N > 0, N1 is N-1, factorial(N1, R1), R is N * R1.
```

### Generated LLVM IR

```llvm
define i32 @factorial(i32 %n) {
entry:
  %cmp = icmp sle i32 %n, 1
  br i1 %cmp, label %base, label %recurse
base:
  ret i32 1
recurse:
  %n1 = sub i32 %n, 1
  %r1 = call i32 @factorial(i32 %n1)
  %result = mul i32 %n, %r1
  ret i32 %result
}
```

---

## Related Documentation

- [Book WASM Chapter 3: JavaScript Integration](../03_javascript.md)
- [Book WASM Chapter 4: String Support](../04_string_support.md)
- [LLVM Target Source](../../../../../src/unifyweaver/targets/llvm_target.pl)
