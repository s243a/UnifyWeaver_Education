<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Compilation - Questions

Q&A companion for [02_compilation_impl.md](./02_compilation_impl.md).

---

## Question Index

1. [What does compile_wasm_module/3 do?](#bwasm02-q-compile)
2. [What function types are supported?](#bwasm02-q-types)
3. [What is the target triple?](#bwasm02-q-triple)
4. [How is tail recursion compiled?](#bwasm02-q-tail)
5. [Why fix %% escaping?](#bwasm02-q-escape)
6. [What are the build steps?](#bwasm02-q-build)
7. [What does wasm-ld do?](#bwasm02-q-link)
8. [How is factorial compiled?](#bwasm02-q-factorial)
9. [What is the output file size?](#bwasm02-q-size)
10. [How do I verify the output?](#bwasm02-q-verify)

---

## Questions and Answers

### <a id="bwasm02-q-compile"></a>Q1: What does compile_wasm_module/3 do?

**Answer**: Generates LLVM IR targeting WebAssembly:

```prolog
?- compile_wasm_module(
       [func(sum, 2, tail_recursion)],
       [module_name(prolog_wasm)],
       Code).
```

Output is `.ll` file for LLVM toolchain.

**See**: [compile_wasm_module/3](./02_compilation_impl.md#compile_wasm_module3)

---

### <a id="bwasm02-q-types"></a>Q2: What function types are supported?

**Answer**:

| Type | Description |
|------|-------------|
| `tail_recursion` | O(1) stack via tail calls |
| `factorial` | Simple recursion |
| `linear_recursion` | Without memoization |

**See**: [Function Types](./02_compilation_impl.md#function-types)

---

### <a id="bwasm02-q-triple"></a>Q3: What is the target triple?

**Answer**: `wasm32-unknown-unknown` - 32-bit WebAssembly with no specific OS.

```llvm
target triple = "wasm32-unknown-unknown"
```

**See**: [Target Triple](./02_compilation_impl.md#target-triple)

---

### <a id="bwasm02-q-tail"></a>Q4: How is tail recursion compiled?

**Answer**: LLVM IR with `tail call` hint:

```llvm
%result = tail call i32 @sum_impl(i32 %n1, i32 %acc1)
ret i32 %result
```

The `tail` keyword hints to the optimizer.

**See**: [Tail Recursion Pattern](./02_compilation_impl.md#tail-recursion-pattern)

---

### <a id="bwasm02-q-escape"></a>Q5: Why fix %% escaping?

**Answer**: Prolog escapes `%` as `%%`, but LLVM IR uses single `%`:

```bash
sed -i 's/%%/%/g' prolog_wasm.ll
```

**See**: [Step 1: Fix Prolog Escaping](./02_compilation_impl.md#step-1-fix-prolog-escaping)

---

### <a id="bwasm02-q-build"></a>Q6: What are the build steps?

**Answer**: Three steps:

```bash
# 1. Fix escaping
sed -i 's/%%/%/g' prolog_wasm.ll

# 2. Compile to object
llc -march=wasm32 -filetype=obj prolog_wasm.ll -o prolog_wasm.o

# 3. Link to WASM
wasm-ld --no-entry --export-all prolog_wasm.o -o prolog_wasm.wasm
```

**See**: [Build Process](./02_compilation_impl.md#build-process)

---

### <a id="bwasm02-q-link"></a>Q7: What does wasm-ld do?

**Answer**: Links object files to WebAssembly module:

| Option | Purpose |
|--------|---------|
| `--no-entry` | No `_start` function |
| `--export-all` | Export all functions |

**See**: [Linker Options](./02_compilation_impl.md#linker-options)

---

### <a id="bwasm02-q-factorial"></a>Q8: How is factorial compiled?

**Answer**: Direct recursion with `mul` instruction:

```llvm
%r1 = call i32 @factorial(i32 %n1)
%result = mul i32 %n, %r1
ret i32 %result
```

**See**: [Factorial Pattern](./02_compilation_impl.md#factorial-pattern)

---

### <a id="bwasm02-q-size"></a>Q9: What is the output file size?

**Answer**: ~376 bytes for simple modules. WASM is very compact.

```bash
ls -la prolog_wasm.wasm
# 376 bytes
```

**See**: [Verification](./02_compilation_impl.md#verification)

---

### <a id="bwasm02-q-verify"></a>Q10: How do I verify the output?

**Answer**: Use `file` command:

```bash
file prolog_wasm.wasm
# WebAssembly (wasm) binary module version 0x1 (MVP)
```

**See**: [Verification](./02_compilation_impl.md#verification)

---

## Summary

WASM compilation provides:
- LLVM IR generation targeting wasm32
- Tail recursion with `tail call` hint
- Three-step build: fix escaping → llc → wasm-ld
- Compact output (~400 bytes)
- MVP WebAssembly module format
