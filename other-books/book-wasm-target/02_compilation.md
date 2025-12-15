# Chapter 2: Compiling Prolog to WASM

Step-by-step guide to compile Prolog predicates to WebAssembly.

## API

```prolog
compile_wasm_module(+Functions, +Options, -LLVMCode)
```

### Supported Function Types

| Type | Description |
|------|-------------|
| `tail_recursion` | O(1) stack via tail calls |
| `factorial` | Simple recursion |
| `linear_recursion` | Without memoization |

## Example: Sum Function

```prolog
?- use_module('src/unifyweaver/targets/llvm_target').

?- compile_wasm_module(
       [func(sum, 2, tail_recursion),
        func(factorial, 1, factorial)],
       [module_name(prolog_wasm)],
       Code),
   write_llvm_program(Code, 'prolog_wasm.ll').
```

## Generated LLVM IR

```llvm
target triple = "wasm32-unknown-unknown"

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

## Build Commands

```bash
# Step 1: Fix Prolog escaping
sed -i 's/%%/%/g' prolog_wasm.ll

# Step 2: Compile to WASM object
llc -march=wasm32 -filetype=obj prolog_wasm.ll -o prolog_wasm.o

# Step 3: Link to WASM module
wasm-ld --no-entry --export-all prolog_wasm.o -o prolog_wasm.wasm

# Alternative if wasm-ld not in PATH:
wasm-ld-10 --no-entry --export-all prolog_wasm.o -o prolog_wasm.wasm
```

## Verify

```bash
file prolog_wasm.wasm
# prolog_wasm.wasm: WebAssembly (wasm) binary module version 0x1 (MVP)

ls -la prolog_wasm.wasm
# 376 bytes
```

## Next Steps

- [Chapter 3: JavaScript Integration](03_javascript.md)
