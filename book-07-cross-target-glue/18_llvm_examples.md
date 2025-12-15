# Chapter 18: LLVM Integration Examples

Working examples demonstrating Go and Rust calling LLVM-compiled Prolog.

## Quick Start

```bash
cd examples/llvm-ffi
./build.sh  # Generates lib, builds Go/Rust, runs tests
```

## What Gets Built

```
examples/llvm-ffi/
├── libprolog_math.so      # LLVM-compiled Prolog (sum, factorial)
├── prolog_math.h          # C header for FFI
├── go/llvm-ffi-test       # Go binary
└── rust/target/release/llvm-ffi-test  # Rust binary
```

## Test Results

Both languages call the **exact same** LLVM-compiled functions:

| Function | Input | Expected | Go | Rust |
|----------|-------|----------|-----|------|
| `sum` | 10 | 55 | ✅ | ✅ |
| `sum` | 100 | 5050 | ✅ | ✅ |
| `factorial` | 5 | 120 | ✅ | ✅ |
| `factorial` | 10 | 3628800 | ✅ | ✅ |

## Go Code Highlights

```go
/*
#cgo LDFLAGS: -L.. -lprolog_math
#include "../prolog_math.h"
*/
import "C"

func Sum(n int64) int64 {
    return int64(C.sum(C.int64_t(n)))
}
```

**Key points:**
- `#cgo LDFLAGS` specifies library location
- `C.sum()` calls the LLVM-compiled function directly
- Type conversion: Go `int64` ↔ C `int64_t`

## Rust Code Highlights

```rust
mod ffi {
    extern "C" {
        pub fn sum(n: i64) -> i64;
    }
}

pub fn sum(n: i64) -> i64 {
    unsafe { ffi::sum(n) }
}
```

**Key points:**
- `extern "C"` block declares C ABI functions
- `unsafe` required for FFI calls
- `build.rs` configures library linking

## Performance Characteristics

| Metric | Value |
|--------|-------|
| Call overhead | < 1 μs |
| Memory copy | None (direct call) |
| Thread safety | Yes (stateless functions) |
| Stack usage | O(1) via musttail |

## Extending with New Functions

1. Define in Prolog:
```prolog
compile_shared_library_llvm([
    func(sum, 2, tail_recursion),
    func(factorial, 1, factorial),
    func(fib, 2, linear_recursion)  % Add new function
], [library_name(prolog_math)], Code).
```

2. Update `prolog_math.h`:
```c
int64_t fib(int64_t n);
```

3. Add Go/Rust wrappers

## Troubleshooting

### "library not found"
```bash
# Set library path
export LD_LIBRARY_PATH=/path/to/libprolog_math.so:$LD_LIBRARY_PATH
```

### Go: "undefined reference"
```bash
CGO_ENABLED=1 go build
```

### Rust: build.rs not running
```bash
cargo clean && cargo build
```

---

**←** [Previous: LLVM FFI](17_llvm_ffi.md) | [📖 Book: Cross-Target Glue](./)
