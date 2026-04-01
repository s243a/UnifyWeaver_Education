<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Native Clause Body Lowering

Write natural Prolog predicates and let UnifyWeaver compile them into AWK functions with `if`/`else if`/`else` chains.

## How It Works

```prolog
classify(X, small) :- X > 0, X < 10.
classify(X, large) :- X >= 10.
```

Generates:

```awk
function classify(arg1) {
    if (arg1 > 0 && arg1 < 10) return "small"
    else if (arg1 >= 10) return "large"
    print "No matching clause for classify/2" > "/dev/stderr"
    exit 1
}

BEGIN {
    if (ARGC > 1) {
        print classify(ARGV[1])
        exit
    }
}
```

## Basic Examples

### Arithmetic

```prolog
double(X, R) :- R is X * 2.
```

```awk
function double(arg1) {
    return (arg1 * 2)
}
```

### Three-Way Classification

```prolog
grade(X, low)  :- X < 50.
grade(X, mid)  :- X >= 50, X < 80.
grade(X, high) :- X >= 80.
```

```awk
function grade(arg1) {
    if (arg1 < 50) return "low"
    else if (arg1 >= 50 && arg1 < 80) return "mid"
    else if (arg1 >= 80) return "high"
    print "No matching clause for grade/2" > "/dev/stderr"
    exit 1
}
```

## AWK-Specific Syntax

| Prolog | AWK |
|--------|-----|
| `X > 0, X < 10` | `arg1 > 0 && arg1 < 10` |
| `X =:= 0` | `arg1 == 0` |
| `X =\= 0` | `arg1 != 0` |
| `R is X mod 2` | `arg1 % 2` |
| No match | `print "..." > "/dev/stderr"; exit 1` |
| Entry point | `BEGIN { ... }` |

## Running Generated Code

```bash
# Save and run
echo 'function classify(arg1) { ... }
BEGIN { print classify(5) }' > classify.awk

awk -f classify.awk
# Output: small
```

## Summary

- Multi-clause predicates compile to AWK `if`/`else if` chains
- One-line returns: `if (cond) return val`
- Error output via `> "/dev/stderr"`
- Entry point in `BEGIN` block with `ARGC`/`ARGV`

---

## Navigation

[Book: AWK Target](./)
