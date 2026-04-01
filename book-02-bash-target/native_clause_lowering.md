<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Native Clause Body Lowering

Write natural Prolog predicates and let UnifyWeaver compile them into Bash functions with `if`/`elif`/`else` and configurable return methods.

## How It Works

```prolog
classify(X, small) :- X > 0, X < 10.
classify(X, large) :- X >= 10.
```

Generates:

```bash
classify() {
    local arg1="$1"
    if (( arg1 > 0 )) && (( arg1 < 10 )); then
        echo "small"
    elif (( arg1 >= 10 )); then
        echo "large"
    else
        echo "Error: No matching clause for classify" >&2
        return 1
    fi
}
```

## Basic Examples

### Three-Way Classification

```prolog
grade(X, low)  :- X < 50.
grade(X, mid)  :- X >= 50, X < 80.
grade(X, high) :- X >= 80.
```

```bash
grade() {
    local arg1="$1"
    if (( arg1 < 50 )); then
        echo "low"
    elif (( arg1 >= 50 )) && (( arg1 < 80 )); then
        echo "mid"
    elif (( arg1 >= 80 )); then
        echo "high"
    else
        echo "Error: No matching clause for grade" >&2
        return 1
    fi
}
```

### Arithmetic

```prolog
double(X, R) :- R is X * 2.
```

```bash
double() {
    local arg1="$1"
    echo $(( (arg1 * 2) ))
}
```

## If-Then-Else

```prolog
abs_val(X, R) :- (X >= 0 -> R = X ; R is -X).
```

```bash
abs_val() {
    local arg1="$1"
    echo $(if (( arg1 >= 0 )); then echo "$arg1"; else echo "$(( (-arg1) ))"; fi)
}
```

Bash uses a subshell `$(if ...; then echo ...; else echo ...; fi)` to capture the conditional value.

## Return Methods

Bash has no return values — functions return exit codes (0-255). UnifyWeaver provides four configurable return methods:

### Echo (Default)

Streaming-compatible — caller captures with `$()`:

```prolog
?- compile_predicate_to_bash(classify/2, [], Code).
```

```bash
# Usage: result=$(classify 5)
classify() {
    local arg1="$1"
    if (( arg1 > 0 )) && (( arg1 < 10 )); then
        echo "small"    # returned via stdout
    ...
}
```

### Global Variable

```prolog
?- compile_predicate_to_bash(classify/2, [return_method(global('~w_return'))], Code).
```

```bash
# Usage: classify 5; echo "$classify_return"
classify() {
    local arg1="$1"
    if (( arg1 > 0 )) && (( arg1 < 10 )); then
        classify_return="small"    # sets global
    ...
}
```

### Nameref (Bash 4.3+)

```prolog
?- compile_predicate_to_bash(classify/2, [return_method(nameref)], Code).
```

```bash
# Usage: classify 5 my_result; echo "$my_result"
classify() {
    local arg1="$1"
    local -n __result="$2"    # nameref to caller's variable
    if (( arg1 > 0 )) && (( arg1 < 10 )); then
        __result="small"
    ...
}
```

### Associative Array

```prolog
?- compile_predicate_to_bash(classify/2, [return_method(assoc(results))], Code).
```

```bash
# Usage: classify 5; echo "${results[$*]}"
classify() {
    local arg1="$1"
    if (( arg1 > 0 )) && (( arg1 < 10 )); then
        results["$*"]="small"    # memoization-friendly
    ...
}
```

## Bash-Specific Syntax

| Prolog | Bash |
|--------|------|
| `X > 0, X < 10` | `(( arg1 > 0 )) && (( arg1 < 10 ))` |
| `X =:= 0` | `(( arg1 == 0 ))` |
| `R is X * 2` | `$(( (arg1 * 2) ))` |
| `R is abs(X)` | `$(( (arg1 < 0 ? -arg1 : arg1) ))` |
| `R is X mod 2` | `$(( (arg1 % 2) ))` |
| No match | `echo "Error: ..." >&2; return 1` |

## Verified Output

```bash
$ source <(swipl -g "
    use_module('src/unifyweaver/targets/bash_target'),
    assert(user:(classify(X, small) :- X > 0, X < 10)),
    assert(user:(classify(X, large) :- X >= 10)),
    compile_predicate_to_bash(classify/2, [], Code),
    write(Code)
" -t halt 2>/dev/null)
$ echo "$(classify 5)"
small
$ echo "$(classify 25)"
large
```

## Summary

- Multi-clause predicates compile to Bash `if`/`elif`/`else`/`fi`
- Arithmetic uses `(( ))` for conditions, `$(( ))` for expressions
- Four return methods: echo, global, nameref, associative array
- ITE uses subshell `$(if ...; then echo ...; fi)` for value capture
- Error handling writes to stderr and returns non-zero

---

## Navigation

[Book 2: Bash Target](./)
