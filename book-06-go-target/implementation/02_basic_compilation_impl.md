<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Basic Compilation - Implementation Details

This document provides function-level documentation for Go target compilation.

**Source**: `src/unifyweaver/targets/go_target.pl`

---

## compile_predicate_to_go/3

Compiles a Prolog predicate to Go code with configurable options.

### Signature

```prolog
compile_predicate_to_go(+Predicate/Arity, +Options, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicate/Arity` | `atom/integer` | The predicate name and arity to compile |
| `Options` | `list` | Compilation options (see below) |
| `Code` | `string` | Generated Go source code |

### Options

| Option | Values | Default | Description |
|--------|--------|---------|-------------|
| `field_delimiter(D)` | `colon`, `tab`, `comma`, `pipe` | `colon` | Field separator in output |
| `unique(B)` | `true`, `false` | `true` | Enable deduplication |

### Algorithm

1. **Collect clauses**: Gather all clauses for `Predicate/Arity`
2. **Classify predicate**: Determine if facts-only or has rules
3. **Generate code**:
   - Facts → Map-based lookup (`map[string]bool`)
   - Rules → Stream processor (stdin → stdout)
4. **Apply options**: Insert delimiter and uniqueness logic

### Example: Facts Compilation

```prolog
% Input facts
user(john, 25).
user(jane, 30).

% Compile
?- compile_predicate_to_go(user/2, [], Code).
```

**Generated Go:**

```go
package main
import "fmt"

func main() {
    facts := map[string]bool{
        "john:25": true,
        "jane:30": true,
    }
    for k := range facts {
        fmt.Println(k)
    }
}
```

### Example: Rules Compilation

```prolog
% Input rule
swap(Y, X) :- input(X, Y).

% Compile
?- compile_predicate_to_go(swap/2, [], Code).
```

**Generated Go:**

```go
package main
import (
    "bufio"
    "fmt"
    "os"
    "strings"
)

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    for scanner.Scan() {
        parts := strings.Split(scanner.Text(), ":")
        if len(parts) >= 2 {
            fmt.Println(parts[1] + ":" + parts[0])
        }
    }
}
```

### Edge Cases

- **Empty predicate**: Returns minimal Go program with no output
- **Mixed facts/rules**: Rules take precedence; facts become initial data
- **Special characters**: Escaped in string literals

---

## compile_facts_to_go/3

Generates struct-based Go code for facts with helper functions.

### Signature

```prolog
compile_facts_to_go(+Predicate, +Arity, -Code)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Predicate` | `atom` | The predicate name |
| `Arity` | `integer` | Number of arguments |
| `Code` | `string` | Generated Go source code |

### Generated Structure

For a predicate `foo/N`, generates:

1. **Struct**: `type FOO struct { Arg1, Arg2, ..., ArgN string }`
2. **GetAllFOO()**: Returns `[]FOO` slice of all facts
3. **StreamFOO(fn)**: Iterator with callback
4. **ContainsFOO(target)**: Membership test

### Algorithm

1. **Collect facts**: Query all `Predicate/Arity` facts
2. **Generate struct**: Create Go struct with `ArgN` fields
3. **Generate slice literal**: Convert facts to struct initializers
4. **Generate helpers**: Create GetAll, Stream, Contains functions

### Example

```prolog
user(john, 25).
user(jane, 30).

?- compile_facts_to_go(user, 2, Code).
```

**Generated Go:**

```go
type USER struct {
    Arg1 string
    Arg2 string
}

func GetAllUSER() []USER {
    return []USER{
        {Arg1: "john", Arg2: "25"},
        {Arg1: "jane", Arg2: "30"},
    }
}

func StreamUSER(fn func(USER)) {
    for _, fact := range GetAllUSER() {
        fn(fact)
    }
}

func ContainsUSER(target USER) bool {
    for _, fact := range GetAllUSER() {
        if fact == target {
            return true
        }
    }
    return false
}
```

### Benefits

- **Type safety**: Go compiler catches field access errors
- **IDE support**: Autocompletion for struct fields
- **Reusability**: Helper functions for common operations
- **Integration**: Easy to embed in larger Go programs

---

## write_go_program/2

Writes generated Go code to a file.

### Signature

```prolog
write_go_program(+Code, +Filename)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `Code` | `string` | Go source code to write |
| `Filename` | `atom` | Output file path |

### Algorithm

1. Open file for writing
2. Write code string
3. Close file handle

### Example

```prolog
?- compile_predicate_to_go(user/2, [], Code),
   write_go_program(Code, 'user.go').
```

---

## Compilation Workflow

### End-to-End Process

```
┌─────────────────┐
│  Prolog Source  │
│   (facts.pl)    │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│ compile_predicate│
│   _to_go/3      │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Go Source Code │
│   (user.go)     │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│   go build      │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Executable     │
│   (./user)      │
└─────────────────┘
```

### Build Commands

```bash
# Compile Prolog to Go
swipl -g compile_facts -t halt facts.pl

# Build Go binary
go build -o user user.go

# Run
./user
```

---

## Map-Based vs Struct-Based Export

| Feature | Map-Based (`compile_predicate_to_go`) | Struct-Based (`compile_facts_to_go`) |
|---------|---------------------------------------|--------------------------------------|
| Lookup | O(1) hash lookup | O(n) linear scan |
| Type safety | String keys only | Typed struct fields |
| Memory | Hash table overhead | Slice + structs |
| Use case | Membership tests | Data processing |
| IDE support | Limited | Full autocompletion |

### When to Use Each

**Map-Based**:
- Fast membership testing (`if facts[key]`)
- Large datasets where O(1) lookup matters
- Simple key-value relationships

**Struct-Based**:
- Processing/transforming data
- Integration with typed Go code
- When field names matter

---

## Delimiter Options

### Available Delimiters

| Option | Character | Example Output |
|--------|-----------|----------------|
| `colon` | `:` | `john:25` |
| `tab` | `\t` | `john	25` |
| `comma` | `,` | `john,25` |
| `pipe` | `\|` | `john|25` |

### Setting Delimiter

```prolog
compile_predicate_to_go(user/2, [field_delimiter(tab)], Code).
```

### Generated Code Difference

```go
// With colon (default)
fmt.Println(parts[0] + ":" + parts[1])

// With tab
fmt.Println(parts[0] + "\t" + parts[1])
```

---

## Related Documentation

- [Book 6 Chapter 1: Introduction](../01_introduction.md)
- [Book 6 Chapter 3: Advanced Features](../03_advanced_features.md)
- [Go Target Source](../../../../src/unifyweaver/targets/go_target.pl)
