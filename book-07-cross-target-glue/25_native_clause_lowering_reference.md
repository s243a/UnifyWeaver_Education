<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 25: Native Clause Lowering — Cross-Target Reference

This chapter is a comprehensive reference showing how the same Prolog predicate compiles to all 28 targets via native clause body lowering. It complements the target-specific chapters in individual books.

## The Source Predicate

Every example in this chapter starts from this Prolog:

```prolog
classify(X, small) :- X > 0, X < 10.
classify(X, large) :- X >= 10.
```

Two clauses, each with guard conditions and a constant output. UnifyWeaver detects the guard/output pattern and generates idiomatic code for each target language.

## All 28 Targets

### Systems Languages

#### C

```c
const char* classify(int arg1) {
    if (arg1 > 0 && arg1 < 10) {
        return "small";
    } else if (arg1 >= 10) {
        return "large";
    } else {
        fprintf(stderr, "No matching clause for classify/2\n");
        exit(1);
    }
}
```

#### C++ (same pattern as C with `std::string`)

#### Rust

```rust
fn classify(arg1: i64) -> &'static str {
    if arg1 > 0 && arg1 < 10 {
        "small"
    } else if arg1 >= 10 {
        "large"
    } else {
        panic!("No matching clause for classify/2")
    }
}
```

Rust uses expression-based returns — no `return` keyword needed.

#### Go

```go
func classify(arg1 interface{}) interface{} {
    if arg1 > 0 && arg1 < 10 {
        return "small"
    } else if arg1 >= 10 {
        return "large"
    } else {
        panic("No matching clause for classify/2")
    }
}
```

### JVM Languages

#### Java

```java
public static String classify(int arg1) {
    if (arg1 > 0 && arg1 < 10) {
        return "small";
    } else if (arg1 >= 10) {
        return "large";
    } else {
        throw new RuntimeException("No matching clause for classify/2");
    }
}
```

#### Kotlin

```kotlin
fun classify(arg1: Long): String {
    if (arg1 > 0 && arg1 < 10) {
        return "small"
    }
    if (arg1 >= 10) {
        return "large"
    }
    throw RuntimeException("No matching clause for classify/2")
}
```

#### Scala, Jython — follow Java/Python patterns respectively.

#### Jamaica / Krakatau — JVM assembly with shared `jvm_bytecode.pl` layer.

### Scripting Languages

#### Python

```python
def classify(arg1):
    if arg1 > 0 and arg1 < 10:
        return "small"
    elif arg1 >= 10:
        return "large"
    else:
        raise ValueError("No matching clause for classify/2")
```

#### Ruby

```ruby
def classify(arg1)
  if arg1 > 0 && arg1 < 10
    "small"
  elsif arg1 >= 10
    "large"
  else
    raise "No matching clause for classify/2"
  end
end
```

Ruby uses implicit returns — the last expression in each branch is the return value.

#### Perl

```perl
sub classify {
    my ($arg1) = @_;
    if ($arg1 > 0 && $arg1 < 10) {
        return "small";
    } elsif ($arg1 >= 10) {
        return "large";
    } else {
        die "No matching clause for classify/2\n";
    }
}
```

#### Lua

```lua
function classify(arg1)
    if arg1 > 0 and arg1 < 10 then
        return "small"
    elseif arg1 >= 10 then
        return "large"
    else
        error("No matching clause for classify/2")
    end
end
```

#### AWK

```awk
function classify(arg1) {
    if (arg1 > 0 && arg1 < 10) return "small"
    else if (arg1 >= 10) return "large"
    print "No matching clause for classify/2" > "/dev/stderr"
    exit 1
}
```

#### Bash

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

### Functional Languages

#### Haskell

```haskell
classify :: Int -> String
classify arg1
  | arg1 > 0 && arg1 < 10 = "small"
  | arg1 >= 10 = "large"
  | otherwise = error "No matching clause for classify/2"
```

Haskell uses guard syntax (`|`) — a natural fit for Prolog's multi-clause pattern.

#### F#

```fsharp
let classify arg1 =
    if arg1 > 0 && arg1 < 10 then
        "small"
    elif arg1 >= 10 then
        "large"
    else
        failwith "No matching clause for classify/2"
```

#### Elixir

```elixir
def classify(arg1) do
  cond do
    arg1 > 0 and arg1 < 10 -> "small"
    arg1 >= 10 -> "large"
    true -> raise "No matching clause for classify/2"
  end
end
```

Elixir uses `cond do` — its multi-branch conditional.

#### Clojure

```clojure
(defn classify [arg1]
  (cond
    (and (> arg1 0) (< arg1 10)) "small"
    (>= arg1 10) "large"
    :else (throw (ex-info "No matching clause for classify/2" {}))))
```

Clojure uses `cond` with S-expression prefix notation.

### Web / TypeScript

```typescript
function classify(arg1: number): string {
    if (arg1 > 0 && arg1 < 10) {
        return "small";
    } else if (arg1 >= 10) {
        return "large";
    } else {
        throw new Error("No matching clause for classify/2");
    }
}
```

### .NET Languages

#### C#

```csharp
public static object Classify(object arg1)
{
    if (Convert.ToInt32(arg1) > Convert.ToInt32(0) && Convert.ToInt32(arg1) < Convert.ToInt32(10))
    {
        return "small";
    }
    else if (Convert.ToInt32(arg1) >= Convert.ToInt32(10))
    {
        return "large";
    }
    else
    {
        throw new ArgumentException("No matching clause for classify");
    }
}
```

C# uses PascalCase naming and `Convert.ToInt32()` for type-safe comparisons.

#### VB.NET

```vbnet
Function Classify(arg1 As Integer) As String
    If arg1 > 0 AndAlso arg1 < 10 Then
        Return "small"
    ElseIf arg1 >= 10 Then
        Return "large"
    Else
        Throw New ArgumentException("No matching clause for classify/2")
    End If
End Function
```

#### PowerShell

```powershell
function classify($arg1) {
    if ($arg1 -gt 0 -and $arg1 -lt 10) {
        return "small"
    } elseif ($arg1 -ge 10) {
        return "large"
    } else {
        Write-Error "No matching clause for classify/2"
    }
}
```

### Assembly / Low-Level

#### WAT (WebAssembly Text)

WAT uses structured control flow with `block`/`br_if`:

```wat
(func $classify (param $arg1 i64) (result i64)
  (if (i64.and (i64.gt_s (local.get $arg1) (i64.const 0))
               (i64.lt_s (local.get $arg1) (i64.const 10)))
    (then (i64.const 0))  ;; "small" index
    (else (if (i64.ge_s (local.get $arg1) (i64.const 10))
      (then (i64.const 1))  ;; "large" index
      (else (unreachable))))))
```

#### Jamaica / Krakatau (JVM Assembly)

JVM bytecode via the shared `jvm_bytecode.pl` layer with `if_icmpgt`/`if_icmplt` guard chains.

### Data Languages

#### SQL (CASE WHEN)

```sql
CASE WHEN arg1 > 0 AND arg1 < 10 THEN 'small'
     WHEN arg1 >= 10 THEN 'large'
     ELSE NULL END
```

#### SQL (PL/pgSQL)

```sql
IF arg1 < 50 THEN RETURN 'low';
ELSIF arg1 >= 50 AND arg1 < 80 THEN RETURN 'mid';
ELSIF arg1 >= 80 THEN RETURN 'high';
END IF;
```

#### R

```r
classify <- function(arg1) {
    if (arg1 > 0 && arg1 < 10) {
        return("small")
    } else if (arg1 >= 10) {
        return("large")
    } else {
        stop("No matching clause for classify/2")
    }
}
```

#### TypR

```typr
let classify <- fn(arg1): Any {
    if (@{ arg1 > 0 && arg1 < 10 }@) {
        "small"
    } else if (@{ arg1 >= 10 }@) {
        "large"
    } else {
        stop("No matching clause for classify")
    }
};
```

TypR embeds R expressions in `@{ ... }@` blocks.

## Cross-Target Comparison

### Conditional Syntax

| Language Family | If | Else If | Else | End |
|----------------|-----|---------|------|-----|
| C/C++/Java/C# | `if (cond)` | `else if (cond)` | `else` | `}` |
| Rust | `if cond` | `else if cond` | `else` | `}` |
| Python | `if cond:` | `elif cond:` | `else:` | (indent) |
| Ruby | `if cond` | `elsif cond` | `else` | `end` |
| Perl | `if (cond)` | `elsif (cond)` | `else` | `}` |
| Lua | `if cond then` | `elseif cond then` | `else` | `end` |
| Bash | `if (( c )); then` | `elif (( c )); then` | `else` | `fi` |
| Haskell | guard `\|` | guard `\|` | `otherwise` | (indent) |
| Elixir | `cond do` | next clause | `true ->` | `end` |
| Clojure | `(cond` | next pair | `:else` | `)` |
| SQL | `CASE WHEN` | `WHEN` | `ELSE` | `END` |
| PowerShell | `if ($c)` | `elseif ($c)` | `else` | `}` |
| F# | `if c then` | `elif c then` | `else` | (indent) |
| AWK | `if (c)` | `else if (c)` | — | `}` |
| VB.NET | `If c Then` | `ElseIf c Then` | `Else` | `End If` |

### Error Handling

| Pattern | Languages |
|---------|-----------|
| `panic!` / `panic()` | Rust, Go |
| `throw new Exception` | Java, C#, TypeScript, Kotlin, VB.NET |
| `raise` | Python, Ruby, Elixir |
| `die` | Perl |
| `error()` | Lua, Haskell |
| `failwith` | F# |
| `exit(1)` | C, AWK |
| `return 1` + stderr | Bash |
| `Write-Error` | PowerShell |
| `RAISE EXCEPTION` | SQL (PL/pgSQL) |
| `NULL` | SQL (CASE) |
| `stop()` | R, TypR |
| `unreachable` | WAT |

### Return Mechanism

| Pattern | Languages |
|---------|-----------|
| `return val` | Java, C, Go, Python, Perl, Lua, Kotlin, TypeScript, C# |
| Expression (implicit) | Rust, Ruby, Haskell, F#, Elixir, Clojure |
| `echo` (stdout) | Bash |
| `RETURN` | SQL (PL/pgSQL) |
| `THEN val` | SQL (CASE) |

## Summary

- **All 28 targets** compile the same Prolog predicate to idiomatic code
- Each uses its language's native conditional syntax
- Guard conditions map to boolean expressions with language-specific operators
- Error handling follows each language's conventions
- The shared `clause_body_analysis` module provides the analysis; targets provide the syntax

---

## Navigation

**←** [Previous: Chapter 24: Composability Patterns](24_composability_patterns) | [Book 7: Cross-Target Glue](./)
