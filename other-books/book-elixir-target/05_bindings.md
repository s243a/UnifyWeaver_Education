# Chapter 5: Bindings

Bindings map Prolog built-in predicates to their Elixir equivalents. They are
registered via `declare_binding/6` in `elixir_bindings.pl`.

## Binding Architecture

Each binding declares:
- **Prolog predicate** — the source-side functor/arity
- **Elixir template** — a format string with `~w` placeholders
- **Input/Output types** — for type checking
- **Properties** — `pure`, `deterministic`, `effect(io)`, etc.

```prolog
declare_binding(elixir, add/3, '~w = ~w + ~w',
    [int, int], [int], [pure, deterministic, total, pattern(arithmetic)]).
```

## Arithmetic Bindings

| Prolog | Elixir Template | Example |
|--------|-----------------|---------|
| `add/3` | `~w = ~w + ~w` | `result = 10 + 20` |
| `subtract/3` | `~w = ~w - ~w` | `result = 30 - 7` |
| `multiply/3` | `~w = ~w * ~w` | `result = 6 * 7` |
| `divide/3` | `~w = div(~w, ~w)` | `result = div(100, 3)` |
| `mod/3` | `~w = rem(~w, ~w)` | `result = rem(100, 3)` |

Note: Elixir uses `div/2` for integer division and `rem/2` for remainder,
matching Erlang's conventions rather than the `/` operator (which returns floats).

### Code Generation Path

When the body translator encounters `is/2`, it generates Elixir assignment:

```prolog
% Prolog rule
compute(X, Y, R) :- R is X + Y.
```

```elixir
# Generated Elixir
def compute(arg1, arg2, arg3) do
  try do
    arg3 = (arg1 + arg2)
    {:ok, [arg1, arg2, arg3]}
  catch
    :fail -> :fail
  end
end
```

## Comparison Bindings

| Prolog | Elixir | Notes |
|--------|--------|-------|
| `>/2` | `~w > ~w` | Strict greater-than |
| `</2` | `~w < ~w` | Strict less-than |
| `>=/2` | `~w >= ~w` | Greater or equal |
| `=</2` | `~w <= ~w` | Less or equal (note: Prolog uses `=<`, Elixir uses `<=`) |
| `=:=/2` | `~w === ~w` | Strict equality |
| `=\=/2` | `~w !== ~w` | Strict inequality |

### In Generated Code

Comparisons become guard-like checks with `throw(:fail)`:

```elixir
# X > 0 in Prolog becomes:
if not (arg1 > 0), do: throw(:fail)
```

This maps Prolog's failure semantics: if the comparison fails, the clause
fails and execution moves to the `catch` block.

## String Bindings

| Prolog | Elixir Template | Notes |
|--------|-----------------|-------|
| `atom_concat/3` | `~w = to_string(~w) <> to_string(~w)` | Concatenation via `<>` |
| `string_length/2` | `~w = String.length(to_string(~w))` | Unicode-aware length |
| `string_upper/2` | `~w = String.upcase(to_string(~w))` | Uppercase |
| `string_lower/2` | `~w = String.downcase(to_string(~w))` | Lowercase |
| `is_alpha/1` | `Regex.match?(~r/^[a-zA-Z]+$/, to_string(~w))` | Alpha check |

All string functions use `to_string/1` for robustness — it handles atoms,
integers, and existing strings.

### String Examples in Elixir

```elixir
# Concatenation
result = to_string("hello") <> to_string(" world")
# => "hello world"

# Length (Unicode-aware)
len = String.length(to_string("héllo"))
# => 5

# Case conversion
String.upcase(to_string("hello"))   # => "HELLO"
String.downcase(to_string("HELLO")) # => "hello"

# Regex test
Regex.match?(~r/^[a-zA-Z]+$/, to_string("hello"))  # => true
Regex.match?(~r/^[a-zA-Z]+$/, to_string("123"))    # => false
```

## I/O Bindings

| Prolog | Elixir | Notes |
|--------|--------|-------|
| `print/1` | `IO.puts(~w)` | Print with newline |
| `write/1` | `IO.write(~w)` | Print without newline |

Both are marked as `effect(io)` — they are not pure functions.

## Control Bindings

| Prolog | Elixir | Notes |
|--------|--------|-------|
| `true/0` | `true` | Always succeeds |
| `fail/0` | `false` | Always fails |

## Using Bindings Programmatically

```prolog
% Initialize all bindings
?- init_elixir_bindings.

% Query a specific binding
?- elixir_binding(string_length/2, Template, Inputs, Outputs, Options).
Template = '~w = String.length(to_string(~w))',
Inputs = [string],
Outputs = [int],
Options = [pure, deterministic, total, pattern(expression)].

% List all bindings
?- elixir_binding(Pred, _, _, _, _), write(Pred), nl, fail ; true.
```

## E2E Verification

All binding templates have been verified to produce valid, runnable Elixir code
with Elixir 1.20 / OTP 28:

```
=== Math Bindings ===
add: 30 ✓    subtract: 23 ✓    multiply: 42 ✓    divide: 33 ✓    mod: 1 ✓

=== Comparison Bindings ===
5 > 3: true ✓    3 < 5: true ✓    5 >= 5: true ✓
3 <= 5: true ✓   42 === 42: true ✓   42 !== 99: true ✓

=== String Bindings ===
concat: hello world ✓    length: 5 ✓    upper: HELLO ✓
lower: hello ✓    is_alpha(hello): true ✓    is_alpha(123): false ✓

=== I/O Bindings ===
IO.puts: ✓    IO.write: ✓

=== Control Bindings ===
true: true ✓    false: false ✓
```

---

**←** [Previous: Pipelines](04_pipelines.md) | [📖 Book: Elixir Target](./)
