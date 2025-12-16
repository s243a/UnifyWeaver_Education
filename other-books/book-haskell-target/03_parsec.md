# Chapter 3: Parser Combinators (Parsec)

Compile Prolog DCGs to Haskell Parsec parser combinators.

## What is Parsec?

Parsec is Haskell's standard parser combinator library. It provides:
- Type-safe parsing with automatic error messages
- Composable parser components
- Backtracking with `try`

## DCG to Parsec

### Single Rule

```prolog
?- use_module('src/unifyweaver/targets/haskell_target').

?- compile_dcg_to_parsec(
       (digit --> [d]),
       [module_name('DigitParser')],
       Code),
   write_haskell_module(Code, 'DigitParser.hs').
```

**Generated Haskell:**

```haskell
{-# LANGUAGE OverloadedStrings #-}
module DigitParser where

import Text.Parsec
import Text.Parsec.String (Parser)

digit :: Parser String
digit = string "d"
```

### Sequence

```prolog
?- compile_dcg_to_parsec(
       (ab --> [a], [b]),
       [module_name('ABParser')],
       Code).
```

```haskell
ab :: Parser String
ab = (string "a" *> string "b")
```

## Multiple Rules (Grammar)

```prolog
?- compile_grammar_to_parsec(
       [(expr --> term),
        (term --> factor),
        (factor --> [x])],
       [module_name('ExprParser'), start_symbol(expr)],
       Code).
```

**Generated Haskell:**

```haskell
module ExprParser 
    ( parseexpr
    , expr
    ) where

import Text.Parsec
import Text.Parsec.String (Parser)

parseexpr :: String -> Either ParseError String
parseexpr = parse expr "(input)"

expr :: Parser String
expr = term

term :: Parser String
term = factor

factor :: Parser String
factor = string "x"
```

## DCG Pattern Mapping

| Prolog DCG | Haskell Parsec |
|------------|----------------|
| `[char]` | `string "char"` |
| `a, b` | `a *> b` |
| `a ; b` | `try a <|> b` |
| `{goal}` | `pure ()` |
| Non-terminal | Recursive call |

## Running the Parser

```bash
# Compile
ghc -O2 ExprParser.hs -o parser

# Or in GHCi
ghci ExprParser.hs
> parseexpr "x"
Right "x"
```

## Use Cases

- **DSL Parsers**: Parse domain-specific languages
- **Config Files**: Type-safe configuration parsing
- **Validation**: Input validation with error messages
- **Transpilers**: Source-to-source translation

---

**Previous**: [Chapter 2: Recursion Patterns](02_recursion.md) | [📖 Book: Haskell Target](./)
