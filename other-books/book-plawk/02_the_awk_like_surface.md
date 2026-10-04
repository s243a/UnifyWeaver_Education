<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 2: The awk-like surface, and where it diverges

> *Skeleton. Each bullet names the source facts the section will draw on. Awk basics are not re-taught; link to the [AWK Target book](../../book-awk-target/README.md).*

## What is shared

- Pattern-action rules, `BEGIN`/`END`, `$0`/`$N`, `NR`, `NF`, `FS`, `OFS`, `print`, `printf`. Parser AST: `program(Begin, Rules, End)`, `rule(Pattern, Actions)` (`parser/plawk_parser.pl:124, 290-308`).
- Patterns: bare `/re/`, `$N ~ /re/`, `$N == "s"`, numeric `$N <op> K`, and `&&`/`||`/`!` combinators (`plawk_parser.pl:336-585`). Bare literal regexes get fast paths (`prefix`, `contains`); metacharacters go to POSIX ERE via `regcomp`.
- Actions: `if`/`else if`/`else`, `next`, `break`, `x++`, `x += K`, `x = e`, `a[k]++`, `for (k in a)` in `END` only (`plawk_parser.pl:841-906, 722-742`).
- Builtins: `length`, `substr`, `index`, `tolower`, `toupper`, `int` (`plawk_parser.pl:937-1122`).

## The cross-compatible subset

- Worked examples verified under both plawk and gawk: the counter/assoc/END report from Chapter 1; `BEGIN { OFS = "," } $1 == "ERROR" { print NR, $2, $3 }`; `{ counts[$2]++ } END { for (k in counts) print k, counts[k] }` (compare after `sort`, since iteration order is unspecified in both).
- A table: construct, valid in awk, valid in plawk, same output.

## Where plawk diverges from POSIX awk

- Not parsed: `while`, `do..while`, C-style `for`; `sub`/`gsub`/`split`; `arr[k] = v`, `delete`, `arr[i,j]`; multiple `END` blocks (recon Q2, `plawk_parser.pl:709-742`).
- User functions: single `return <arith-expr>` only, no locals, loops, or conditionals (`plawk_parser.pl:136-177`). They desugar to Prolog clauses (Chapter 8).
- `BEGIN` accepts only `BINFMT`, `OUTFMT`, `DYNLOAD`, `DYNCACHE`, `FS`, `OFS` assignments and `print` (`plawk_parser.pl:654-707`); `FS`/`OFS` are single bytes.
- Semantic differences already observed: integer `/` truncates; uninitialised scalar is `0`, not empty; `%d`-family `printf` only (project README, "printf" paragraph).
- Parse error vs "parses but is outside the compilable surface": the CLI distinguishes them (exit 2 vs 3).

## Reading a rejection

- How to tell a parse failure from a codegen rejection; the codegen `fail` clauses that reject mismatched `writebin`, dual-terminal `if` branches, and text-only forms in binary mode (recon Q5, `plawk_native_codegen.pl:2492, 3097-3104, 5736-5740`).
- <!-- TODO: confirm whether the CLI surfaces a reason for a rejection or only the generic "outside the compilable surface" message (recon follow-up) -->
