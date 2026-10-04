<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 5: Tagged unions and `case` routing

> *Skeleton. This is the book's centrepiece: the feature with no awk analogue. Verified example: the telemetry program from Chapter 1 (output `6.5 2` on a six-record feed). Source tests: `tests/test_plawk_tagged_unions.pl`, `test_plawk_union_rep.pl`, `test_plawk_union_assoc.pl`.*

## The problem: one stream, several shapes

- Tag, arm, discriminated record, in the vocabulary of C unions, Rust enums, and Prolog functors (TUTORIAL "Tagged unions").
- Byte-level wire diagrams for the metric arm and the event arm.

## Declaring a union

- `BINFMT = "case(i64 f64 | lps16 i64)"`: an 8-byte tag then the arm's layout; arms separated by `|`.

## `case K { ... }` blocks

- AST: `case_blocks([case_arm(Index, Rules), ...])` (`plawk_parser.pl:258-288`).
- Inside a block `$1..$N` are typed by the arm; why a block rather than a tag test in each guard (the compiler must know the arm's types before it can accept `$1 == "boom"`).
- Shared across arms: scalars (including doubles), `NR`, `next`/`break`, `if`/`else`, the `END` report, and one assoc table across arms.

## The `TAG == K` shorthand

- Parses to `tag_pat(Tag)` (`plawk_parser.pl:403-413`); sugar for the same rule inside `case K` (identical IR per the project's docs). Rules: the tag test must lead the guard; `||`/`!` or non-leftmost tag tests are rejected; do not mix spellings in one program.

## What the generated reader does

- Read 8-byte tag, native `switch`, per-arm field reads into one buffer sized to the widest arm. Test evidence: the IR assertions in `test_plawk_tagged_unions.pl` (`switch i64 %vr_tag, ...`, `malloc(i64 24)`).
- Unknown tag or truncated arm exits with the read-error code; arms with no block are read and skipped so framing is kept.

## Arms with lists

- `case(i64 rep4(lps8 i64) | i64)`: `foreach` inside an arm; element types, staging, and buffer sizing resolve per arm.

## Rejections worth knowing

- Case index beyond declared arms; string equality on an `i64` arm field; numeric compare on an `lps` field; `case` without a union `BINFMT` (the `union_rejections` test).
