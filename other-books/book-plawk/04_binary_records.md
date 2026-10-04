<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 4: Binary records

> *Skeleton. The ground-up explanation (byte-offset diagrams) follows `TUTORIAL.md`'s "Binary records, from the beginning"; the verified examples are in `tests/test_plawk_binary_records.pl` and friends.*

## Why binary

- Text `200 2.5` is re-parsed on every use; a binary `i64 f64` record is 16 bytes and `$1` is one typed load at a compile-time offset (TUTORIAL; README).
- Preview of the project's benchmark claim (W3, binary aggregation); full treatment and caveats in Chapter 11.

## Declaring a layout: `BINFMT`

- `i64` and `f64` are 8 native-endian bytes; `sN` is N fixed bytes, NUL-padded; offsets and record size follow from the declaration.
- Run example: `BEGIN { BINFMT = "i64 i64" } $1 > 100 { sum += $2 } END { print sum, NR }` over a three-record file (verified: output `6 3`).
- String fields are print/equality only; arithmetic, `float()`, numeric compares, and assoc keys on `sN` are rejected.
- Text-shaped forms (`$0`, regex, `substr`/`length`/`index`/case, string assoc keys, foreign calls on strings) are rejected at codegen in binary mode (recon Q5, `plawk_native_codegen.pl:2492-2493`).

## Variable length: `lpsN`

- Length-prefixed string: 8-byte length plus up to N bytes; "variable on the wire, fixed in memory" because each record is materialised into a fixed buffer.
- Failure modes: oversized length, truncated payload, mid-record EOF exit with the read-error code; clean EOF only at a record boundary.

## Lists inside a record: `repK` and `foreach`

- `BINFMT = "i64 rep4(i64 f64)"`; element count is an ordinary `i64` field; `foreach { }` runs the block per element with `$1..$M` rebound to the element; a real runtime loop whose code size does not depend on the cap.
- `foreach` is parsed as `foreach(Actions)` (`plawk_parser.pl:985-989`). Elements may contain `lpsN`.

## Integer-keyed associative arrays

- In binary mode `counts[$1]++` keys on the raw `i64`; `END { for (k in counts) print k, counts[k] }` prints numeric keys; integer keys are binary-only (text mode interns atoms, so integer keys would collide).

## Handing opaque payloads to Prolog: `blob`

- `blobN` is a length-prefixed payload whose only consumer is a compiled Prolog predicate; forward reference to Chapter 8.
