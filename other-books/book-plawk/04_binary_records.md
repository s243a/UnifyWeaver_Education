<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 4: Binary records

Chapter 1 asserted that plawk can treat a record as typed binary data, so that `$1` is "a single load of a known type at a known byte offset, with no field splitting and no number parsing". This chapter earns that sentence. It starts from zero: you do not need to know how a C struct is laid out.

## Why binary

A text record `200 2.5` is five characters and a space. To use the `200` a program must find the word boundary, then turn the digit characters into a number, and it does this again on every use. A binary record stores 200 the way a CPU register already holds it: as 8 bytes. Reading it back is one load instruction.

The cost of the text form is therefore *repeated work per field*; the cost of the binary form is that you must tell the compiler, in advance, what the bytes mean. That declaration is `BINFMT`.

## Declaring a layout: `BINFMT`

You assign a string to `BINFMT` in the `BEGIN` block. Each space-separated word is one field type. The fixed-width types are:

- `i64` — an 8-byte integer, native byte order.
- `f64` — an 8-byte double, native byte order.
- `sN` — a fixed `N`-byte string, for example `s8`. A value shorter than `N` is padded with zero bytes ("alpha" plus three zeros in an `s8`).

("Native byte order" means whatever your machine uses. The book's examples assume a little-endian machine such as x86-64; the byte patterns below would be reversed on a big-endian one.)

`BINFMT = "i64 f64"` declares that every record is exactly 16 bytes:

```text
byte offset:   0        8        16
              +--------+--------+
              |  i64   |  f64   |     $1 = the i64, $2 = the f64
              +--------+--------+
```

The offset of each field is the sum of the widths before it, and the record size is the sum of all of them. Nothing is stored in the file to say where a field starts; the layout alone determines it. For `"s8 f64 i64"` that is 8 + 8 + 8 = 24 bytes, with the `f64` at offset 8 and the `i64` at offset 16 (checked in `tests/test_plawk_binfmt_strings.pl`).

Here is the point of the whole chapter. After the declaration, `$N` means "the Nth declared field", not "the Nth space-separated word". `$1` over an `i64 i64` layout compiles to a load of 8 bytes at offset 0 of the record buffer, and `$2` to a load at offset 8. The generated code contains no field splitter and no number parser; the project's test for this checks the emitted LLVM for exactly those two loads and for the *absence* of the text-path helpers.

```awk
BEGIN { BINFMT = "i64 i64" }
$1 > 100 { hits++; sum += $2 }
END { print hits, sum }
```

Over three records `(50, 1)`, `(200, 2)`, `(300, 3)` this prints `2 5`. That output was produced by building and running `tests/test_plawk_binary_records.pl` (`surface_binary_guard_and_sum`), one of the suite's 14 tests, all passing; the suite builds and runs a native binary where `clang` is available. The input is a binary file of 48 bytes, not text, so you cannot create it with `echo`; Chapter 6 shows how plawk writes such files itself.

A few things are checked at compile time rather than at run time:

- `f64` is a double, so arithmetic on it must say so: `sum += float($2)`, as in Chapter 3. The plain `sum += $2` over an `f64` field is rejected, as is any `$3` beyond the declared fields.
- Reading is strict about framing. If the file ends partway through a record (half of an `i64 i64` record, say), the program exits with status 11 instead of silently dropping the tail. A clean end of file at a record boundary just runs `END`.

### What binary mode will not do

Binary mode has no text to search. Forms that need the record as text — `$0`, regular-expression patterns, `substr`, `length`, `index`, `tolower`/`toupper`, and foreign calls on strings — are rejected when the program is compiled; the codegen comment lists them (`plawk_native_codegen.pl`, above `plawk_assoc_record_program_ok`). Two nearby cases are allowed and worth distinguishing:

- String fields (`sN`, and `lpsN` below) can be printed and compared for equality with a literal, `$1 == "ERR"`. They cannot be used in arithmetic, `float()`, numeric comparison, or as an associative-array key.
- Integer fields *can* key an associative array, covered at the end of this chapter.

## Variable length: `lpsN`

Fixed `sN` slots waste space when most strings are short. The alternative is a **length-prefixed string**, `lpsN`: on the wire, an 8-byte length followed by exactly that many bytes, at most `N`. The string `"bee"` in an `lps16` costs 8 + 3 = 11 bytes, not a padded 16:

```text
              +--------+---+
              |   3    |bee|      an lps16 holding "bee"
              +--------+---+
```

Records containing an `lps` field are therefore different sizes on the wire, so the reader can no longer fetch a whole record in one read. It reads field by field, and copies each string into a fixed `N`-byte NUL-padded slot in an in-memory buffer. After that, `$k` for an `lps16` field behaves exactly like `$k` for an `s16`: the in-memory type is the fixed one. The source comment states it directly ("downstream consumers see it exactly like an sN field"). This "variable on the wire, fixed in memory" arrangement is what keeps every later feature a compile-time offset.

```awk
BEGIN { BINFMT = "i64 lps16" }
$1 > 10 { hits++; print $2, $1 }
$2 == "skipme" { skips++ }
END { print hits, skips }
```

The varlen tests exercise this layout (`tests/test_plawk_varlen_records.pl`). The reader exits with status 11 on a length greater than the cap, a truncated payload, or an end of file in the middle of a record; end of file exactly between records is clean. (The test asserts the status; the exact byte patterns it feeds are in the test file.)

## Lists inside a record: `repK` and `foreach`

Some records carry a variable number of sub-items: an order with up to four line items, a reading with up to eight samples. The wire convention mirrors `lps`: a count, then that many elements. `repK(...)` declares it, with `K` the cap and the parenthesised words the element's fields:

```awk
BEGIN { BINFMT = "i64 rep4(i64 f64)" }
```

reads as "an `i64`, then up to 4 elements of `(i64, f64)`". On the wire:

```text
        +--------+--------+-----------------+-----------------+
        |  i64   | count=2| elem 1 (i64,f64)| elem 2 (i64,f64)|
        +--------+--------+-----------------+-----------------+
```

The element count is an ordinary `i64` field in its own right: here `$1` is the leading `i64` and `$2` is the count. The elements follow as flat fields (`$3` is the first field of element 1, zero-filled past the actual count), a layout confirmed by `surface_count_field_and_direct_access` in `tests/test_plawk_bounded_rep.pl`: over a `rep2(i64 f64)` layout, `{ csum += $2 ; s += $3 }` on records with 2, 0, and 1 elements prints `3 17` — the counts `2 + 0 + 1`, then the leading integer of each record's first element, `10 + 0 + 7`. A count above `K`, or a payload cut short, exits with status 11, just as for `lps`.

To process the elements, `foreach { ... }` runs its block once per element, and inside the block `$1`, `$2` mean the *current element's* fields:

```awk
BEGIN { BINFMT = "i64 rep4(i64 f64)" }
$1 > 0 { foreach { n++; wsum += float($2); if ($1 > 10) { big++ } } }
END { print n, wsum, big }
```

On the test's four records (with 2, 0, 4, and 1 elements; the last has a negative leading `i64` and is skipped by the guard), this prints `6 6.5 3` — `surface_foreach_aggregation`, one of the 8 passing tests in `tests/test_plawk_bounded_rep.pl`, built and run.

`foreach` is a real runtime loop, not an unrolled copy. The compiler emits one loop; each iteration copies the current element into a fixed scratch slot placed after the declared fields and runs the block against it. So the code size does not depend on the cap: the project's test shows `rep64` compiles to the same single loop body as `rep4`. Memory is constant too.

Elements may be `i64`, `f64`, `sN`, or `lpsN`. With `lpsN` elements, each element has a different wire size, so the reader parses one at a time:

```awk
BEGIN { BINFMT = "i64 rep4(lps8 i64)" }
{ foreach { if ($1 == "hot") { hits++ }; total += $2 } }
END { print hits, total }
```

Limits, all from the rejection tests: `foreach` needs a `rep` layout; `foreach` blocks do not nest; a layout may contain only one `rep`; and elements cannot be nested `rep`s or `blob`s. Repetition also composes with tagged unions, where an arm of a `case(...)` may carry its own `repK(...)`. That belongs to Chapter 5.

## Integer-keyed associative arrays

In binary mode an associative array can be keyed by an `i64` field, using the raw integer as the key:

```awk
BEGIN { BINFMT = "i64 i64" }
{ counts[$1]++ }
END { for (k in counts) print k, counts[k] }
```

Over the keys `5, -3, 5, 9, -3, 5` this yields (in no guaranteed order; the test sorts it) `-3 2`, `5 3`, `9 1`. Missing keys read as `0`. Integer keys are a binary-mode feature: the test suite includes a check that text mode rejects them, and string keys are rejected in binary mode.

## Where the rest goes

- **Chapter 5** covers tagged unions: a stream whose records start with a tag choosing among several layouts, via `BINFMT = "case(i64 f64 | lps16 i64)"` and `case K { ... }` blocks.
- **Chapter 6** covers writing binary records with `OUTFMT` and `writebin`, which is how you create the input files used here.
- **`blobN`**, a length-prefixed payload that plawk never interprets and only passes to a compiled Prolog predicate, is the bridge to the Prolog half of the book. It is parsed and compiled, but its only use is as an argument to a foreign call, so it appears in Chapter 8.

## What is demonstrated, and what is not

Every `BINFMT` type in this chapter (`i64`, `f64`, `sN`, `lpsN`, `repK(...)`) is parsed and compiled by `plawk_native_codegen.pl`, and has a test that builds and runs a native binary (the tests are skipped if `clang` is absent). The chapter's printed outputs — `2 5`, `3 17`, `6 6.5 3`, and the integer group-by counts — come from `tests/test_plawk_binary_records.pl`, `tests/test_plawk_bounded_rep.pl`, and `tests/test_plawk_binary_assoc.pl`, which were built and run for this book: 14, 8, and 11 tests respectively, all passing. The `sN`/`lpsN` string-layout and varlen claims draw on `tests/test_plawk_binfmt_strings.pl` and `tests/test_plawk_varlen_records.pl`, cited but not separately re-run here.

The win Chapter 1 claimed, "no splitting, no re-parsing", is demonstrated here in its *structural* form: the generated IR has fixed-offset loads and no text-path helpers. The *speed* claim, that this is faster than awk on real data, is the project's benchmark and is reported with its caveats in Chapter 11.

## Next

Chapter 5: Tagged unions and `case` routing — one stream carrying several record shapes, and the `case K { ... }` block that routes them.
