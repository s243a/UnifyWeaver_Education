<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 6: Writing binary: `OUTFMT`, `writebin`, `writebin_arm`

Chapter 4 ended with a debt: its binary input files cannot be made with `echo`, and the chapter promised that plawk would write them itself. This chapter pays it. The write side mirrors the read side. You declare an output layout once, with `OUTFMT`, and then emit records with `writebin`; the bytes that come out are the bytes the `BINFMT` reader of Chapters 4 and 5 expects. That match is what makes the payoff Chapter 1 promised possible: one plawk program writes binary, the next reads it, and no text is ever formatted or parsed in between.

## A text-to-binary converter

`OUTFMT` is assigned in `BEGIN`, exactly like `BINFMT`, and uses the same type words. `writebin` then takes one expression per slot:

```awk
BEGIN { OUTFMT = "i64 f64" }
{ writebin $1, float($2) }
```

Fed the text lines `5 2.5`, `20 4.5`, `30 1.25`, this writes three 16-byte records to standard output: `(5, 2.5)`, `(20, 4.5)`, `(30, 1.25)`, each an `i64` then an `f64`. The test decodes the bytes and checks exactly that list (`surface_text_to_binary_converter` in `tests/test_plawk_binary_writers.pl`). The parser represents the action as `writebin([field(1), field(2)])` (`plawk_parser.pl:1014-1017`). As in Chapter 3, a text field headed for an `f64` slot is wrapped in `float(...)`.

Arguments are ordinary expressions, not just fields. `BEGIN { OUTFMT = "i64 i64" } { writebin NR, $1 + 1 }` over the lines `10` and `20` writes `(1, 11)` and `(2, 21)` (`surface_writebin_nr_and_expr_args`).

`writebin` works in binary-input mode too, so a program can be a filter or transformer. This one keeps records whose `$1` exceeds 10, scales them, and still prints a count:

```awk
BEGIN { BINFMT = "i64 f64"; OUTFMT = "i64 f64" }
$1 > 10 { n++; writebin $1 * 10, float($2) * 0.5 }
END { print n }
```

On the test's three input records the output is two binary records, `(200, 2.25)` and `(300, 0.625)`, followed by the text `2\n` (`surface_binary_transform_with_guard_and_scalars`). Note what that means: `writebin` and `print` share standard output, so a trailing `print` lands as text after the binary records. Keep them apart in a real pipeline.

An `i64` expression may fill an `f64` slot. The reverse is rejected, along with several other mistakes, when the program is compiled (`writebin_rejections`):

- `writebin` with no `OUTFMT` declared;
- an argument count that differs from the number of slots;
- a double into an `i64` slot (`OUTFMT = "i64"` with `writebin 1.5`);
- a string field into an `i64` slot (`s8` in, `i64` out).

## Strings on the way out

String slots follow the reader's conventions.

- `sN` writes exactly `N` bytes, zero-padded. A text field `alpha` into `s8` is 8 bytes; an empty field writes an all-zero slot (`tests/test_plawk_outfmt_strings.pl`). A literal longer than `N` (`writebin "toolong"` into `s4`) is a compile-time rejection.
- `lpsN` writes an 8-byte length and then exactly that many bytes, so `"alpha"` into `lps16` costs 13 bytes, not 24. A source value longer than the cap is clamped: `longvalue` into `lps4` is written as `long` (`surface_text_clamps_to_cap` in `tests/test_plawk_varlen_writers.pl`). Wire lengths are exact; the test with a 16-character value in an `lps16` slot confirms no padding and no clamp at exactly the cap.

A literal can fill a slot as a constant tag. `BINFMT = "i64 lps8"; OUTFMT = "lps4 lps8 i64"` with `$1 > 10 { writebin "tag", $2, $1 * 2 }` prepends the constant `tag` to each surviving record, and an empty `lps8` source is written as a zero-length string (`surface_varlen_transform_with_literal_and_empty`).

Lists pass through with `repK` slots: `OUTFMT = "i64 rep4(i64 f64)"` writes the count and then exactly the live elements, never padding to `K`. A filter that keeps records with `$1 > 0` produces output byte-identical to the kept input records (`surface_rep_filter_is_byte_exact` in `tests/test_plawk_rep_writer.pl`). The rep slot is fed by the input's own rep field (the test passes `$2`); the same test file rejects a differing cap (`rep8` out of `rep4` in), differing element types, and feeding a rep slot from a non-rep field.

## Flat output from a union: per-arm writers

A union input does not force a union output. Here two record shapes are normalised into one fixed layout:

```awk
BEGIN { BINFMT = "case(i64 f64 | lps16 i64)"; OUTFMT = "i64 i64" }
case 0 { { writebin $1, NR } }
case 1 { { writebin $2, NR } }
```

For the test feed (a metric `50`, an event `boom` with count `7`, a metric `200`) the output is the 16-byte records `(50, 1)`, `(7, 2)`, `(200, 3)` (`surface_union_normalizer` in `tests/test_plawk_union_writebin.pl`). `OUTFMT` is program-wide, one layout for the whole program; what differs per rule is the *source*. Inside `case 1`, `$2` is the arm's `i64` at its own offset, which the IR test pins down. The same file shows the tag-guard spelling, `TAG == 1 && $2 > 5 { writebin $1, $2 }` with `OUTFMT = "lps16 i64"`, passing an arm's string through; arm 0 has no rule and is read and skipped, as Chapter 5 describes.

## Tagged output: `writebin case K`

To write a union rather than flatten one, declare a union `OUTFMT` and say which arm each write targets:

```awk
BEGIN { BINFMT = "case(i64 f64)"; OUTFMT = "case(i64 | i64 lps8)" }
TAG == 0 && $1 > 100 { writebin case 0, $1; next }
TAG == 0             { writebin case 1, $1, "low" }
```

`writebin case K, args` stores the 8-byte tag `K`, then the arm's slots; it parses to `writebin_arm(Index, Fields)` (`plawk_parser.pl:998-1013`). The arguments must match arm `K`'s slots in number and type. On inputs `(200, 1.5)`, `(5, 2.5)`, `(300, 0.25)` the test expects, in order: tag 0 and `200`; tag 1, `5`, length 3, `low`; tag 0 and `300` (`surface_split_stream_into_tagged_output` in `tests/test_plawk_union_out.pl`). The generated writer shares one buffer sized to the widest arm (16 bytes here) and stores each site's constant tag first.

### Layout mismatches are rejected

`writebin` and `writebin case K` are not interchangeable. The codegen says so in a comment and a pair of `fail` clauses (`plawk_native_codegen.pl:3097-3104`):

- a **plain `writebin` against a union `OUTFMT`** fails: it cannot pick an arm;
- a **`writebin case K` against a flat `OUTFMT`** fails: there is no arm to pick.

The test file confirms both with programs that do not compile (`union_outfmt_rejections`), alongside an arm index beyond the declared arms and an arm with the wrong argument count. A rejected program produces no binary, not a binary that writes garbage.

## Pipelines with no text in the middle

The reader and writer agree byte for byte, so one program's output can be another's input. The capstone test splits a tagged stream and then reads the result with a second program:

```awk
# stage 1, as above
BEGIN { BINFMT = "case(i64 f64)"; OUTFMT = "case(i64 | i64 lps8)" }
TAG == 0 && $1 > 100 { writebin case 0, $1; next }
TAG == 0             { writebin case 1, $1, "low" }

# stage 2
BEGIN { BINFMT = "case(i64 | i64 lps8)" }
TAG == 0 { bigsum += $1 }
TAG == 1 && $2 == "low" { lows++ }
END { print bigsum, lows }
```

Stage 1's `OUTFMT` is stage 2's `BINFMT` with the arms written in the same order; that is the whole contract. On the three-record feed the second program prints `500 1` (`surface_tagged_output_roundtrips_through_union_reader`). Flat pipelines are tested too, with the stage output captured and checked:

- text to `OUTFMT = "i64 f64"`, then `BINFMT = "i64 f64"` with `$1 > 10 { sum += float($2) }`, printing `5.75` (`surface_plawk_to_plawk_pipeline`, `test_plawk_binary_writers.pl`);
- text to `OUTFMT = "lps16 i64"`, then the matching `BINFMT` summing the `alpha` rows, printing `7` (`surface_varlen_round_trip`);
- a group-by, `{ counts[$1]++ } END { for (k in counts) writebin k, counts[k] }` over `i64 i64` input, then a second stage summing the counts, printing `6` (`surface_groupby_pipeline_two_stages`).

The group-by is worth noticing. `for (k in counts) writebin k, counts[k]` is how an integer-keyed table from Chapter 4 leaves the program as records. The test checks the pairs `(-3, 2)`, `(5, 3)`, `(9, 1)` after sorting, because iteration order is not guaranteed.

### What these tests do and do not show

They show that the bytes one compiled program writes are consumed correctly by another, with no text formatting or parsing in the middle. They do **not** show a live operating-system pipe. In each two-stage test the harness runs `stage1 > stage.bin && stage2`: the first program's output goes to a file, and the second program is compiled with that file's path as its input. So "a pipeline" here is a file handoff, and nothing in these tests demonstrates streaming through `|` or reading standard input.

<!-- TODO(run-verify): confirm whether a compiled plawk binary can read from a pipe or stdin; the tests above all use a named file compiled into the driver. -->

Every output in this chapter is a test's expected value, not a fresh run by the author; the tests build and run native binaries only where `clang` is available. The chapter makes no performance claim for the write path; Chapter 11 owns benchmarks.

## Next

Chapter 7 turns to the other half of the project: the Prolog core that plawk sits on.
