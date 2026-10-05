<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 5: Tagged unions and `case` routing

This is the chapter the book exists for. Everything so far (typed arithmetic, binary layouts) is awk made faster or stricter. A tagged union is something awk cannot say at all: a single stream whose records have *different shapes*, with the compiler knowing, rule by rule, which shape each rule sees.

## The problem: one stream, several shapes

The telemetry feed from Chapter 1 mixes two kinds of record. A *metric* is an id and a reading; an *event* is a name and a code. In awk you would dispatch by hand on a prefix or a first column, and every value would stay a string until you converted it. In a binary stream there is no prefix to match on, and each kind has a different size and different field types. So the stream carries its own discriminator: every record starts with an 8-byte **tag** (an `i64`), and the tag says which layout follows. The vocabulary is the one from C unions, Rust enums, or Prolog functors: a *tag*, and an *arm* per shape.

```text
metric (tag 0):  | tag=0 | i64 id | f64 reading |            24 bytes

event  (tag 1):  | tag=1 | len=4 | "boom" | i64 code |       8+8+4+8 = 28 bytes
```

The metric arm is fixed-width. The event arm uses the length-prefixed string of Chapter 4, so its size varies per record. (The test builds exactly these shapes: `write_union_records` in `tests/test_plawk_tagged_unions.pl` writes `m(V, F)` as tag 0, `i64`, `f64`, and `e(S, C)` as tag 1, length, bytes, `i64`.)

## Declaring a union

```awk
BEGIN { BINFMT = "case(i64 f64 | lps16 i64)" }
```

`case( ... )` wraps the arms, separated by `|`. Arm 0 is `i64 f64`, arm 1 is `lps16 i64`; an arm's number is its position, counting from zero, and it is the value the tag must hold. Each arm is an ordinary Chapter 4 layout; the tag is not a field. `$1` never means the tag.

## `case K { ... }` blocks

Rules for an arm go inside a block:

```awk
BEGIN { BINFMT = "case(i64 f64 | lps16 i64)" }
case 0 { $1 > 100 { msum += float($2); mhits++ } }
case 1 { $2 == 7 { print $1 }  $1 == "boom" { events++ } }
END { print mhits, msum, events }
```

Fed six records (metrics `(50, 1.5)`, `(200, 2.5)`, `(300, 0.25)` interleaved with events `("hello", 7)`, `("boom", 3)`, `("boom", 7)`, in the order metric, event, metric, event, metric, event), this prints

```text
hello
boom
2 2.75 2
```

That is the expected output of `surface_union_dispatch_and_state` in `tests/test_plawk_tagged_unions.pl`, which builds and runs a native binary. Chapter 1's shorter program, which prints only `msum` and `events`, is the same computation minus `mhits`; its `2.75 2` is the last two numbers of this line. *Run-claim: the Chapter 1 variant itself is not a separate test; its output is inferred from this one and should be re-run.*

The parser turns the blocks into `case_blocks([case_arm(0, Rules), case_arm(1, Rules)])`, which the `parses_case_blocks` test checks (for example `case 0 { $1 > 100 { c++ } }` becomes `case_arm(0, [rule(field_cmp(1, gt, 100), [inc(var(c))])])`).

### Arm typing

Why a block rather than a tag test in every guard? Because the compiler must know what `$1` *is* before it can accept `$1 > 100` or `$1 == "boom"`. Inside `case 0`, the arm is `i64 f64`, so `$1` is an integer and `$2` a double (hence `float($2)`, as in Chapter 3). Inside `case 1`, the arm is `lps16 i64`, so `$1` is a string and `$2` an integer. The same token, `$1`, has two types in two blocks, and each use is checked against its own arm. That checking is the point: awk would let `$1 == "boom"` run against a metric and quietly compare the text of a number.

What is shared across arms is the program's state, not the fields: scalars (including the double `msum`), `NR`, the `END` report, and `next`. In the test `surface_union_next_and_nr`, `case 1 { $1 == "skip" { next } { events++ } }` over four records (a metric, `skip`, `go`, a metric) prints `4 1`: `NR` counts records of every arm, `next` works inside a block. An arm with no block is still read and skipped, so framing stays intact: with only a `case 1` block, a stream of two metrics and two events prints `2` (`surface_union_unhandled_arm_still_reads`).

## The `TAG == K` shorthand

The same program can be written without blocks, by guarding each rule with its tag:

```awk
BEGIN { BINFMT = "case(i64 f64 | lps16 i64)" }
TAG == 0 && $1 > 100 { msum += float($2); mhits++ }
TAG == 1 && $2 == 7  { print $1 }
TAG == 1 && $1 == "boom" { events++ }
END { print mhits, msum, events }
```

It prints the same `hello`, `boom`, `2 2.75 2` on the same six records (`surface_tag_guard_dispatch`). `TAG == K` parses to `tag_pat(K)`, and the parser's own comment calls it surface sugar: the code generator groups tag-guarded rules into the same per-arm blocks. The test `tag_guard_sugar_matches_case_blocks_exactly` goes further, and checks that `TAG == 1 && $2 > 5 { b++ }` and `case 1 { $2 > 5 { b++ } }` produce *byte-identical* LLVM IR. Rules for different arms may be interleaved in source order, as above.

Because a tag test selects an arm, it has to be unambiguous. `tag_guard_rejections` lists what is refused, and in each case the program parses but the code generator rejects it:

- an unguarded rule beside guarded ones (`TAG == 0 { a++ } { b++ }`): a rule with no tag has no arm to type its fields against;
- a tag under `||` (`TAG == 0 || TAG == 1`): two arms, so no single typing;
- a tag that is not the leftmost conjunct (`$1 > 3 && TAG == 0`);
- a tag beyond the declared arms (`TAG == 5` over two arms);
- `TAG` over a layout that is not a union (`BINFMT = "i64 i64"`).

The test does not exercise mixing both spellings in one program, so this chapter makes no claim about it; treat it as unsupported until checked.

## What the generated reader does

The reader reads the 8-byte tag, then branches on it with a native LLVM `switch`, and each arm has its own field-reading sequence. The test `union_ir_has_tag_switch_and_arm_reads` asserts the IR contains `switch i64 %vr_tag, label %fail_read [ i64 0, label %vr_a0 i64 1, label %vr_a1 ]`, per-arm reads (`%vr_a0_f0_dst`, `%vr_a1_f0_fits`), and a rule guard that tests the tag before its own pattern (`icmp eq i64 %vr_tag, 0`). One record buffer serves every arm and is sized to the widest: for `i64 f64` (16 bytes) against `lps16 i64` (16 + 8 = 24 bytes) the test finds `malloc(i64 24)`. The tag lives only in `%vr_tag`, not in the buffer. This is Chapter 4's "variable on the wire, fixed in memory" extended across arms.

The error paths are checked in `surface_union_error_paths`, for the program `case 0 { { c++ } }`:

| input | result |
| --- | --- |
| tag 9 (no such arm) | exit status 11 |
| tag 0 then only an `i64` (arm cut short) | exit status 11 |
| empty file | `END` runs, prints `0` |

An unknown tag is fatal, because the reader cannot know how many bytes to skip and framing is lost.

## Arms with lists

An arm may carry the repetition of Chapter 4. In `tests/test_plawk_union_rep.pl`:

```awk
BEGIN { BINFMT = "case(i64 rep4(i64 f64) | lps16 i64)" }
case 0 { $1 > 0 { foreach { n++; wsum += float($2) } } }
case 1 { $1 == "boom" { events++ } }
END { print n, wsum, events }
```

Over five records (arm 0 with leading `1` and two elements, an event `boom`, arm 0 with leading `2` and no elements, an event `x`, arm 0 with leading `3` and one element), this prints `3 4.25 1` (`surface_rep_arm_foreach_dispatch`): three elements totalling 1.5 + 2.5 + 0.25, and one `boom`. As in Chapter 4, the element count is a field of its own, so here `$1` is the leading `i64` and the elements are reached with `foreach`. Buffer sizing and element typing are resolved per arm. `TAG ==` works the same way: `case(rep4(lps8 i64) | i64)` with `TAG == 0 { foreach { if ($1 == "hot") { hits++ }; total += $2 } } TAG == 1 { other++ }` prints `2 14 1` on the test's three records (`surface_rep_lps_arm_with_tag_guards`). The error paths carry over: a count above the cap, a truncated element region, and an unknown tag each exit with status 11 (`surface_rep_arm_error_paths`).

## Rejections worth knowing

`union_rejections` lists programs that parse but must not compile:

- `case 2` over a two-arm union: the arm does not exist;
- `case 0 { $1 == "x" ... }` over an `i64` arm: string equality on an integer field;
- `case 1 { $1 > 5 ... }` over an `lps8` arm: numeric comparison on a string;
- `case 1 { { counts[$1]++ } }` where arm 1's `$1` is a string: associative keys must be raw `i64` fields of their arm;
- `case 0 { ... }` with no `BINFMT`, or with `BINFMT = "i64 i64"`: `case` demands a union layout.

`rep_arm_rejections` adds that `foreach` in an arm without a `rep` (`case 1` of `case(i64 rep4(i64) | lps16 i64)`) is refused, as is `$3` in an arm whose elements have only two fields.

Every one of these is a *compile-time* error. The cost of mis-typing a field in an awk program is a wrong answer at 3 a.m.; the cost here is a program that does not build.

## What is demonstrated, and what is not

Demonstrated by passing tests that build and run native binaries (`test_plawk_tagged_unions.pl`, 11 of 11 passing as run for this chapter; `test_plawk_union_rep.pl` is taken from its expected strings): case blocks, `TAG == K`, shared state, skipped arms, the error exits, and the rejections above. Not demonstrated here: union-aware `writebin case K` (Chapter 6), unions combined with associative arrays beyond the rejection case (the skeleton cited `test_plawk_union_assoc.pl`, which this chapter did not read), and any performance claim.

## Next

Chapter 6: Writing binary — `OUTFMT`, `writebin`, and `writebin case K`, which lets a plawk stage emit the very union streams this chapter reads.
