<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 11: Demonstrated vs design

Chapter 1 gave a short ledger of what runs and what does not. This chapter is the long form, and it is the one place in the book where the whole account is collected. Each earlier chapter ended with its own "what is demonstrated" caveat; this one gathers them, adds what the project's documents flag as next, and says plainly where the evidence stops.

Three words carry the ledger. **Run** means a program or test was executed in the course of preparing this book and its result observed. **Test-backed** means a test in the repository asserts the behaviour, but its result was read, not re-run here. **Design** means a document or comment describes it, or it is a stated intent, with no passing test that establishes it.

## What each subsystem has behind it

The count first, because earlier drafts of this book were vague about it. There are exactly **44** files matching `tests/test_plawk_*.pl`, plus the **16**-test pure-Prolog suite in `examples/plawk/core/plawk_core_tests.pl`. They exercise different things.

- **The core** (Chapter 7) has the 16 tests. They cover the reader/handler loop, field accessors, `NR`/`NF`/`FS`/`OFS`, and a text-file reader. They say nothing about the parser or code generator.
- **The parser and native code generator** have no tests in that core file. They are exercised by the 44-file suite, which parses programs, inspects generated IR, and, where `clang` is present, builds and runs native binaries.

This corrects a mistake that the first reconnaissance pass made. That pass read only the core test file and reported the parser and code generator as untested. They are not: they are the subject of 44 test files. The error is worth stating because it is the natural one. The file named `plawk_core_tests.pl` sounds like the project's test suite and is only one subsystem's.

I ran twenty-three of the 44 files, all of which passed in full. Twelve are the output-bearing feature tests of Chapters 3 to 6: `float_slots` (13 tests), `bounded_rep` (8), `binary_assoc` (11), `tagged_unions` (11), `binary_records` (14), `union_rep` (5), `union_out` (4), `binary_writers` (7), `union_writebin` (4), `varlen_writers` (7), `rep_writer` (6), and `outfmt_strings` (6) — 96 tests across the typed-slot, binary-record, union, and writer features. The other six are the Prolog bridge and runtime-grammar tests behind Chapters 8 and 9: `prolog_blocks` (6), `surface_prolog_calls` (16), `functions` (5), `f64_foreign` (7), `dyncall` (5), and `dyncall_at` (5) — 44 more tests. Five more were run while preparing this book: `surface_arith_exprs` (22), `surface_float_exprs` (17), `surface_stdin_input` (8), `binfmt_strings` (9), and `varlen_records` (6) — 62 tests covering integer and float expressions, stdin input, and fixed- and variable-length binary strings. That is 202 tests over twenty-three files, and it establishes that the parser, code generator, foreign bridge, runtime loader, and CLI path are all exercised for real, not just the core.

It is still not a run of the whole suite. The other 21 files — among them `tier2_blob` and the append and else-if surface tests — were read but not run for this book. Where a chapter quotes a value from one of those, it is the test's expected string, flagged as such at the point of use.

The front door was also run directly. A pure-awk program (the counter-and-report shape of Chapter 1) was compiled by `plawk` and executed, and its output matched gawk's: `total 5 errors 3 ERROR-lines 3`.

## The decoupling, stated precisely

The interpreter does not run the parser's output. The parser feeds the native code generator, and the `plawk` CLI never calls the interpreter (Chapters 7 and 10). Two consequences belong in the ledger.

First, the 16 core tests passing does not show that compiled binaries behave as the core does. The one test that links the two, `test_plawk_compiled_stream_core.pl`, compiles a hand-written loop that reuses the core's helper predicates, not `process_all/4`. Second, the project's design intent is that the core serves as a reference model for the compiled loop. I found no test that runs the same input through both and compares the outputs. That agreement is a design intent, not a checked property.

## The ledger

| Feature | Status | Evidence |
|---|---|---|
| Pure-awk subset, source to native binary | Run | Direct build and run; output matched gawk |
| Pure-Prolog core (`process_all/4`, accessors) | Test-backed (reference model only) | 16 core tests; not the engine behind `plawk` |
| `i64`/`f64` binary records (`BINFMT`) | Run (tests) | `binary_records` 14/14 |
| `lpsN`, `repK(...)`, `foreach` | Run (tests) | `bounded_rep` 8/8; `rep_writer` 6/6; `varlen_writers` 7/7 |
| Tagged unions, `case K`, `TAG == K` | Run (tests) | `tagged_unions` 11/11; `union_rep` 5/5 |
| `OUTFMT`, `writebin`, `writebin case K` | Run (tests) | `binary_writers` 7/7; `union_out` 4/4; `union_writebin` 4/4 |
| Typed integer-keyed assoc tables | Run (tests) | `binary_assoc` 11/11 |
| `float` slots, double `END` expressions | Run (tests), with caveats below | `float_slots` 13/13 |
| Text-mode `printf`, `float(...)`, guards | Run (tests) | `surface_prolog_calls` 16/16; `f64_foreign` 7/7 |
| `@prolog` blocks, bridged calls, `function` | Run (tests) | `prolog_blocks` 6/6; `surface_prolog_calls` 16/16; `functions` 5/5; `f64_foreign` 7/7 |
| `blobN` Tier-2 payloads | Run (tests), one path | DCG-over-blob end to end in `prolog_blocks` (`119`); dedicated `tier2_blob` file not re-run |
| `DYNLOAD`, `dyncall`, `dyncall_at` | Run (tests); standalone not reproduced | `dyncall` 5/5, `dyncall_at` 5/5 (harness builds the `.wamo`, runs the binary); no hand-built `.wamo` outside the harness |
| Reading from stdin | Test-backed, not re-run | `surface_stdin_input` checks the emitted `main` and has stdin smoke tests; I did not run them or a shell pipe |
| Benchmarks (W1 to W4) | Project's claim | README table; `bench.sh` not re-run here |
| Runtime grammar extension (JIT) | Design | Implementation plan Phase 5 |

Read the table with the three qualifiers in mind. "Run (tests)" means the test file passed when I ran it, so the feature is demonstrated at the level of those tests' programs and no further. It does not mean every combination of features works.

## Design, partial, and unconfirmed

**The awk subset has hard edges.** No `while`, `do`, or C-style `for`; the only `for` is `for (k in arr)` as the action of `END`. No `sub`, `gsub`, or `split`. The only array write is `arr[k]++`: no `arr[k] = v`, no `delete`, no multi-dimensional keys. A function body is a single `return` expression. `BEGIN` configures and does not compute, and `END` holds one action (Chapter 2). Parse-clean does not imply compilable: binary mode rejects text-shaped forms (regexes, string equality, `substr`, `$0`, arrays) at code generation, and the CLI's rejection message says only that the program is "outside the compilable surface".

**Interpreter and engine are separate.** Covered above. The core is a model, not an engine, and no test ties the two together.

**Double slots are partial.** Doubles work in expressions, in `END` arithmetic, and as accumulators with `float(...)` (Chapter 3). Assigning a double-typed expression to a scalar is rejected at code generation, because general scalar slots are `i64`, and the README calls typed double slots the documented follow-up. A double used as a pattern guard operand has no test that I found, so it is unconfirmed. Treat it as neither supported nor rejected until someone tries it. A `printf` format that mismatches its operand type is also uncharacterised beyond the README's statement that double expressions into `i64` formats are rejected.

**`dyncall` is run at the test level, not reproduced standalone.** This has moved since earlier drafts. The two test files were run for this book, and they exercise the whole loop: each builds a real `.wamo` with `write_wam_object/3`, compiles the plawk program to a native binary, runs it, and asserts the output — the `150`-then-`44` swap with no rebuild, `dyncall_at` selection by field (`30`), and `mtime` reload (`7` then `11`). So `dyncall` is demonstrated end to end, not merely described (Chapter 9). What remains open is narrower than it was: a `.wamo` built *by hand outside the harness* and driven by a program of my own, as an independent reproduction of the README's worked example. The README also calls the load step "JIT-like"; the execution-architecture document describes AOT native loops plus WAM bytecode on a natively compiled interpreter, with no JIT yet, which is why this book says "runtime-loaded", not "JIT".

**Bridged calls on text fields have a known root cause, not a mystery.** Chapter 8 settles the mechanism: in text mode a field `$N` is interned as a Prolog *atom* (`wam_intern_atom`), never coerced to a number, so a bridged call that does arithmetic on a raw text field gets atoms where awk's automatic string-to-number coercion would give numbers. The supported text-mode uses — classifying a field against a fact table, mapping it to a rank, and integer or string literals — are run and pass (`surface_prolog_calls` 16/16). Binary-mode arithmetic, where fields arrive typed, is also run (`f64_foreign` 7/7). What is genuinely open is a *design* question, not a behaviour the book has failed to pin down: whether plawk should coerce numeric-looking text fields at the bridge or keep atom semantics and require an explicit conversion. (The separate observation that a `function` placed before `BEGIN` fails to parse is a consequence of the fixed program grammar, explained in Chapter 8, not a defect.)

**Pipelines are file handoffs in the tests.** The two-stage tests of Chapter 6 write the first stage's output to a file and compile the second stage with that path. They show that one program's bytes are read correctly by another. They do not show streaming through `|`.

## Benchmarks in context

The README reports four workloads at N = 2,000,000 records (about 29 MB text, 32 MB binary), with mawk 1.3.4 as the baseline and best-of-3 timings, from one run on a 4-core 2.80 GHz Xeon container using `clang -O2`:

| Workload | plawk | mawk | Ratio |
|---|---|---|---|
| W1 filter-count (text) | 104 ms | 180 ms | 1.7x |
| W2 aggregate (text) | 139 ms | 309 ms | 2.2x |
| W3 aggregate (binary records) | 17 ms | 234 ms | 13.8x |
| W4 group-by (text) | 117 ms | 186 ms | 1.6x |

These are the project's own figures, and I did not reproduce them. I did not run `bench.sh`, so I cannot say what they would be on another machine, and the README itself calls them environment-relative. Some context for reading them:

- **A correctness gate precedes timing.** `bench.sh` checks that plawk's output is byte-identical to the system awk's on every text job before timing anything. That makes W1, W2, and W4 comparisons between programs that agree on output. It is a design strength of the harness, and I read it in the script, not by running it.
- **W3 is not like-for-like.** The 13.8x figure compares plawk reading binary records against mawk parsing the text encoding of the same data. It measures the benefit of the representation, which is the project's thesis, and not plawk's speed against mawk on equal input. The text-mode ratios of 1.6x to 2.2x are the like-for-like numbers, and the README itself describes them as honest but modest.
- **One machine, one run, one baseline.** There are no variance figures beyond best-of-3, and mawk is one particular awk. Gawk or another implementation could rank differently.
- **A second figure appears elsewhere.** The README also quotes 0.040 s for a binary program against 0.225 s for mawk on equivalent text (5.6x), and 0.156 s for plawk's own text mode, on 2M records in a different passage. It is a different measurement from the table's W3, and the README does not say they come from the same run, so the two should not be read as one result.
- **The foreign-call costs** quoted in Chapter 8 (about 5 microseconds per bridged call, about 0.2 microseconds on the blob path) are likewise the project's figures.

To check any of this, run `N=2000000 sh examples/plawk/bench/bench.sh` on your own machine. That needs `swipl`, `clang`, `python3` (the script uses it to generate the data), and an awk. The repository also has `tests/test_plawk_bench_smoke.pl`, a smoke test of the harness, which is not a performance result.

## Known gaps and roadmap

What the project's documents flag as next, in their words and with their status:

- **Typed double slots.** The README names them as the follow-up to the scalar-slot restriction above.
- **More of awk.** None of the Chapter 2 omissions is described as planned; they are the current boundary. I found no stated commitment to add `while`, `sub`, or general arrays, so do not read the list as a backlog.
- **Grammar-to-native readers.** `PLAWK_DCG_BINARY_READERS` fixes a three-tier design: Tier 1, LL(1)-over-fields grammars with compile-time caps lowered to native read sequences; Tier 2, native framing plus WAM-bytecode payload parsing through the foreign bridge; Tier 3, a full DCG fallback. The `lpsN`, union, and `repK` readers are its first slices, and the implementation plan says Tier-2 composition sugar remains.
- **Phases 4 and 5.** The implementation plan lists file descriptors and subprocess redirection (Phase 4) and runtime grammar extension, clause assertion, and a REPL (Phase 5) as goals. Neither is shown by a test in this book's survey. `dyncall` is the nearest existing piece to Phase 5, and it loads a pre-built object, which is narrower than that phase's stated success condition of a running binary accepting new rules and compiling them.

The design documents are `PLAWK_PHILOSOPHY`, `PLAWK_SPECIFICATION`, `PLAWK_IMPLEMENTATION_PLAN`, `PLAWK_EXECUTION_ARCHITECTURE`, and `PLAWK_DCG_BINARY_READERS`, under `docs/design/`. They are statements of intent, and several describe more than exists.

## A note on the phase labels

The project README is inconsistent about its own state, and this book should not repeat the inconsistency as fact. Its opening status line says "design / prototype phase". Its "Planned structure" tree labels `core/` as Phase 0 and both `parser/` and `codegen/` as Phase 2, under the heading "Planned", yet the same README documents the parser and code generator as working, with a CLI, a benchmark, and the test suites described above. It also headlines a section "Run the Phase 0 prototype" over commands that run the core's tests and demos.

The implementation plan supplies the likeliest reading: Phase 0 is the Prolog core, Phase 1 the LLVM integration, Phase 2 the awk sugar (parser and its code generation), and Phase 3 the binary data structures and DCG readers. Under that numbering "Phase 2" for the parser is a correct phase name. What is stale is the word "Planned" and the single-line status. The plan's own text records slices of Phase 3 (varlen writers, tagged unions, bounded repetition, typed associative arrays) as already landed. So the implementation has moved past the README's labels. This book uses the labels only to point at the plan, and describes status by what runs.

## What the ledger adds up to

The front door is real for the subset: a `.plawk` file in, a native binary out, with output matching gawk on the one cross-compatible program I ran, and with binary records, tagged unions, writers, the `@prolog` bridge, and runtime-loaded grammars all backed by passing tests that build and run native code. What is not there is the breadth of awk, a link between the reference interpreter and the compiled output, a *standalone* `dyncall` reproduction outside the test harness, a settled design for doubles as general scalars and for text-field coercion at the bridge, and an independent measurement of the speed claims. The thesis, that typed binary records beat parsing text, is the best-evidenced part structurally and the least independently measured numerically.

## Remaining checks

If you extend this ledger, these are the open items, in order of how much they would change it:

1. Run all 44 test files, not twenty-three, and record the machine and the `swipl` and `clang` versions.
2. Re-run `bench.sh` and record the hardware beside the numbers.
3. Build a `.wamo` by hand with `write_wam_object/3`, outside the test harness, and reproduce the README's `dyncall` swap from a program of your own.
4. Write a differential test of native output against `process_all/4` on the same text input — the native-versus-interpreter agreement that is currently design intent, not a checked property.
5. Settle the two open design questions: a double used as a pattern-guard operand (behaviour untested), and whether the bridge should coerce numeric-looking text fields (Chapter 8's atom-interning edge).
