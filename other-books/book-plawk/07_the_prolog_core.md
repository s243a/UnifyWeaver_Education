<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 7: The Prolog core: Reader, Handler, Writer

This chapter describes a small Prolog module, `examples/plawk/core/plawk_core.pl`, and it has to begin with a warning, because Chapter 1's "runs today versus design" ledger is easy to misread here.

**`plawk_core.pl` is not what runs your `.plawk` program.** `bin/plawk` parses the source with `plawk_parser.pl`, hands the resulting AST to `plawk_native_codegen.pl`, and sends the LLVM IR it produces to `clang`. `process_all/4` appears nowhere on that path. The bin script loads exactly three modules — the parser, the native code generator, and `wam_llvm_target` (`examples/plawk/bin/plawk:25-27`) — and the string `plawk_core` does not occur anywhere in it. The core is simply not on the compile path. The parser, the code generator, and the core are three decoupled subsystems, and the parser's output is never executed by the core.

So what is it? It is a **reference model**: a small, executable specification of what a pattern-action stream processor means. It is short enough to read in one sitting, and it has a real test suite (16 tests, all passing). It pins down the contract that the native code must reproduce, in a language where you can try it in one `swipl` session. Read this chapter as the semantics behind Chapters 2-6, not as their implementation.

## Reading a clause

Chapters 7-9 lean on Prolog, so here is the minimum. A clause is `head :- body.`: the head is true if every goal in the body is. Variables start with a capital letter. `( Test -> Then ; Else )` is if/else. `call(G, A, B)` invokes `G` with extra arguments appended, so `call(foo(1), X, Y)` runs `foo(1, X, Y)`. There are no return values; a predicate "returns" by binding its output arguments. That last point shapes the whole design: state is not mutated, it is passed in and passed back out.

## The driver: `process_all/4`

```prolog
process_all(Reader, Handler, State0, StateN) :-
    call(Reader, Item, State0, State1),
    (   Item == end_of_file
    ->  StateN = State1
    ;   call(Handler, Item, State1, State2, Continue),
        (   Continue == yes
        ->  process_all(Reader, Handler, State2, StateN)
        ;   StateN = State2
        )
    ).
```

(`plawk_core.pl:38-47`; declared `:- meta_predicate process_all(3, 4, +, -)` at line 21, meaning `Reader` takes three extra arguments and `Handler` four.) This is awk's main loop in ten lines: fetch a record, stop at end of input, run the rules, repeat. The `Continue` flag is how a rule says `break`: anything other than `yes` ends the loop with the state as the handler left it.

## Reader, Handler, State, and the missing Writer

The contract has three parts, and the third is not a separate argument.

- **Reader**, called as `call(Reader, Item, S0, S1)`. It yields the next record, or the atom `end_of_file`, and returns the state with its input position advanced. The concrete one is `text_file_reader/5` (`plawk_core.pl:75-85`). Partially applied as `text_file_reader(Path, " ")`, it matches the Reader shape. On first call it reads the whole file, splits on newlines, splits each line on the field separator, and stores the remaining records in the state as `text_reader(Path, Rest)`. Each record is `record(text, Line, Fields)`.
- **Handler**, called as `call(Handler, Item, S0, S1, Continue)`. This is the pattern-action block: it inspects the item, maybe emits output, and decides whether to go on. In the compiled language it corresponds to your rules.
- **State**, a four-slot term: `state(InputStreams, OutputStreams, Counter, UserFields)`. `InputStreams` holds the reader's bookmark, `Counter` is the record count, and `UserFields` is the slot for options and user variables.
- **Writer.** There is no Writer argument. Output is *appended to the state*: `append_output/3` adds a line to `OutputStreams` (stored as `outputs(List)`), and `print_fields/3` and `print_item/3` are built on it (`plawk_core.pl:102-142`). The caller reads the result afterwards with `state_outputs/2`. Writing is therefore pure, which is what makes the core testable: a test inspects a list rather than capturing a stdout.

## Awk concepts as Prolog terms

| awk | core | notes |
|---|---|---|
| `$0` | `item_field(0, Item, Line)` | the original line text (`plawk_core.pl:49`) |
| `$N` | `item_field(N, Item, V)` | 1-based, `nth1/3` over the fields; fails if out of range |
| `NF` | `nf(Item, Count)` | length of the field list |
| `NR` | `nr(State, N)` | the state's `Counter`, advanced by `increment_counter/2` |
| `FS` / `OFS` | `fs/2`, `ofs/2` | read from `UserFields = plawk_options(FS, OFS)`, default `" "` |
| `print a, b` | `print_fields([A, B], S0, S1)` | joins with `OFS`, appends one output line |
| `print` | `print_item(Item, S0, S1)` | appends `$0` |

Two details are worth noticing. `NR` is not magic: it is a counter the handler must bump itself (`increment_counter/2`), and every handler in the tests does. And `fs/2` only reads the option; `text_file_reader` takes its separator as an explicit argument, so nothing connects the two inside the core.

A worked example is `examples/plawk/demo/print_error_fields.pl`. Its handler is the Prolog form of the awk rule `$1 == "ERROR" { print NR, $2, $3 }` with `OFS=","`:

```prolog
print_error_fields(Item, State0, StateN, yes) :-
    increment_counter(State0, State1),
    (   item_field(1, Item, "ERROR")
    ->  item_field(2, Item, Component),
        item_field(3, Item, Message),
        nr(State1, RecordNumber),
        print_fields([RecordNumber, Component, Message], State1, StateN)
    ;   StateN = State1
    ).
```

It is driven by `process_all(text_file_reader(Path, " "), print_error_fields, state([], [], 0, plawk_options(" ", ",")), StateN)`. Unmatched records fall through with the state unchanged except for the counter.

## What the 16 tests establish

`examples/plawk/core/plawk_core_tests.pl` is the demonstrated surface of the core, and it is run with `swipl -q -s examples/plawk/core/plawk_core_tests.pl -g run_tests -t halt`. It establishes:

- **Termination.** The loop stops on `end_of_file` (`process_all_stops_on_explicit_eof`) and on a handler returning `no` (`process_all_honors_break`: two records are queued, one is processed).
- **Indexing.** `$2` of `["alpha","beta","gamma"]` is `"beta"`; `$0` is the original line text, not a re-join of the fields.
- **Counters.** `NF` is the field count; `NR` reflects the state counter; `increment_counter/2` changes only the counter slot.
- **Separators.** `FS` and `OFS` default to `" "` and are read from `plawk_options("|", ",")` when present.
- **Output.** `print_fields/3` joins with `OFS` (`"alpha,beta,gamma"`); `print_item/3` emits the original line; `append_output/3` preserves call order.
- **The reader.** `text_file_reader/5` over `core/testdata/sample_log.txt` (`INFO boot`, `ERROR disk`) yields two records and then `end_of_file`.
- **End to end.** `process_all_collects_error_lines` runs the reader and a handler together over the sample log and ends with counter 2 and output `["ERROR disk"]`.

Only two of the sixteen touch a file; the rest use small fake readers (`test_reader/4`) or bare state terms, so the contract is exercised without any I/O.

What the tests do **not** establish: anything about the parser, the native code generator, binary records, tagged unions, `BEGIN`/`END`, arrays, or `printf`. The core has no notion of those. It models text records only (the `record(text, ...)` shape is the only one `item_field/3` accepts), which is a fraction of the language Chapters 3-6 describe.

## What the core is, and is not

It **is** the specification of three things: the shape of the loop, the threading of state through it, and the meanings of `$N`, `NF`, `NR`, `FS`, `OFS`, and `print`. When the native code generator emits a driver loop for a text stream, this is the behavior it is trying to reproduce, and the tests give you a cheap way to say what that behavior is.

It is **not**:

- the engine behind `bin/plawk`. That path is parser, then `plawk_native_codegen.pl`, then `clang`, as described in Chapter 10. The AST the parser builds is consumed by the code generator and never by `process_all/4`.
- an interpreter for `.plawk` source. Nothing in the core parses awk text, and nothing converts a parsed rule into a Handler.
- automatically kept in agreement with the compiler. The decoupling cuts both ways: because the two do not share code, the core's tests passing says nothing about whether compiled output matches it. Agreement is a design intent here, not something this chapter's test suite checks.

The core does touch the native toolchain in one place, and it is easy to over-read. `tests/test_plawk_compiled_stream_core.pl` imports `plawk_core` and compiles a small stream program through `wam_llvm_target` and `clang`. But its loop (`plawk_stream_loop/3`) is **hand-written** in that test file; it does not call `process_all/4`. What it reuses are the core's state helpers (`increment_counter/2`, `item_field/3`, `append_output/3`, and `normalize_outputs/2`, which it lists explicitly as a predicate to compile). So the evidence is narrower than "the core is compiled": the core's data shapes and accessor predicates survive compilation to native code, for one test program on a four-line input. It is a smoke test, and it requires `clang`.

Why keep a model that does not run the product? Because a contract that exists only as generated IR is hard to inspect or argue with. A ten-line driver and sixteen executable examples let you state what `NR`, `$0`, and output ordering mean, and check those statements, before you read any LLVM. That is also where the design intends it to serve as the determinism evidence for the compile step (the implementation plan's Phase 0); treat that as the design's rationale, not as a delivered guarantee.

One caveat completes the picture, and it follows straight from the decoupling. Because the core and the compiler share no code, the core's tests passing says nothing about whether a compiled binary behaves as the model says — the third item in the list above. The natural way to close that gap would be a *differential* test: run one input through `process_all/4` and through the compiled binary and compare the outputs. No such test exists in the code this book was checked against. The one test that touches both sides, `test_plawk_compiled_stream_core.pl`, compiles the hand-written `plawk_stream_loop/3` that reuses the core's helpers, not `process_all/4`, and it asserts that the program builds and runs, not that it agrees with the interpreter. So "the compiled loop is meant to match the core" is a design intent — Phase 0 names the core as the determinism evidence for the compile step — and this chapter reports it as intent, not as a checked property. That distinction is the whole reason to read this chapter carefully: it is what keeps a green core test suite from being misread as a guarantee about the thing that actually runs your `.plawk` file.

## Next

Chapter 8: Foreign Prolog — `@prolog` blocks, `function` definitions, and calling Prolog from a rule. This is where Prolog stops being a reference model and starts being code compiled into the binary: the core stays a model, and the clauses in an `@prolog` block become part of the engine.
