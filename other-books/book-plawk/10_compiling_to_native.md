<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 10: Compiling to native: WAM to LLVM

Every earlier chapter used the compiled path without opening it. This one opens it: what the command line does, the stages between a `.plawk` file and an executable, and what the intermediate LLVM looks like. The claim to check is "compiled, not interpreted". The evidence is the generated IR, which is plain text you can read.

Sources: `examples/plawk/bin/plawk` (219 lines), `codegen/plawk_native_codegen.pl`, and `src/unifyweaver/targets/wam_llvm_target.pl`; for the target itself see the sibling LLVM book (`../book-llvm-target/`).

## The CLI

```text
plawk build FILE.plawk [-o OUT] [--keep-ll]
plawk run   FILE.plawk [INPUT ...]
```

`bin/plawk` is itself a SWI-Prolog script. It loads three modules, the parser, the native codegen and the WAM-to-LLVM target (`bin/plawk:25-27`), so it needs `swipl` and `clang` on the machine. `build` writes a native binary; the default output name is the source name with `.plawk` stripped (`default_out/2`, `:76-81`). `run` builds into a per-process temporary directory and executes the binary with the remaining arguments, returning the program's own exit status (`:41-49`).

The produced binary follows awk's input convention: it reads the file named by `argv[1]`, treats `-` as stdin, and with no argument reads stdin. You can see that in the emitted `main` below (`have_arg`, `@.wam_stream_stdin_dash`, `strcmp`).

The compiler's own exit codes distinguish where it stopped:

| code | meaning | where |
|---|---|---|
| 0 | success | `:33-40` |
| 2 | no such file, or parse error, or an `@prolog` block containing directives | `:84-108` |
| 3 | compile error, program outside the compilable surface, or a called Prolog predicate that nothing defines | `:122-142`, `:185-206` |
| 4 | `clang` failed | `:157-161` |

The check for undefined predicates (`check_foreign_calls/3`) exists because the bridge from Chapter 8 references `@<name>_start_pc` directly; without it a typo would surface as a raw linker error rather than a plawk message.

## The compilation path

`build/3` (`bin/plawk:83-161`) is the whole pipeline, in this order:

1. **Parse.** `plawk_parse_source/3` returns two things: an AST `program(Begin, Rules, End)` and a list of Prolog clauses (from `@prolog` blocks and desugared `function` definitions). A parse failure exits 2.
2. **Assert the clauses.** `plawk_prolog_block_preds/2` loads them into module `user`. A program with no Prolog at all gets a placeholder `user:plawk_cli_marker/0`, because the target wants at least one predicate (`:29-31`, `:100-101`). That placeholder is why even a pure-awk build prints one `WAM fallback` line.
3. **Compile the Prolog to WAM and LLVM.** `write_wam_llvm_project/3` writes a complete LLVM module (the WAM runtime, atom table, and one function per predicate) to a `.ll` file. Afterwards `wam_llvm_last_compile_counts/2` supplies `InstrCount` and `LabelCount`.
4. **Generate the driver.** `plawk_program_native_driver_ir/4` takes the AST plus `[wam_vm(InstrCount, LabelCount)]` and returns the IR text of the native `main` and its helpers. If the AST is not in the compilable surface this fails, and the CLI says so and exits 3 (`:128-135`).
5. **Append and link.** The driver IR is appended to the same `.ll` file (`:143-146`) and the CLI runs `clang -w -O2 FILE.ll -o OUT -lm` (`:147`). Without `--keep-ll` the `.ll` is deleted afterwards; with it, it is kept next to the output as `OUT.ll`.

The compiler narrates on standard output while it works, so the CLI captures that into a string (`with_output_to`, `:123-126`) to keep stdout clean for the compiled program. The `WAM fallback` notes arrive on standard error and are build diagnostics.

If the program uses `dyncall` or `dyncall_at` (Chapter 9), step 3 also gets `emit_wamo_loader(true)` so the loader is compiled into the host module (`:116-121`).

Notice what is absent: `plawk_core.pl`, the interpreter of Chapter 7, is not loaded by the CLI and takes no part. The compiled path and the Prolog reference core share a design, not code.

## Direct emission versus delegation

The driver is assembled from two sources (recon Q3; `plawk_native_codegen.pl:67-87, 3940-3980`).

plawk emits itself: pattern guards, string/integer/float conversions, `printf` format globals, the scalar state carried as `phi` nodes, associative tables, and the foreign-call shims of Chapter 8.

It delegates the stream framing to the target: `llvm_emit_stream_driver_ir/3` for text lines, `llvm_emit_binary_stream_driver_ir/4` for fixed-width binary records, and `llvm_emit_varlen_stream_driver_ir/5` for variable-length and tagged-union records. It also reuses the target's guard and `printf` emitters (for example `llvm_emit_atom_prefix_guard/5`, `llvm_emit_regex_field_match_guard/7`, `llvm_emit_printf_i64/5`). The practical consequence is that the binary-record chapters (4 to 6) and the plain text loop share one framing implementation with the rest of the UnifyWeaver LLVM target, rather than each re-implementing it.

## Reading the generated IR

A small, demonstrated example. This is the Chapter 1 shape, reduced to a counting program:

```awk
{ total++ }
$1 == "ERROR" { errors++ }
END { print "total", total, "errors", errors }
```

Built with `plawk build t.plawk -o t --keep-ll` and run on three lines (`ERROR a`, `INFO b`, `ERROR c`), it printed `total 3 errors 2`. The build took about three seconds on the author's checkout. The kept `t.ll` is 20,891 lines with 174 function definitions, nearly all of it the WAM runtime and atom table that every plawk binary carries; `plawk_cli_marker/0` is the only compiled predicate, and `main` is the last function in the file. What follows is excerpted from that `main` (blank lines and the end-block `printf` calls trimmed).

**Input selection.** The `argv[1]`-or-stdin convention, as machine code:

```llvm
define i32 @main(i32 %argc, i8** %argv) {
entry:
  %have_arg = icmp sgt i32 %argc, 1
  br i1 %have_arg, label %check_argv_path, label %use_stdin
...
use_stdin:
  %stdin_handle = call %Value @wam_stream_open_fd_value(i64 0)
  br label %have_handle
```

**The loop and its state.** The two counters are not memory cells. They are `phi` nodes at the loop head, so LLVM sees them as ordinary SSA values:

```llvm
loop:
  %slot_0 = phi i64 [0, %check_handle_value], [%next_slot_0, %continue_loop]
  %slot_1 = phi i64 [0, %check_handle_value], [%next_slot_1, %continue_loop]
  %line = call %Value @wam_stream_read_line_transient_value(%Value %handle)
```

**Rule 0, `{ total++ }`.** No pattern, so the guard is the constant `true`; the action is one `add`:

```llvm
rule_0_apply:
  %rule_0_body_slot_1_op_0 = add i64 %slot_1, 1
```

(`slot_1` is `total`, `slot_0` is `errors`.)

**Rule 1, `$1 == "ERROR"`.** The pattern is a call to a runtime helper with the literal's global, its length (5) and the field separator byte (32, a space):

```llvm
@.plawk_5Fsurface_5Frule_5F1 = private constant [6 x i8] c"ERROR\00"
...
  %rule_1_is_match = call i1 @wam_atom_field_eq_value(%Value %line, i64 1, i8* %plawk_5Fsurface_5Frule_5F1_ptr, i64 5, i8 32)
  br i1 %rule_1_is_match, label %rule_1_apply, label %continue_loop
```

The literal is a compile-time constant in the module, not a string parsed at run time. Each rule's output state feeds the next rule's `phi`, and `continue_loop` joins them and branches back to `loop`.

**End of input.** `check_eof` compares the line against an EOF sentinel and falls to `close_stream` and `end_print`, which `printf` the final `slot` values and `ret i32 0`. The failure labels return fixed codes from the compiled program: 10 for open failure, 11 for a read error, 12 for a bad line tag, 16 for a close failure.

Two honest limits on this tour. It shows one small text program; the tag switch of Chapter 5, the typed loads of Chapter 4 and the associative-table walk (`wam_assoc_i64_iter_next`) come from the delegated binary and assoc emitters and are not excerpted here. <!-- TODO: add excerpts from a --keep-ll build of a tagged-union program and of a counts[$1]++ program before this chapter claims their IR shape -->. And the committed `examples/plawk/generated/*.ll` files are probes of the Prolog core and reader (`plawk_core_probe.ll`, `plawk_loop_probe.ll`, and so on, about 18,000 lines each), not plawk driver output, so the excerpts above come from a fresh build, not from that directory.

Regex patterns are described in the source as compile-time constants with a `regcomp` per site. <!-- TODO: not verified in this chapter; confirm against a --keep-ll build of a regex-guard program (`llvm_emit_regex_field_match_guard/7`) before stating the caching behavior as fact -->

## Demonstrated and not

Demonstrated: the front door. A `.plawk` file goes in and a native executable comes out, through the five stages above, and the pure-awk program of Chapter 1 builds, runs and matches gawk (`total 5 errors 3 ERROR-lines 3`). The counting program in this chapter was built and run as described. Not demonstrated here: performance relative to awk (no measurement was made for this chapter), and the IR of the binary and tagged-union drivers.
