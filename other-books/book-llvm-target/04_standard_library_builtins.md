<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)
-->

# The standard-library builtin surface (libc-backed)

This chapter is about the **WAM target** — `wam_llvm_target.pl`, entered through
`write_wam_llvm_project/3` — which compiles WAM bytecode to LLVM. It is a different
generator from the legacy, pattern-based `llvm_target.pl` that the earlier chapters
describe; keep the two apart. Beyond unification, arithmetic, and control, the WAM
target exposes a **standard-library surface** — clock, filesystem, environment,
process, random, and error builtins — implemented by calling into the C library.
This is what lets a compiled Prolog program (and, by extension, a plawk program that
bridges to Prolog) read the clock, list a directory, or run a shell command, without
an interpreter in the loop.

The names are deliberately **SWI-Prolog-compatible**: `getenv/2`, `shell/1`,
`get_time/1`, `make_directory/1`, `directory_files/2` are the same predicates SWI
offers from `library(system)` and `library(filesex)`. The same source therefore runs
under `swipl` for testing and compiles to native code here, which is the point of the
compatibility.

## How a name becomes a libc call

Three pieces connect a predicate call to a C function.

1. **A dispatch id.** `builtin_op_to_id(Name/Arity, Id)` maps each supported builtin
   to a small integer (`wam_llvm_target.pl`). For example
   `builtin_op_to_id('getenv/2', 99)`.
2. **A WAM instruction.** The compiler lowers a call to a known builtin into a single
   `builtin_call <Id>` instruction rather than a general predicate call.
3. **Per-builtin LLVM.** `wam_llvm_case('builtin_call', ...)` emits, for that id, the
   IR that marshals the WAM arguments, calls the C extern (the module `declare`s
   `@getenv`, `@system`, `@clock_gettime`, and the rest), and turns the result back
   into a WAM value — succeeding, failing, or binding an output argument as the
   builtin's contract says.

A worked example ties the three together: `getenv/2` is id `99`. A `getenv(Name,
Value)` call lowers to `builtin_call 99`; that id's arm in `wam_llvm_case('builtin_call',
...)` marshals `Name`, calls the `@getenv` C extern, and binds `Value` to the result
atom (or fails if the variable is unset).

So "the libc surface" is concretely the set of `builtin_op_to_id` entries whose
lowering calls a C-library function. There is no FFI for the program author to
declare: the binding is built into the target, and a program reaches it just by
calling the SWI-named predicate.

The full `builtin_op_to_id` table is larger than the libc surface — on the order of
170 entries, most of them pure string, list, term, arithmetic, and type-checking
builtins, with a catch-all id for anything unmapped. Every id has a matching arm in
the `@execute_builtin` dispatch. This chapter covers only the libc-backed subset; the
rest are the ordinary logic/term builtins documented elsewhere.

## The surface, by category

The contracts below are the per-builtin behaviour recorded in the source table.
Floats are WAM floats, integers are WAM integers, and a path or name is a Prolog atom.

**Time and clock.**

| Predicate | Behaviour | C backing |
|---|---|---|
| `get_time/1` | wall-clock seconds since the epoch, as a float | `clock_gettime` |
| `cpu_time/1` | process CPU time, as a float | `clock_gettime(CLOCK_PROCESS_CPUTIME_ID)` |
| `monotonic_time/1` | monotonic clock, as a float | `clock_gettime(CLOCK_MONOTONIC)` |
| `format_time/3` | format a timestamp (`strftime`) | `strftime` + `localtime` |
| `stamp_date_time/3` | timestamp → a `date/9` term | `localtime_r` |
| `date_time_stamp/2` | a `date/9` term → timestamp | `mktime` |
| `sleep/1` | sleep for *N* seconds | `usleep(N * 1e6)` |

**Filesystem.** `exists_directory/1`; `make_directory/1` (`mkdir`, mode `0o755`);
`delete_file/1` (`unlink`); `rename_file/2` (`rename`); `delete_directory/1`
(`rmdir`, empty directories only); `size_file/2` (`stat` → `st_size`); `time_file/2`
(`stat` → `st_mtime`, as a float); `working_directory/2` (`getcwd` for the old value,
`chdir` if the new value is an atom); `directory_files/2` (`opendir`/`readdir` → a
list of name atoms); `realpath/2`; `read_link/2` (`readlink`); `symlink/2`; `link/2`
(a hard link); `same_file/2` (compares `stat` device + inode); `access/2`; `chmod/2`;
`chown/3`; `truncate/2`; `mkfifo/2`; `umask/2`; `file_directory_name/2` (the
`dirname(1)` prefix); `read_file_to_atom/2` (`stat` + `open` + read loop, interned as
an atom).

**Environment.** `getenv/2` (the value as an atom, or **fail** if unset); `setenv/2`;
`unsetenv/1`.

**Process and system.** `shell/1` (`system`, succeeds iff the command exits `0`);
`shell/2` (binds `Status` to the decoded `WEXITSTATUS`, `0..255`); `getpid/1`,
`getppid/1`, `getpgrp/1`; `kill/2`; `gethostname/1`; `getlogin/1`; `getuid/1`,
`geteuid/1`, `getgid/1`, `getegid/1`; `nice/1`; `getpriority/1`, `setpriority/1`;
`getrlimit/2`, `setrlimit/2` (soft limit only); `process_max_rss/1` (`getrusage`
`ru_maxrss`, in KB on Linux); `process_user_time/1` (`ru_utime`, as a float);
`uname_sysname/1`, `uname_machine/1`; `halt/0`, `halt/1` (`exit`).

**Random.** `random/1` (`drand48`, in `[0, 1)`); `random_between/3` (`lrand48`);
`set_random/1` (`srand48`, seeded from a `seed(N)` term).

**Error.** `strerror/2` (`strerror(errno)` as an atom).

## Reaching the surface from plawk

plawk's own code generator declares none of these externs. Its CLI loads the
WAM-to-LLVM target (`examples/plawk/bin/plawk:27`) and delegates to it, so a plawk
program reaches the libc builtins the same way it reaches any Prolog: through an
`@prolog` block whose clauses call the SWI-named predicate. A rule that wants the
wall-clock time, a directory listing, or a shell exit status calls a bridged
predicate that, in turn, calls `get_time/1`, `directory_files/2`, or `shell/2`. The
builtin surface is therefore shared: it is defined once in the target and inherited
by every front end built on it.

## Scope of this chapter

This chapter documents the libc-backed entries of the `builtin_op_to_id` table as the
source defines them; each row's contract is the behaviour that table and its lowering
record. It does **not** survey per-builtin test coverage — which of these have a
passing build-and-run test is a separate question from which are defined — nor does it
claim portability beyond the Linux/`clang` baseline the examples assume (`ru_maxrss`
in KB, the `CLOCK_*` ids, and `umask` semantics are platform-specific).

**Native build only.** This surface belongs to the ordinary native target
(`write_wam_llvm_project/3`). The WebAssembly build (`write_wam_llvm_wasm_project/3`)
targets `wasm32` with a bump allocator and does **not** declare these C externs, so
the libc builtins described here are not available in a WASM module — a program meant
to run in the browser or a wasm sandbox cannot call `shell/1`, `directory_files/2`,
and the rest.

Treat the table as the exposed surface, and verify a specific builtin's behaviour on
your platform before relying on it.
