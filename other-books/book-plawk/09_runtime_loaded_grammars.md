<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 9: Runtime-loaded grammars: `DYNLOAD` and `dyncall`

> *Skeleton. Source tests: `tests/test_plawk_dyncall.pl`, `test_plawk_dyncall_at.pl`. This chapter is the most "design-adjacent" part of the surface; mark clearly what the tests cover.*

## Compiled vs loaded

- The compiled bridge (Chapter 8) fixes the Prolog at build time. `DYNLOAD = "file.wamo"` names a WAM object built ahead of time with `write_wam_object/3` and loaded lazily on the first `dyncall` (README).
- Terminology: the README calls this a JIT-like step; the execution-architecture doc states the system is AOT native loops plus WAM bytecode on a natively compiled interpreter, with no JIT yet. Use "runtime-loaded" and say why.

## `dyncall(args...)` and `dyncall_at(Source, args...)`

- `dyncall(Args)` and `dyncall_at(Source, Args)` parse to `dyncall`/`dyncall_at` (`plawk_parser.pl:1279-1297`); arity N+1 grammar predicates (N inputs, output last); i64 result, `0` on load or call failure.
- Worked example from the README: `square.wamo` swapped for a doubling grammar with no rebuild. <!-- TODO: reproduce this example end to end (needs a .wamo built with write_wam_object/3) before it is presented as run (recon follow-up) -->

## Cache policy: `DYNCACHE`

- `on` (default), `mtime`, `off`; trade-offs.

## The guard rail

- `dyncall` without `DYNLOAD` throws `plawk_dyncall_without_dynload` (`plawk_native_codegen.pl:644-646`); `dyncall` is a reserved form, never a compiled predicate.
