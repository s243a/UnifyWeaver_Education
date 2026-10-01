<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Book: Package / ABI Dependency Resolver

A guide to modeling Debian-style package and ABI dependency resolution declaratively in Prolog (the `examples/pkg_resolver` app in UnifyWeaver), and compiling that model to many targets.

## Status: 🚧 In progress

Chapters 1-3 and Chapter 6 (the CLI reference) are written; Chapters 4-5 are
detailed skeletons awaiting a writing pass; 7-9 are planned. Appendix A (how the
loader finds a library) is written.

## The arc

The book moves in a line from the problem to the model to the machinery. Chapters
1-2 motivate: *why versioned ABI resolution is hard and what a verdict has to
decide.* Chapters 3-5 build the declarative model: *the two axes as data, the
evidence store, and the verdict/floor/range the resolver produces.* Chapter 6
drives it from the command line. Chapters 7-9 widen out: *compiling the one model
to many targets, making it scale, and the open edges.*

## Contents

1. [Introduction](01_introduction.md) - Why ABI resolution is hard, the Debian `.symbols`/`dpkg-shlibdeps` grounding, and what one declarative model compiled to many targets buys you - *written*
2. [The resolution problem](02_the_resolution_problem.md) - Sonames, versioned symbols, the loader's exact-match rule, and the gap between what `ldd`/`dpkg-shlibdeps` answer and what we need - *written*
3. [Modeling in Prolog](03_modeling_in_prolog.md) - The two axes as data, catalog-as-facts, and a driver built above the frozen package resolver - *written*
4. [Store & evidence tiers](04_store_and_evidence_tiers.md) - The JSONL store, and *complete* vs *curated* evidence - why absence is only sometimes a fact - *skeleton*
5. [Verdicts, floor, range](05_verdicts_floor_range.md) - Deriving and labelling a verdict, the floor that matches `dpkg-shlibdeps`, and the compatible range - *skeleton*
6. [The CLI & `explain`](06_the_cli_and_explain.md) - Driving the resolver from the command line and reading a verdict - *written*
7. Compiling to targets - *planned*
8. Pruning & scale - *planned*
9. Open problems - *planned*

- [Appendix A: How the loader finds a library](appendix_a_the_loader_and_the_cache.md) - `ldd` vs `ldconfig` vs the loader, the soname→file mapping, the cache, overriding it, and why we reason statically - *written*

## Prerequisites

- SWI-Prolog (`swipl`)
- A checkout of UnifyWeaver (the book refers to `examples/pkg_resolver/abi/`)
- Basic familiarity with Prolog facts and queries
- Helpful: a rough idea of shared-library sonames and ELF symbol versioning (Appendix A covers this from the ground up)

## Quick Start

```bash
swipl -q -g main -t halt examples/pkg_resolver/abi/abi_cli.pl -- \
      <store-dir> explain <binary> <soname> <release>
```

Chapter 6 shows how to build a small store and run every command.

## See Also

- `examples/pkg_resolver/abi/README.md` (in UnifyWeaver) - the reference for the verdict grammar and store schema
- `examples/pkg_resolver/abi/SYMBOL_ABI_HOWTO.md` (in UnifyWeaver) - more usage
