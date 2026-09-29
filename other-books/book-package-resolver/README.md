<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Book: Package / ABI Dependency Resolver

A guide to modeling Debian-style package and ABI dependency resolution declaratively in Prolog (the `examples/pkg_resolver` app in UnifyWeaver), and compiling that model to many targets.

## Status: 🚧 Initial

Only Chapter 6 is written so far; the other chapters are planned.

## Contents

1. Introduction - *planned*
2. The resolution problem - *planned*
3. Modeling in Prolog - *planned*
4. Store & evidence tiers - *planned*
5. Verdicts, floor, range - *planned*
6. [The CLI & `explain`](06_the_cli_and_explain.md) - Driving the resolver from the command line and reading a verdict
7. Compiling to targets - *planned*
8. Pruning & scale - *planned*
9. Open problems - *planned*

## Prerequisites

- SWI-Prolog (`swipl`)
- A checkout of UnifyWeaver (the book refers to `examples/pkg_resolver/abi/`)
- Basic familiarity with Prolog facts and queries
- Helpful: a rough idea of shared-library sonames and ELF symbol versioning

## Quick Start

```bash
swipl -q -g main -t halt examples/pkg_resolver/abi/abi_cli.pl -- \
      <store-dir> explain <binary> <soname> <release>
```

Chapter 6 shows how to build a small store and run every command.

## See Also

- `examples/pkg_resolver/abi/README.md` (in UnifyWeaver) - the reference for the verdict grammar and store schema
- `examples/pkg_resolver/abi/SYMBOL_ABI_HOWTO.md` (in UnifyWeaver) - more usage
