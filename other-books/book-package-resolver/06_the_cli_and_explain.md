<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 6: The CLI and `explain`

`abi_cli.pl` is a small callable driver over the ABI resolver. It loads a store of facts, asks the resolver one question, and prints the answer. This chapter runs each command against a tiny synthetic store, with special attention to `explain`, which turns a verdict into readable sentences.

## Prerequisites

- SWI-Prolog
- A UnifyWeaver checkout; the driver lives at `examples/pkg_resolver/abi/abi_cli.pl`
- For the vocabulary used in the output (`below_floor`, `missing`, `unknown`, ...), the reference is `examples/pkg_resolver/abi/README.md`. Its "The model" section documents the verdict grammar and its "Store shape" section documents the store schema. For more usage see `examples/pkg_resolver/abi/SYMBOL_ABI_HOWTO.md`.

## Running the driver

```bash
swipl -q -g main -t halt abi_cli.pl -- <store-dir> <cmd> <args...>
```

The examples below run from the `examples/pkg_resolver/abi/` directory, so the script is written as the bare `abi_cli.pl`. `main/0` reads the command-line arguments after `--`: the first is the store directory, the second the command, and the rest are that command's arguments. It loads the store with `load_abi_store/1` and dispatches to a `run/2` clause. Unknown commands, or a known command with the wrong argument shape, print a usage line to stderr and exit with status 2.

In the examples, `$ ...` stands for that same `swipl ... -- <store>` prefix, and `<store>` for the store directory.

## Building a synthetic store

A store is a directory of JSONL files; each line is a two-element `[key, value]` array. The store used here has two libraries. `liba.so.1` exports `foo@LIBA_1` from release 1.0, and `libb.so.1` exports `bar@LIBB_1` only from release 2.0. A binary `mybin` needs both.

`symprov.jsonl` says which release a symbol is provided from (the `.symbols` "since" tier):

```
["liba.so.1|foo@LIBA_1", ["since", "1.0", "1.0", "default"]]
["libb.so.1|bar@LIBB_1", ["since", "2.0", "2.0", "default"]]
```

`symreq.jsonl` says which symbols the binary requires, and from which soname:

```
["mybin|foo@LIBA_1", ["liba.so.1", "GLOBAL"]]
["mybin|bar@LIBB_1", ["libb.so.1", "GLOBAL"]]
```

`needed.jsonl` lists the binary's `DT_NEEDED` sonames:

```
["mybin", "liba.so.1"]
["mybin", "libb.so.1"]
```

`evidence.jsonl` records where the facts came from: the two providers have `curated` `.symbols` evidence, and the binary's requirements were fully read with `readelf`:

```
["provides|liba.so.1", ["symbols", "1.0", "curated", "test-fixture"]]
["provides|libb.so.1", ["symbols", "2.0", "curated", "test-fixture"]]
["requires|mybin", ["readelf", "complete", "ok"]]
```

`releases.jsonl` is the candidate release axis for each soname:

```
["liba.so.1", "1.0"]
["liba.so.1", "1.1"]
["libb.so.1", "2.0"]
```

Finally `replaces.jsonl` (declared soname successions) is created empty. The script `examples/pkg_resolver/abi/test_explain_cmd.sh` builds exactly this store in a temporary directory with here-documents and asserts on the `explain` output, so it is a runnable version of this section.

## The commands

### `verdict <binary> <soname> <release> [DropSym DropNode DropAt]`

Asks the resolver whether `<binary>`'s requirements on `<soname>` are satisfied at `<release>`, and prints the verdict term as-is (with `~q`, so it reads back as Prolog). The optional three drop arguments model a hypothetical removal of a symbol; they are described in the abi `README.md`.

```
$ ... verdict mybin libb.so.1 1.0
verdict mybin libb.so.1 1.0: incompatible([below_floor(bar@'LIBB_1','2.0')])
```

The symbol `bar` is only provided from 2.0, so at 1.0 the verdict is `incompatible`.

### `status <binary> <soname> <release>`

Prints one line per requirement, using the per-requirement status the verdict is built from (provided, missing, and so on).

```
$ ... status mybin libb.so.1 1.0
  below_floor(bar@'LIBB_1','2.0')
```

### `floor <binary> <soname>`

The curated lower bound implied by the `.symbols` data: the earliest release that satisfies all of the binary's *versioned* requirements on that soname. This is the same figure `dpkg-shlibdeps` would put in a dependency. It is a dependency minimum, not a proof of absence below it — stronger evidence can still make a lower release compatible — and it does not settle unversioned requirements. If a requirement has no `since` provider row, it prints `none` with an explanation instead.

```
$ ... floor mybin liba.so.1
floor mybin liba.so.1: 1.0
```

### `axis <soname>`

The ingested release candidates for a soname, ascending. It does not depend on any binary.

```
$ ... axis liba.so.1
axis liba.so.1: [1.0,1.1]
```

### `range <binary> <soname> [DropSym DropNode DropAt]`

The verdict at each release on the axis, summarized as the span from the lowest to the highest release at which the binary is compatible, followed by one line per release. If no range exists the header shows the kind of result (`no_candidate`, `unknown`, `no_releases`).

```
$ ... range mybin liba.so.1
range mybin liba.so.1: [1.0, 1.1]
  1.0: compatible(curated)
  1.1: compatible(extrapolated)
```

Release 1.0 is `curated` because it rests on `.symbols` metadata, and 1.1 is `extrapolated` because presence is assumed to persist to later releases within a soname.

### `report <binary>`

A one-shot summary: for each `NEEDED` soname (in ascending soname order) it prints the floor, the axis, and the range, so you do not need to repeat `floor`, `axis` and `range` by hand.

```
$ ... report mybin
liba.so.1:
  floor: 1.0
  axis:  [1.0,1.1]
  range: range('1.0','1.1',['1.0'-compatible(curated),'1.1'-compatible(extrapolated)])
libb.so.1:
  floor: 2.0
  axis:  [2.0]
  range: range('2.0','2.0',['2.0'-compatible(curated)])
```

If the binary has no evidenced `NEEDED` sonames it prints `report <binary>: no NEEDED sonames evidenced`.

## `explain`: verdicts for humans

`explain <binary> <soname> <release> [DropSym DropNode DropAt]` computes exactly the same verdict as `verdict`, with the same arguments. It differs only in presentation. The header line names the verdict (`incompatible`, `unknown`, or a compatible/not-needed verdict as-is), and then there is one indented, human-readable line for each reason inside an `incompatible([...])` or `unknown([...])` verdict.

```
$ ... explain mybin libb.so.1 1.0
explain mybin libb.so.1 1.0: incompatible
  symbol bar (version node LIBB_1) first appears in release 2.0 (below_floor)
```

Compare with the `verdict` output for the same question:

```
verdict mybin libb.so.1 1.0: incompatible([below_floor(bar@'LIBB_1','2.0')])
```

The term `below_floor(bar@'LIBB_1','2.0')` is `below_floor(Sym@Node, Min)`: the curated `.symbols` metadata guarantees symbol `bar` under version node `LIBB_1` only from release `Min` = 2.0 — a dependency *floor*, which a maintainer may have set at or above where the symbol physically first appeared — and 2.0 is above the release we asked about, so (absent stronger evidence) it is below the floor. `explain` unpacks that term into a sentence. The other reasons are handled the same way: `missing(...)` (absent at this release, removed by a hypothetical drop, or observed absent at a later release), `unknown(Sym@Node, Why)`, and `soname_mismatch(offered(O), needed(N))`. A reason with no dedicated sentence is printed as the raw term, so nothing is hidden. The full grammar of these terms is in the abi `README.md`.

When the verdict is compatible there are no reasons, so `explain` prints only the header. Asking about `liba.so.1` at 1.0 in this store gives a header with no indented lines, which is what `test_explain_cmd.sh` asserts:

```
explain mybin liba.so.1 1.0: compatible(curated)
```

For a compatible verdict the header keeps the evidence tier (`exact`, `curated` or `extrapolated`); only `incompatible` and `unknown` headers are reduced to the bare kind, because their detail moves into the reason lines.

## Summary

| Command | Question it answers |
|---------|---------------------|
| `verdict` | Is the binary compatible with this release? (raw term) |
| `explain` | The same, with one readable line per reason |
| `status` | Per-requirement status at this release |
| `floor` | What is the lowest release that satisfies the requirements? |
| `axis` | Which releases are known for this soname? |
| `range` | Which releases are compatible, and on what evidence? |
| `report` | `floor`, `axis` and `range` for every `NEEDED` soname |

## Next

Chapter 7 covers compiling the resolver to other targets. For the definitions behind every term shown here, read `examples/pkg_resolver/abi/README.md` and `examples/pkg_resolver/abi/SYMBOL_ABI_HOWTO.md`.
