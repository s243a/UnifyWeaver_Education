<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 5: Verdicts, floor, range

> **Status: skeleton / planned.** Section-and-bullet outline for a later writing
> pass. It fixes the teaching targets and the grammar sources; the finished
> chapter will be narrative, deriving each verdict shape from the evidence of
> chapter 4. This is the chapter that defines every term chapter 6 prints.

Goal of the chapter: assemble the per-identity statuses of chapter 4 into the
three top-level outputs the resolver exists to produce — a *verdict*, a *floor*,
and a *range* — and explain the confidence labels that make them trustworthy.

## 5.1 Per-requirement status

- `req_status/5`: one status per requirement of a binary that concerns a soname
  (versioned requirements attributed via the version index, plus unversioned
  ones). From `examples/pkg_resolver/abi/abi_resolve.pl`.
- The status vocabulary: `provided(Sym@Node, Basis)`, `below_floor(Sym@Node,
  Min)`, `missing(Sym@Node[, Why])`, `unknown(Sym@Node, Why)`,
  `weak_unresolved(...)`. Each tied back to the chapter-4 evidence that produces
  it.
- The hypothetical `drop(Sym, Node, At)`: how a modelled removal exercises the
  upper bound of a range (used again in chapter 8).

## 5.2 Aggregating to a verdict

- `aggregate_statuses/2`: hard vetoes win, else unknowns, else compatible; the
  compatible basis is the *weakest* among the provided requirements.
- The verdict grammar (reference: `examples/pkg_resolver/abi/README.md`, "The
  model"):
  - `compatible(exact | curated | extrapolated)` — defeasible "structurally
    possible", labelled by how it was reached.
  - `incompatible([...])` — a hard veto, reachable *only* with complete,
    attributed evidence; reasons `missing`, `below_floor`, `soname_mismatch`.
  - `unknown([...])` — honest uncertainty, with a reason list.
  - `not_needed(So)` — not in `DT_NEEDED` and not a declared replacement.
- Teaching point: why the three confidence tiers on `compatible` matter —
  `exact` (readelf at this release), `curated` (`.symbols` metadata),
  `extrapolated` (the monotone-export assumption). This is the defeasible
  reasoning of chapter 1 made into a label on the answer.

## 5.3 The gatekeeping order

- Walk `abi_verdict/5`'s order of checks: no requirement evidence → unknown;
  incomplete requirement evidence → unknown; soname mismatch → incompatible; not
  needed → not_needed; no provider evidence → unknown; otherwise aggregate.
- Why the order is itself part of the semantics (a mismatch outranks a missing
  symbol, etc.).

## 5.4 The floor

- `abi_floor/3`: the highest `.symbols` minimum among the provider rows matched
  by the binary's requirements — i.e. the same number `dpkg-shlibdeps` emits.
- Fails (prints `none`) when a requirement has no `since` provider row; explain
  why that is the honest result, not a bug.
- Payoff: the `/bin/ls` floors (`libc.so.6` = 2.34, `libselinux.so.1` = 3.1~)
  reproduce coreutils' declared `Pre-Depends` exactly — close the loop opened in
  chapter 1.

## 5.5 The range

- `abi_range/*`: evaluate the verdict at every release on the real axis, then
  report `range(Min, Max, Pairs)` where both ends are compatible by construction;
  or `no_candidate` / `unknown` / `no_releases`.
- Reading a range: `1.0: compatible(curated)`, `1.1: compatible(extrapolated)` —
  the per-release confidence labels tell the story the single floor cannot.
- The hypothetical-drop range as the bridge to chapter 8 (removing a symbol caps
  the range; removing a load-bearing one gives `no_candidate`).

## 5.6 Hand-off to the CLI

- Everything in chapters 2–5 is now a predicate returning one of these terms. The
  reader is ready for chapter 6, which is the thin command-line driver over
  exactly these queries and the `explain` command that renders the terms as
  English.

## Next

Chapter 6: The CLI and `explain` *(written)* — drive the resolver and read a
verdict out loud.
