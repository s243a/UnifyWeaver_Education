<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 3: Modeling in Prolog

> **Status: skeleton / planned.** Section-and-bullet outline for a later writing
> pass. It fixes the teaching targets and sources; the finished chapter will be
> narrative, building each predicate from the problem rather than listing the
> module's exports.

Goal of the chapter: show how the problem of chapter 2 becomes a small set of
Prolog relations over data, and why the design puts the ABI lane *above* the
existing package resolver instead of inside it. This is the "why declarative is
powerful" chapter promised in chapter 1.

## 3.1 Catalog-as-data, not database

- The governing idea (shared with the coarse resolver, see
  `examples/pkg_resolver/README.md`, "The model"): the facts are *data* passed to
  queries, not `assert`/`retract` into the Prolog database at large.
- For the ABI lane the facts live in dynamic predicates loaded from a store
  (`symprov/4`, `symreq/5`, `needed/2`, `replaces/2`, `prov_evidence/4`,
  `req_evidence/4`, `release/3` in `examples/pkg_resolver/abi/abi_resolve.pl`);
  the store is a directory, loaded atomically, cleared on any bad row.
- Teaching point: why "data, not code" is what makes the model portable and
  testable (bridge to chapters 4 and 7).

## 3.2 Representing the two axes

- Version node as an atom matched by equality; `Base`/`none` for unversioned.
- Package version as a `deb(Epoch, Upstream, Revision)` term produced by the
  Debian parser; a non-Debian release id kept as `label(Atom)` that only matches
  itself (`rel_term/2`).
- Ordering delegated to the frozen `resolver:version_lt/2` via `rel_lt/2` /
  `rel_le/2` — the lane never re-implements Debian version comparison.
- Worked micro-example: why `3.1~` sorts *below* `3.1`, and why that matters for a
  floor.

## 3.3 Identity: the (soname, symbol, node) triple

- `symprov(So, Sym, Node, Bound)` as "soname So exports Sym@Node, with evidence
  bound `Bound`"; `symreq(Bin, Sym, Node, So, Bind)` as "binary Bin requires it".
- Matching is exact on the triple; show how a requirement finds (or fails to
  find) its provider row.
- The unversioned case threaded through: a requirement with `Node = none` binds
  to a `Base` or default export in any `NEEDED` object.

## 3.4 A driver above a frozen core

- Design decision: `resolver.pl` and `resolver_store.pl` are *not edited*; the
  ABI lane sits on top and reuses only `version_lt/2` and the deb parser
  (`examples/pkg_resolver/abi/README.md`, intro).
- Why: the coarse resolver is contract-tested and compiled to many targets;
  freezing it keeps those guarantees intact while a new lane is added. Tie back
  to chapter 1's "one model, many targets".
- The three provider tiers as a layering story (coarse `Depends:` → `.symbols` →
  `readelf`), cheapest first.

## 3.5 Mode-safety and determinism as teaching moments

- Why `ident_status/5` computes into a fresh variable and then unifies with the
  caller's pattern (so a caller passing a partially-bound status cannot skip the
  clause the unbound call would fire) — a short, concrete lesson in a real
  Prolog hazard, drawn from the comments in `abi_resolve.pl`.
- Determinism at the API edge; why a single, mode-independent status matters for
  the differential in chapter 7.

## Next

Chapter 4: Store & evidence tiers — the on-disk facts, and why absence is only
sometimes a fact *(planned)*.
