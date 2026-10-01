<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 4: Store & evidence tiers

> **Status: skeleton / planned.** Section-and-bullet outline for a later writing
> pass. It fixes the teaching targets and the source schema; the finished chapter
> will be narrative, motivating each store file from a question the resolver has
> to answer. This is the chapter chapter 6 leans on for its tiny synthetic store.

Goal of the chapter: give the reader the store schema *and* the single idea that
governs everything downstream — that evidence comes in tiers, and that the tier
decides whether "the symbol is not here" is a fact or a shrug.

## 4.1 The store as a directory of facts

- Shape: a directory of P/2 JSONL files, each line a `[key, value]` pair
  (`examples/pkg_resolver/abi/README.md`, "Store shape"). Mirror chapter 6's
  running two-library store so the reader sees the same bytes twice.
- Atomic loading: a contradictory or malformed row throws and the partial store
  is cleared, so no query ever runs on half a load (`load_abi_store/1`).
- The six files and what each answers: `symprov` (who provides what, from where),
  `symreq` (who needs what), `needed` (`DT_NEEDED` sonames), `evidence` (where
  the facts came from), `releases` (the candidate axis), `replaces` (declared
  soname succession).

## 4.2 Provider rows and their bounds

- `since(Min, MinAtom, R0, Bind)` — from `.symbols`: present at evidence release
  R0 and, by the curated lower bound, at every release `>= Min`. Stress that
  `Min` is a *curated* lower bound, not an introduction date (Debian policy lets
  it be raised); `R < Min` is `below_floor`.
- `at(R0, Bind)` — from `readelf`: a direct observation at R0.
- The invariant `Min =< R0` and why a row violating it is contradictory and
  rejected at load time.
- `Bind` = default / nondefault / unproven, and why the binding is carried on the
  provider row (the unversioned-reference rule from chapter 3 needs it).

## 4.3 The central distinction: complete vs curated

- `prov_evidence(So, Src, R0, Status)` with `Status` ∈ {complete, curated}.
- **complete** = the export set was fully observed (readelf, or a `.symbols` file
  cross-checked against the ELF with `--elf`): *absence from it is a fact*.
- **curated** = a plain `.symbols` lower-bound list: *presence is evidence,
  absence proves nothing* — an omitted symbol is `unknown`, never
  `missing`/`below_floor`.
- This is the chapter's thesis sentence. Everything in chapter 5 is a consequence
  of it. Worked contrast: the same missing symbol under complete vs curated
  evidence gives `missing` vs `unknown`.

## 4.4 Aggregating evidence across releases

- `ident_status/5`: for one identity at one release, combine *all* usable
  evidence rows of the soname. Evidence *at* the release decides (readelf before
  `.symbols`); otherwise the nearest below and nearest above are combined.
- The three outcomes to teach: presence below *extrapolates upward*; absence from
  a *complete* set above *propagates downward*; a present-below row with an
  observed-absent-above row means the symbol was *dropped between* — `unknown`,
  neither a false compatible nor a false veto.
- Keep the binding story light here; point to `abi_resolve.pl`'s `combine/4`
  comments for the ambiguous-binding corner cases.

## 4.5 Requirement evidence and why it gates everything

- `req_evidence(Bin, Src, Status, Detail)`: if the requirement set is not
  `complete` (missing file, readelf failed, inconsistent), the verdict is
  `unknown` before any symbol is examined.
- Teaching point: you cannot give a hard verdict on incomplete inputs. This is
  the honesty that chapter 1 promised, enforced at the store boundary.

## 4.6 The release axis

- `releases.jsonl` as the candidate axis per soname; `release_axis/2` returns it
  ascending and deduplicated. This is the set of releases a `range` query walks
  (chapter 5).
- Note the real axis from `/bin/ls` (`[2.35-0ubuntu3, 2.35-0ubuntu3.15]`) and
  that an axis extended below the floor still yields the floor as the minimum,
  never the lowest release.

## Next

Chapter 5: Verdicts, floor, range — turning this evidence into an answer with a
confidence label *(planned)*.
