<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 4: Store & evidence tiers

Chapter 3 settled the model's shape — the two axes, the identity triple, the
driver built above a frozen core — but left its evidence grammar for here. This
chapter is that grammar: what the on-disk store records, line for line, and the
one idea the whole store exists to carry. That idea governs everything
downstream: evidence comes in tiers, and the tier decides whether *"the symbol is
not here"* is a fact or a shrug. Get that distinction wrong and the resolver
either vetoes a binary that would have run or blesses one that would crash; get it
right and every verdict in chapter 5 follows. The spine is still the same
`/bin/ls` from coreutils on an Ubuntu 22.04 box.

## 4.1 The store as a directory of facts

Chapter 3 described the store in the abstract — a handful of dynamic predicates
loaded atomically from disk. Concretely it is a directory of JSONL files, and
each file is one relation in **P/2** form: every line is a JSON `[key, value]`
pair, nothing more. The key is a flat atom and the value is a scalar or a small
JSON array. That uniform shape is what lets `load_abi_store/1` read any file the
same way — parse the line, split the key, hand `[K, V]` to the file's handler —
and it is why a store is just bytes on disk that a test fixture can write by hand.

The loading is all-or-nothing, as chapter 3 promised. `load_abi_store/1` first
clears whatever was there, then reads every row of every file; if a single row
fails to parse or validate, it clears the partial store again and re-throws. A
contradictory row — we will meet the canonical one in §4.2 — does not quietly
drop out and leave the rest standing; it brings down the whole load. The comment
in the code is the guarantee: callers never compute on half a load. Every query
runs against a complete, self-consistent store or against none at all.

Six files make up the store, one per question the resolver has to answer:

- `symprov.jsonl` — who provides what, and from what evidence (`symprov/4`).
- `symreq.jsonl` — who needs what (`symreq/5`).
- `needed.jsonl` — each binary's `DT_NEEDED` sonames (`needed/2`).
- `evidence.jsonl` — where the facts came from and how complete they are
  (`prov_evidence/4` for providers, `req_evidence/4` for requirers).
- `releases.jsonl` — the candidate axis, the actual releases to evaluate
  against (`release/3`).
- `replaces.jsonl` — declared soname succession (`replaces/2`).

These are the predicate names chapter 3 introduced, now anchored to the files
that fill them. The two-library store chapter 6 drives the CLI against is written
in exactly this schema, so the bytes you read here are the bytes you will see
again there.

## 4.2 Provider rows and their bounds

`symprov.jsonl` carries the provider facts, and it has two record shapes — one
per evidence tier — distinguished by a tag at the head of the value array:

```
["<soname>|<sym>@<node>", ["since", "<debver>", "<evidence-release>", <binding>]]   # .symbols tier
["<soname>|<sym>@<node>", ["at", "<evidence-release>", <binding>]]                  # readelf tier
```

The key flattens the identity triple from chapter 3: `<soname>|<sym>@<node>`, so
`libc.so.6|fopen@GLIBC_2.2.5` is one key. The value's first element says which
tier the row came from, and the rest is the bound.

A `since` row loads as `since(Min, MinAtom, R0, Bind)` and comes from a
`.symbols` list. It records two things at once: the symbol was present at the
evidence release `R0` the row was taken from, and — by the curated lower bound —
at every release `>= Min`. The emphasis belongs on *curated*. `Min` is not an
introduction date; it is the minimum version dpkg's metadata happens to promise,
and Debian policy explicitly lets a maintainer *raise* it. So a release `R < Min`
is reported `below_floor`, the conservative floor `dpkg-shlibdeps` would emit, and
not a claim that the symbol did not exist earlier — only that this evidence does
not reach below `Min`.

An `at` row loads as `at(R0, Bind)` and comes from `readelf`: a direct
observation of the export set at the single release `R0`, with no lower bound to
extrapolate because none is implied.

Between the two shapes sits an invariant the loader enforces: `Min =< R0`. A
curated minimum version can never exceed the release the row was curated *from* —
a `.symbols` list taken at `R0` cannot honestly promise a symbol only from some
*later* version. `assert_symprov` checks exactly this with `rel_le(Deb, R0)`, and
a `since` row that violates it is *contradictory*. This is the canonical bad row
of §4.1: it is rejected at load, and because loading is atomic, the whole store
fails to load rather than admitting a self-inconsistent fact.

The last field on every provider row is the binding, `default | nondefault |
unproven`. It is carried here, on the provider row, for the reason chapter 3's
unversioned-reference rule demands: an unversioned requirement binds only to a
`Base` export or to a *default* (`@@`) export, never to a hidden (`@`) one, so the
resolver has to know a symbol's default-version binding to decide whether it
satisfies such a reference. Keeping the binding on the same row that establishes
presence is what guarantees presence and binding never diverge — the resolver can
never credit a binding from a row other than the one that proved the symbol is
there.

## 4.3 The central distinction: complete vs curated

Everything so far — the two tiers, the two bounds — converges on the single
distinction this chapter exists to teach, and it lives in the evidence file:

```
["provides|<soname>", ["symbols"|"elf", "<release-id>", "complete"|"curated", "<source>"]]
```

Each such row loads as `prov_evidence(So, Src, R0, Status)`, and the `Status`
field takes exactly two values. The difference between them is the thesis of the
chapter:

- **complete** means the export set was *fully observed* at `R0` — either
  `readelf` on the actual ELF, or a `.symbols` file cross-checked against the ELF
  with `--elf` (the ingest rejects any disagreement). When the export set is fully
  observed, *absence from it is a fact*: a symbol that is not in a complete set
  genuinely is not exported.
- **curated** means a plain `.symbols` lower-bound list ingested *without*
  `--elf`. Here *presence is evidence but absence proves nothing*. A `.symbols`
  file is a curated list, not a census; a symbol it omits may simply never have
  been added to it. So an omitted identity is `unknown`, never `missing` and never
  `below_floor`.

That one rule is the hinge the rest of the resolver turns on. In the code it is a
single guard: when the resolver asks what an evidence row says about an identity,
it reports `absent` only when the row's status is `complete`; a `curated` row that
omits the identity yields no statement at all. The consequence is a clean worked
contrast. Take a symbol that is simply not present in the evidence for its soname.
Under **complete** evidence the verdict is `missing(Sym@Node)` — a hard veto,
because absence was observed. Under **curated** evidence the very same missing
symbol yields `unknown(Sym@Node, absent_from_incomplete_evidence(So))` — the
evidence is silent, so the resolver is too. Same symbol, same gap, opposite
answers, and the only thing that changed was the tier of the evidence. Chapter 5
is the elaboration of this one sentence into a verdict vocabulary.

## 4.4 Aggregating evidence across releases

A soname usually has more than one evidence row, taken at different releases, and
a verdict is about a *particular* release that may be none of them. `ident_status/5`
is the predicate that reconciles this: for one identity at one release it combines
*all* the usable evidence rows of the soname into a single status. The rule has a
clear priority. Evidence taken *at* the queried release decides directly — and
when both tiers were taken at that release, `readelf` (a direct observation) wins
over `.symbols` (curated metadata). Otherwise the resolver reaches for the nearest
evidence *below* the release and the nearest *above*, and combines the two.

Three outcomes are worth holding in mind, because they are where the
complete/curated distinction earns its keep:

- **Presence below extrapolates upward.** A symbol observed present at an earlier
  release is taken to still be present at a later one — the defeasible
  monotone-export assumption from chapter 1 — and the status carries the basis
  `extrapolated` so the conclusion is labelled as the inference it is.
- **Absence from a complete set above propagates downward.** If a *complete*
  export set at a later release does not contain the symbol, then under monotone
  exports it was absent at the queried release too, and the status is
  `missing(observed_absent(Src, R1))` — the inference is visible in the term.
- **Present below, observed absent above, is a drop.** When a present row below
  meets an *observed-absent* complete row above, the symbol was removed somewhere
  in between. The resolver refuses to guess in either direction: the status is
  `unknown(dropped_between(R0, Src, R1))` — neither a false `compatible` that
  would bless a binary against a release where the symbol is already gone, nor a
  false veto for the releases where it was still present.

The binding story across releases has its own corner cases — what happens when a
present row and a covering curated row disagree on whether a symbol is the default
export — and it is deliberately kept light here. The resolver marks such conflicts
`ambiguous` rather than committing to a confident answer; the reasoning is laid
out in `abi_resolve.pl`'s `combine/4` comments for a reader who wants it.

## 4.5 Requirement evidence and why it gates everything

Provider evidence is only half the store. The other half records what we know
about the *binary's* requirements, in the same evidence file:

```
["requires|<binary>", ["readelf", "complete"|"missing_file"|"readelf_failed"|"inconsistent", "<detail>"]]
```

This loads as `req_evidence(Bin, Src, Status, Detail)`, and its status gates the
entire verdict. Before the resolver examines a single symbol, it checks the
requirement evidence: if there is none, the verdict is
`unknown([no_requires_evidence(Bin)])`; if the requirement set is anything other
than `complete` — the file was missing, `readelf` failed, or the result was
internally inconsistent — the verdict is `unknown([requires_evidence(Status,
Detail)])`. Only once the requirements are known completely does per-symbol
reasoning begin.

The teaching point is the honesty chapter 1 promised, enforced right at the store
boundary. You cannot give a *hard* verdict on incomplete inputs: if you do not
reliably know what the binary needs, you cannot prove it is satisfied and you
cannot prove it is not. The resolver declines to pretend otherwise, and it
declines early — a hard `compatible` or `incompatible` is simply unreachable when
the inputs are incomplete, by construction rather than by a later check that might
be forgotten.

## 4.6 The release axis

The last file supplies the axis the verdicts range over:

```
["<soname>", "<debver>"]
```

Each row loads as `release/3`, naming one candidate release of a soname, and
`release_axis/2` returns them ascending and deduplicated. This is not a synthetic
grid; it is the set of releases that actually exist for the library, and it is the
set a `range` query walks in chapter 5. For `/bin/ls` on this machine the axis for
`libc.so.6` is the real two-release span `[2.35-0ubuntu3, 2.35-0ubuntu3.15]`.

One property of the axis matters enough to state now, because it is where the
floor and the axis meet. Extending the axis with a release *below* the floor does
not drag the compatible minimum down to it. A release below the floor comes back
`below_floor` — incompatible — so it never enters the compatible set the range is
read from, and the minimum of the range stays at the lowest release the evidence
actually supports (`2.34-0ubuntu3` for `libc.so.6`), never the lowest release
merely *listed*. The axis says which releases to ask about; the evidence tiers of
this chapter decide what the answer is.

With the store schema in hand and the complete-versus-curated distinction fixed,
the evidence is finally in a form the verdict predicates can consume. The next
chapter turns it into answers: how a verdict is derived and labelled with its
basis, how the floor is computed to match `dpkg-shlibdeps` exactly, and how the
compatible range is read off the real release axis.

## Next

Chapter 5: Verdicts, floor, range — turning this evidence into an answer with a
confidence label *(planned)*.
