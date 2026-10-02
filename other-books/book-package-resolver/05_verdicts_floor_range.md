<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 5: Verdicts, floor, range

Chapter 4 ended on a distinction that governs everything downstream: when a
library's evidence is *complete*, absence of a symbol is a fact; when it is
merely *curated*, absence proves nothing at all. That distinction is a property
of individual identities. A user does not ask about an identity, though. They ask
about a binary against a library at a release, and they want three answers: a
*verdict* — can this run? — a *floor* — what is the earliest release that will
do? — and a *range* — across the releases that actually exist, which ones are
safe? This chapter is how the per-identity statuses of chapter 4 are assembled
into those three outputs, and how each output carries a label that says how much
to trust it. It is also the chapter that defines every term chapter 6 prints. The
spine stays the same `/bin/ls` from coreutils on an Ubuntu 22.04 box.

## 5.1 Per-requirement status

A binary does not have one requirement on a library; `/bin/ls` has **112
versioned plus 3 unversioned** references against `libc.so.6` and
`libselinux.so.1`. Before any verdict can be formed, each of those references has
to be resolved on its own. That is the job of `req_status/5`, which enumerates
exactly one status per requirement of a binary that concerns the queried soname —
the versioned requirements, attributed to their soname through the ELF version
index, plus the binary's unversioned references, which the loader resolves
against any `NEEDED` object.

Each status is the chapter-4 identity verdict dressed in the requirement's own
terms. A versioned requirement that resolves cleanly becomes
`provided(Sym@Node, Basis)`, where `Basis` is the trust label carried up from the
evidence — `exact` from a readelf observation, `curated` from a `.symbols` lower
bound, `extrapolated` from the monotone-export assumption. A requirement whose
only provider is a curated floor above the queried release becomes
`below_floor(Sym@Node, Min)`: the symbol is known from `Min` onward and the
release asked about is older. A requirement that a complete export set positively
rules out becomes `missing(Sym@Node)`, or `missing(Sym@Node, Why)` when the
absence was inferred from a later release rather than observed at this one. And a
requirement the evidence cannot settle becomes `unknown(Sym@Node, Why)` — the
honest outcome whenever the soname has evidence but none of it speaks to this
identity, or no usable provider evidence at all.

One status is special because it never vetoes. A *weak* reference — the three
unversioned weak symbols in `/bin/ls` are the real example — that fails to
resolve becomes `weak_unresolved(...)` rather than a missing. The loader does not
abort on an unresolved weak symbol, so neither does the resolver; the status
records the fact without letting it decide a verdict.

The per-requirement layer is also where the model's one hypothetical lives. A
`drop(Sym, Node, At)` term asks the resolver to pretend a symbol was removed at a
given release — an in-soname deletion that the monotone-export assumption says
cannot happen. When a requirement names a dropped identity, its status becomes
`missing(Sym@Node, hypothetical_drop)`. A single modelled removal is a small
thing here; its payoff is at the other end of the chapter, where it caps a range,
and again in chapter 8, where it becomes a lever for reasoning about scale.

## 5.2 Aggregating to a verdict

With one status per requirement in hand, `aggregate_statuses/2` collapses the
list into a single verdict for the `(binary, soname, release)` question. The rule
is a strict precedence. Any hard veto wins outright: if even one requirement is
`missing` or `below_floor`, the verdict is `incompatible`, carrying the full list
of offending reasons. Failing a veto, any `unknown` wins: the verdict is
`unknown` with its own reason list, because a single unsettled requirement is
enough to make the whole answer unsettled. Only when every requirement is
provided — no vetoes, no unknowns — is the verdict `compatible`.

The confidence label on a `compatible` verdict is the *weakest* basis among the
requirements that produced it, and the reason is a chain-and-link argument. A
verdict is only as trustworthy as its least-trustworthy step, so if any
requirement rested on the monotone-export assumption the whole verdict is
`compatible(extrapolated)`; if none did but some rested on `.symbols` metadata it
is `compatible(curated)`; and only when every requirement was observed directly by
readelf at exactly this release is it `compatible(exact)`. The three tiers are not
cosmetic. They are the defeasible reasoning of chapter 1 made into a label on the
answer: `exact` is a direct observation, `curated` is curated metadata, and
`extrapolated` is a conclusion good enough to act on but held only until harder
evidence overturns it.

The full grammar, as the lane's README fixes it, is four shapes:

```
compatible(exact | curated | extrapolated)
incompatible([ missing(Sym@Node)
             | missing(Sym@Node, observed_absent(Src, R1))
             | missing(Sym, no_default_export(So, Node))
             | below_floor(Sym@Node, Min)
             | soname_mismatch(offered(So), needed(N)) ])
unknown([ no_requires_evidence(Bin) | requires_evidence(Status, Detail)
        | no_provider_evidence(So) | unknown(Sym@Node, Why) | ... ])
not_needed(So)
```

Two properties of this grammar are worth naming. The first concerns when the
resolver will say "no" at all. A `missing` veto — a symbol *physically absent* — is
reachable only with complete, attributed evidence; the resolver will never emit a
confident "absent" from a curated list that merely failed to mention a symbol. A
`below_floor` veto is a different animal: it rests on the curated `.symbols`
dependency minimum, so it *can* come from curated evidence alone — but it is
defeasible. It says only "the declared dependency minimum is above the release you
asked about," not "the symbol is physically absent below it," and a direct
observation of the symbol below that floor overrides it (you will see
`compatible(exact)` win). The second is that every verdict carries its
reasons as a term, not a bare boolean, which is exactly what lets chapter 6's
`explain` command read a verdict out loud without re-deriving it.

## 5.3 The gatekeeping order

`abi_verdict/5` does not jump straight to aggregation. It runs a fixed sequence
of gates first, and the order of those gates is itself part of the semantics —
each gate answers a question that must be settled before the next one is even
meaningful.

It asks, in order: Is there any requirement evidence for this binary at all? If
not, the answer is `unknown([no_requires_evidence(Bin)])` — with nothing to
resolve, there is nothing to be compatible *with*. Is that requirement evidence
complete? If the binary's symbols could not be read fully, the answer is
`unknown([requires_evidence(Status, Detail)])` rather than a guess over a partial
list. Does the offered soname even match what the binary needs? A declared
succession like `replaces(libc.so.7, libc.so.6)` turns an offer of `libc.so.7`
against a binary needing `libc.so.6` into `incompatible([soname_mismatch(...)])`.
Is the soname needed at all? If it is neither in `DT_NEEDED` nor a declared
replacement for something that is, the verdict is `not_needed(So)` and the
question simply does not apply. Is there any usable provider evidence for the
soname? If not, `unknown([no_provider_evidence(So)])`. Only when all of those
gates pass does the resolver gather the per-requirement statuses and aggregate.

The order encodes priorities that would otherwise be ambiguous. A soname
mismatch outranks a missing symbol, because if the generations do not match there
is no point enumerating which symbols are absent — the loader would never pair the
two libraries in the first place. A `not_needed` outranks everything below it, so
offering an unrelated library never produces spurious missing-symbol noise. The
sequence turns a pile of independent checks into a single well-defined function.

## 5.4 The floor

The floor is the one number this whole machine was built to reproduce, because it
is the number the existing tooling already emits. `abi_floor/3` computes it as the
highest `.symbols` minimum-version among the provider rows matched — exactly, by
node — by the binary's requirements on the soname. That is, for every versioned
requirement it finds the curated lower bound of the symbol it needs, and the floor
is the largest of those bounds: the earliest release at which *all* of them are
guaranteed present. This is precisely what `dpkg-shlibdeps` does when it turns a
binary's symbols into a `libc6 (>= 2.34)` dependency line.

The predicate *fails* — and the CLI reports `none` — when any versioned
requirement on the soname has no `since()` provider row to supply a bound, whether
because the symbol is genuinely missing or because the only evidence is
readelf-only with no curated minimum. That failure is the honest result, not a
bug. A floor is a claim about every release at and above some version; without a
lower bound for even one required symbol, there is no version the resolver can
honestly name as the bottom of the compatible set, and inventing one would be the
guessing the whole design refuses.

Here is where the loop opened in chapter 1 closes. Asked for the floors of
`/bin/ls`, the resolver computes

```
libc.so.6        (>= 2.34)
libselinux.so.1  (>= 3.1~)
```

— the same `libc6 (>= 2.34), libselinux1 (>= 3.1~)` that coreutils actually
declares as its `Pre-Depends`. The `3.1~` is the detail that proves the axis is
real and not a numeric coincidence: the `~` sorts *before* everything, so `3.1~`
is one tick below `3.1`, and only the full Debian version ordering — delegated to
the frozen comparator of chapter 3, never reimplemented here — gets it right.
Reproducing a number you could already read off the package metadata is the point:
it shows the model, from the same evidence, agreeing with the tool everyone
trusts, before it goes on to compute the things the metadata does not carry.

## 5.5 The range

The floor names the bottom of the compatible set and nothing else. The range
characterises the rest of it across the real world — but as a pair of compatible
endpoints plus the full per-release detail, not as a guaranteed-contiguous band.
`abi_range` evaluates the
verdict at every release on the *actual* candidate axis — the releases the store
was told exist, not a synthetic sweep — and reports `range(Min, Max, Pairs)`,
where `Pairs` is every release paired with its verdict and both `Min` and `Max`
are, by construction, releases whose verdict came back compatible. Crucially, `Min`
and `Max` bound only the *compatible* releases — the lowest and highest that came
back compatible, not a promise that every release between them did. An interior
release can still be incompatible (a symbol dropped then re-added, or a hypothetical
drop), so `Min`–`Max` is a summary and `Pairs` is the authoritative per-release
picture: read it for gaps. When no release
is compatible, the result is `no_candidate` instead; when nothing is compatible
but something is unsettled, `unknown`; and when the axis is empty, `no_releases`.

The value of a range over a single floor is that it carries the per-release
confidence labels, and those labels tell a story the floor cannot. Reading a
`Pairs` list like

```
1.0: compatible(curated)
1.1: compatible(extrapolated)
```

you can see where the evidence stops being metadata and starts being assumption —
where the resolver is standing on a `.symbols` lower bound and where it is leaning
on monotone exports. For `/bin/ls` against `libc.so.6` the real axis runs
`[2.35-0ubuntu3, 2.35-0ubuntu3.15]`, and the range comes back
`[2.35-0ubuntu3, 2.35-0ubuntu3.15]` — the full span, both ends
`compatible(curated)`, because the evidence here is `.symbols` metadata and an
`exact` verdict would need readelf run at that specific release.

The hypothetical drop of §5.1 is the bridge to chapter 8, and the range is where
it bites. Modelling the removal of `getenv@GLIBC_2.2.5` at `2.35-0ubuntu3.15`
caps the range at `[2.35-0ubuntu3, 2.35-0ubuntu3]` — the upper releases drop out
once a symbol they need disappears. Modelling the removal of a load-bearing
symbol like `__libc_start_main@GLIBC_2.34` at the oldest release instead collapses
the result to `no_candidate`: take away something every release needs from the
bottom up, and nothing is left to stand on. The same mechanism, aimed below the
floor, shows the other face of the labels. Pointed at a hypothetical
`2.31-0ubuntu9.9`, older than the floor, the verdict is

```
incompatible([below_floor(__libc_start_main@GLIBC_2.34, 2.34),
              below_floor(lstat@GLIBC_2.33, 2.33)])
```

— not a bare "no", but the specific symbols that would be absent and the release
each first appears at, the same evidence the floor was computed from turned to a
different question. And because a curated floor never vetoes a release that some
other evidence row satisfies, adding readelf evidence for that release would turn
the verdict into `compatible(exact)` — the defeasible conclusion withdrawn the
moment a harder observation contradicts it.

## 5.6 Hand-off to the CLI

Everything in chapters 2 through 5 is now a predicate that returns one of these
terms. The two axes are data; the store records evidence in tiers that know when
absence is a fact (chapter 4); `req_status/5` resolves each requirement;
`aggregate_statuses/2` and `abi_verdict/5` assemble and gate a verdict;
`abi_floor/3` reproduces `dpkg-shlibdeps`; and `abi_range` reads the compatible
span off the real axis. Each answer carries its basis and its reasons as a term,
which is the whole reason the next chapter is short. Chapter 6 is the thin
command-line driver over exactly these queries, and the `explain` command that
renders the terms defined here as plain English.

## Next

Chapter 6: The CLI and `explain` *(written)* — drive the resolver and read a
verdict out loud.
