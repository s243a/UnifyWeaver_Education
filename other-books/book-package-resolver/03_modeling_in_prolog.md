<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 3: Modeling in Prolog

Chapter 2 ended with a demand: to answer any of its questions as an actual query,
we need a way to *write down* the evidence and the two axes as data the resolver
can reason over. This chapter is that writing-down. It turns the problem into a
small set of Prolog relations over facts, and it makes one design decision that
shapes everything after it — the ABI lane is built as a driver *above* the
existing package resolver, not inside it. This is the "why declarative is
powerful" chapter chapter 1 promised, and the payoff is concrete: once the two
axes and the evidence are data, the hard questions become short relations — and, by
design, the model is written to compile to targets beyond SWI-Prolog (what is
demonstrated today versus what is still a goal is the subject of Appendix C). The spine stays the same `/bin/ls` from coreutils on an Ubuntu 22.04 box.

## 3.1 Catalog-as-data, not database

The coarse package resolver this lane sits above holds to a rule it states up
front: *the catalog is data, not the Prolog database.* Every one of its queries
takes a `catalog/6` term as its first argument, and there is no `assert` or
`retract` anywhere in the spec — a query is a question asked against a value you
pass in, not against whatever happens to be loaded in the interpreter. Swap the
catalog term, ask the same query, get the answer for the new world.

The ABI lane keeps that governing idea and realizes it a little differently. Its
facts are larger and come from disk, so rather than thread one giant term through
every call it loads them into a handful of dynamic predicates:

```
symprov/4  symreq/5  needed/2  replaces/2
prov_evidence/4  req_evidence/4  release/3
```

What matters is that these dynamic predicates are still treated as a *store*, not
as a growing global database. The store is a directory of JSONL files, and
`load_abi_store/1` loads it atomically: it clears whatever was there with
`abi_store_clear`, loads every row, and — this is the point — if any single row
fails to parse or validate, it clears the partial store again and re-throws. A
contradictory row (we will meet one in §3.2) brings down the whole load rather
than leaving the resolver computing on half a world. The comment in the code puts
it plainly: callers never compute on half a load. Loading is all-or-nothing, so a
query always runs against a complete, self-consistent store or against none.

This is what "data, not code" buys. Because the library knowledge is a store and
not hard-coded clauses, you can point the resolver at a different store and ask
the identical questions — the basis of the testable fixtures in chapter 4. And
because the *reasoning* is clauses over data rather than data baked into the
reasoning, the same relations can be compiled to a different target and checked
against the SWI oracle, which is chapter 7. The model is portable precisely to
the extent that it refuses to put facts in the program.

## 3.2 Representing the two axes

Chapter 2 insisted the two version axes must never be mixed. The model enforces
that structurally, by giving each axis its own representation rather than trusting
a programmer to remember which string is which.

The **version node** is the opaque one. It is carried as a plain atom —
`GLIBC_2.34`, `LIBSELINUX_1.0`, `COMMON_1`, `PUBLIC` — and the only operation the
model ever performs on it is unification. There is no comparison, because there
is no ordering: a requirement `foo@LIB_1` is satisfied only by a provider row
spelling exactly `foo@LIB_1` on the same soname. The unversioned case is the atom
`Base`, dpkg's spelling for "no node at all," and the loader of `assert_symprov`
hard-codes that a `Base` export always binds an unversioned reference regardless
of its recorded binding (`binding('Base', _, default)`).

The **package version** is the ordered one, and it is represented so that it
*can* be ordered. A release identifier is turned into a term by `rel_term/2`: a
genuine Debian version becomes a `deb(Epoch, Upstream, Revision)` term produced by
the frozen Debian parser (`parse_deb_version/2`), while anything that is not a
Debian version — a label with no numeric upstream — is wrapped as `label(Atom)`
and only ever matches itself. The guard is deliberately strict: a parse counts as
a real version only when the upstream part starts with a digit (Debian Policy
§5.6.12); otherwise it is a label. The two shapes cannot be confused because they
are different functors.

Ordering is then delegated, never reinvented. `rel_lt/2` and `rel_le/2` are thin
wrappers over the frozen `resolver:version_lt/2` operating on those `deb/3` terms.
The ABI lane does not carry a second, subtly-different copy of Debian version
comparison; it reuses the one the package resolver already contract-tests. This is
why the micro-example from chapter 1 works out. Ask why `3.1~` sorts *below* `3.1`
and the answer lives entirely in `version_lt/2`: the `~` sorts before everything,
including the empty string, so `3.1~` precedes `3.1`. That ordering is what lets
the floor for `/bin/ls` against `libselinux.so.1` come back as `3.1~` — a floor
one tick below `3.1`, exactly the `Pre-Depends` coreutils declares. Get the
ordering wrong and the floor is wrong; the lane avoids the risk by not owning the
ordering.

The store validation in §3.1 rests on this axis too. When `assert_symprov` reads a
curated `.symbols` row, it checks `rel_le(Deb, R0)` — a curated minimum version can
never exceed the release the row was curated from. A row claiming otherwise is
*contradictory*, and it is exactly the kind of bad row that makes the whole store
fail to load.

## 3.3 Identity: the (soname, symbol, node) triple

With the axes settled, the unit of knowledge falls out. The thing a library
offers, and the thing a binary wants, is the exact triple **(soname, symbol,
version-node)**.

A provider fact is `symprov(So, Sym, Node, Bound)`: soname `So` exports `Sym` at
`Node`, and `Bound` is the evidence it rests on — either `since(Deb, Atom, R0,
Bind)` from a `.symbols` lower bound, or `at(R0, Bind)` from a direct readelf
observation at release `R0`. (Those bounds, and the `Bind` default-version tag,
are the subject of chapter 4; here they are just the shape of the fact.) A
requirement fact is `symreq(Bin, Sym, Node, So, Bind)`: binary `Bin` needs
`Sym@Node` from soname `So`, with `Bind` recording whether the reference is
`GLOBAL` or `WEAK`.

Matching is exact on the triple, and you can watch a requirement reach for its
provider. `req_status/5` enumerates one status per requirement; for a versioned
one it calls `ident_status/5` with the requirement's own `(So, Sym, Node)`, which
succeeds only against provider rows carrying that same triple. The interesting
cases are the failures, because they are not all the same. If the soname has
provider evidence but nothing for *this* identity, the status is `unknown` — the
evidence simply does not speak to the symbol. If the soname has no usable provider
evidence at all, the status is `unknown(Sym@Node, no_provider_evidence(So))`. A
hard "missing" verdict is reserved for the case where complete evidence positively
*rules out* the symbol, a distinction chapter 4 makes precise and chapter 5 turns
into verdicts.

The unversioned requirement is threaded through as its own case. It is stored with
`Node = none` and `So = none`, because the loader resolves an unversioned
reference against *any* `NEEDED` object, not a particular one. `unversioned_status`
honours exactly the loader's rule from chapter 2: it binds only to the symbol's
*default* version — a `Base` export, a `@@` default export, or the legacy
hidden-at-verdef-index-2 node the loader also treats as default — never an ordinary
non-default hidden (`@`) node, and it searches the
queried soname first, then the binary's other `NEEDED` objects at their own
evidence release. A symbol present only at a non-default node does not satisfy an
unversioned reference, and the model records precisely that rather than pretending
the reference resolved.

Put the pieces together on a tiny store. The two rows that carry the identity are
the provider and the requirement:

```jsonl
# symprov.jsonl — libb.so.1 exports bar@LIBB_1, curated from release 2.0
["libb.so.1|bar@LIBB_1", ["since", "2.0", "2.0", "default"]]
# symreq.jsonl  — mybin needs bar@LIBB_1 from libb.so.1, a GLOBAL reference
["mybin|bar@LIBB_1", ["libb.so.1", "GLOBAL"]]
```

Those two rows alone are deliberately *not* enough. Query them and every release
comes back `unknown(bar@'LIBB_1', no_provider_evidence('libb.so.1'))`: the resolver
has a provider row but nothing telling it what *kind* of evidence stands behind it,
and it will not reason from an unqualified fact — the refusal that is the whole
subject of chapter 4. Add the one evidence row that marks `libb.so.1`'s exports as
curated `.symbols` data —

```jsonl
# evidence.jsonl
["provides|libb.so.1", ["symbols", "2.0", "curated", "test-fixture"]]
```

— (with `mybin`'s requirement-evidence row and a `needed`/`releases` line completing
the store, as chapter 4 lays out) and the query resolves. Now `req_status/5` reaches
for the provider carrying the matching `(libb.so.1, bar, LIBB_1)` triple, finds it,
and the curated lower bound decides the answer:

```
status mybin libb.so.1 2.0  ->  provided(bar@'LIBB_1',curated)
status mybin libb.so.1 1.0  ->  below_floor(bar@'LIBB_1','2.0')
```

At `1.0`, below the curated minimum, the *same* row yields `below_floor` — the
provider is there, but its guaranteed-from version is above what you asked for. One
provider row, one requirement row, one evidence row, two releases, two answers — and
every later mechanism (the floor and range of chapter 5, the English of chapter 6)
is this lookup, aggregated over all of a binary's requirements and dressed up.

## 3.4 A driver above a frozen core

The single most important structural decision is the one the lane's README leads
with: it is a driver *above* the frozen resolver. `resolver.pl` and
`resolver_store.pl` are not edited. The ABI lane reuses only two things from
below — `version_lt/2` for the package-version axis, and the Debian parser that
feeds it — and builds everything else on top as a new module.

Freezing the core is not timidity; it is what protects the guarantees the core
already carries. The coarse resolver is contract-tested against an append-only
corpus, and the *same* relations are compiled through other backends — a
JavaScript WAM, with a choice of indexed or LMDB fact stores — and held to the
SWI oracle by a zero-divergence differential. Editing it to add ABI reasoning
would put all of that at risk to graft on a concern the core was never about. So
the ABI lane sits on top, consumes the frozen version comparison as a settled
black box, and inherits the core's "one model, many targets" discipline for free.
This is chapter 1's slogan made structural: the new lane is new relations over new
data, not a fork of the thing underneath it.

Layering also shows up in the evidence itself, as three provider tiers arranged
cheapest first. The coarsest is the `Packages` `Depends:` line — the single floor
`libc6 (>= 2.34)` the stock package manager already consumes, which this lane's
`abi_floor/3` recomputes from the symbols and reproduces exactly. Next is the
`.symbols` control member: every exported `sym@node` with a curated minimum
version, already sitting on disk under `/var/lib/dpkg/info/*.symbols` at a few
kilobytes per package, no binary download. Richest is `readelf` over an actual
ELF, which observes the export set directly. The tiers climb from cheap-and-coarse
to expensive-and-complete, and the model is built to use whichever it has — the
grammar of which tier proves what is chapter 4.

## 3.5 Mode-safety and determinism as teaching moments

Two small design choices in the code are worth pausing on, because each is a
concrete lesson in a real Prolog hazard rather than an incidental style.

The first is why `ident_status/5` computes its answer into a fresh variable and
only then unifies it with the caller's pattern:

```prolog
ident_status(So, Sym, Node, Rel, Status) :-
    ident_status_(So, Sym, Node, Rel, S0), !,
    Status = S0.
```

The worker `ident_status_/5` is a chain of clauses, and its helpers `combine/4`
and `says_status/3` carry their cuts *after* head unification. If a caller passed
a partially-bound `Status` — say `provided(_, default)` — straight into the
worker, head unification could skip the clause the unbound call would have fired
and silently match a later one. That is not hypothetical: it once produced a false
`compatible` for a symbol that was actually observed dropped. Computing into a
fresh `S0` and unifying last makes the predicate *mode-insensitive*: the status
is computed as if the output were unbound, and every caller — whatever it passes —
sees the single, correct, mode-independent answer.

The second choice is the determinism that buys. `ident_status/5` commits with a
cut to one status per identity, and the verdict predicates aggregate those into
one verdict per `(binary, soname, release)` question. A single, mode-independent
answer is not just tidy; it is a precondition for chapter 7's differential. When
the same specification is compiled to another backend and both are asked the same
question, "the answer" has to be one answer, independent of how the query was
posed, or "zero divergences" would mean nothing. The mode-safety in a five-line
predicate is where that property is actually earned.

With the axes, the identity triple, and the driver-above-frozen-core design in
hand, the model has a shape but not yet its evidence grammar. The next chapter
fills that in: what the on-disk store records, and the distinction that governs
everything downstream — when absence of a symbol is a *fact*, and when it proves
nothing at all.

## Next

Chapter 4: Store & evidence tiers — the on-disk facts, and why absence is only
sometimes a fact.
