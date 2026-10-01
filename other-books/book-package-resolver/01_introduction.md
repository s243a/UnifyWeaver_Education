<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 1: Introduction

**The cast.** Three names recur throughout this book. **glibc**, the GNU C
library, ships as `libc.so.6` and is the library almost every Linux program links
against for the basics — opening a file, starting up, calling `printf`.
**libselinux** (`libselinux.so.1`) is a smaller library for security labelling
that ordinary utilities like `ls` pull in. And **`ldd`** is the command-line tool
that lists which shared libraries a given program depends on — it asks the
**dynamic loader** (the part of the system that finds and loads those libraries
when a program starts) what it would pull in. If those three are already
familiar, skip ahead.

## A question `ldd` cannot answer

Run `ldd` on a program and it tells you which shared libraries the dynamic
loader will pull in: `libc.so.6`, `libselinux.so.1`, and so on. Run it on a
machine where one of those libraries is *too old*, and it still tells you the
same thing — right up until the program starts, reaches for a function the old
library does not export, and dies with

```
undefined symbol: __libc_start_main, version GLIBC_2.34
```

`ldd` answers "which libraries", by *soname* — the versioned name a library
advertises itself under, like `libc.so.6` (Chapter 2 makes this precise). It does
not answer the question a distribution maintainer actually has to answer, which is
"which *versions* of
those libraries will this binary run against". That second question is the
subject of this book. It is harder than it looks, it has a genuinely elegant
declarative structure once you find it, and — this is the part that makes it
worth a whole book — it is a good test of whether "write the model once, compile
it to many targets" is a real idea or a slogan.

## Why versions are the hard part

A shared library is not one thing over time. `libc.so.6` has carried that same
soname across decades of releases, and the whole point of keeping the soname
fixed is a promise: a binary linked against an older `libc.so.6` keeps working
against a newer one. The promise is kept by *symbol versioning*. The library
does not just export `fopen`; it exports `fopen@GLIBC_2.2.5`, and when glibc
needs to change `fopen`'s behaviour without breaking old callers, it adds
`fopen@GLIBC_2.17` alongside the old one. A binary records exactly which
versioned symbol it wants. The loader matches by the exact `symbol@version`
string — never "close enough", never a numeric comparison of the version node.
`foo@LIB_1` is simply not satisfied by `foo@LIB_2`, and the loader will say so.

So the real compatibility relation lives at the level of individual
`(soname, symbol, version-node)` triples, and it has two axes that look similar
and must never be mixed:

- The **version node** — `GLIBC_2.34`, `LIBSELINUX_1.0` — is an *opaque label*.
  Despite the digits in it, there is no ordering. `GLIBC_2.34` is matched to a
  requirement by string equality and nothing else.
- The **package version** — `2.35-0ubuntu3.15`, `3.1~`, `1:2.3-1` — *is*
  ordered, by the full Debian comparison rules (epochs, the `~` that sorts
  before everything, revisions). This is the axis along which we ask "is release
  R new enough?"

Confusing the two is the classic bug in this space, and keeping them apart is
the first design decision the resolver in this book makes.

## How Debian already half-solves this

Debian has carried a partial answer to the version question for a long time, and
the resolver is built directly on top of it, so it is worth knowing the shape of
what already exists.

When a `.deb` is built, `dpkg-shlibdeps` inspects the binary, sees which
versioned symbols it needs, looks them up in the `.symbols` files that ship
with the libraries, and emits a dependency like `libc6 (>= 2.34)`. That `>= 2.34`
is a *floor*: the earliest release of `libc6` that provides every symbol the
binary needs. The `.symbols` files it reads are curated lists, a few kilobytes
each, already sitting on every Debian system under `/var/lib/dpkg/info/*.symbols`.
Each line records a versioned symbol and the minimum package version it is
guaranteed to appear from.

This is genuinely useful and it is also genuinely limited. A `.symbols` file is
a curated *lower bound*, not a complete census of what a library exports. It
tells you a symbol is present *from* some version; it does not, on its own, tell
you a symbol is *absent* — maybe it was simply never added to the curated list.
And `dpkg-shlibdeps` emits a single floor; it does not tell you the *range* of
releases a binary is compatible with, which is exactly what you need when you are
reasoning about an immutable base layer that you would like to keep and a binary
you would like to drop on top of it.

The example that recurs throughout this book is deliberately mundane: the
`/bin/ls` from coreutils on an Ubuntu 22.04 box. It needs `libc.so.6` and
`libselinux.so.1`. Ask the resolver for the floor and it computes
`libc.so.6 (>= 2.34)` and `libselinux.so.1 (>= 3.1~)` — *exactly* the
`Pre-Depends` coreutils actually declares. The point of reproducing a number you
could already read off the package metadata is that the same model, from the
same evidence, then goes on to compute things the metadata does not carry: the
full compatible release range, and a defensible verdict for a release nobody has
a `.symbols` file for.

## Why model it in Prolog

Once the problem is stated as "given this evidence about symbols, versions, and
releases, what is the verdict for this binary against this library at this
release", it is a reasoning problem, and reasoning problems are what logic
programming is for.

Three properties of the Prolog formulation carry the whole design:

- **The evidence is data, not code.** The resolver does not hard-code any
  library. It loads a *store* of facts — which symbols a library provides from
  which version, which symbols a binary requires, where each fact came from —
  and every query is a question asked against that store. Swap the store, ask
  the same questions. Chapter 4 is about the store and the tiers of evidence it
  records.
- **A verdict is a derivation.** "Incompatible" is never a bare boolean; it is a
  term that carries *why* — `below_floor(bar@'LIBB_1', '2.0')` says a symbol
  first appears in release 2.0 and you asked about something earlier. Because the
  reasoning is declarative, the explanation falls out of the same clauses that
  reach the conclusion. Chapter 6, already written, shows the `explain` command
  turning those terms into English.
- **Defeasible reasoning is natural.** The deepest idea in the model is an
  *assumption that can be overridden*: within a single soname, exports do not
  disappear (removing one would be an ABI break that forces a soname bump). So
  presence at one release can be *extrapolated* forward to later releases — a
  conclusion good enough to act on, but labelled as such and withdrawn the moment
  harder evidence contradicts it. Prolog's clause ordering and cut make this kind
  of "believe it unless something stronger says otherwise" reasoning direct to
  write. Chapters 5 and 8 return to it; chapter 9 lists where it still bites.

This is why the verdict vocabulary has more than two values. A release can be
`compatible`, `incompatible`, or honestly `unknown` — and the `compatible` and
`incompatible` verdicts carry a note about *how much to trust them*: whether the
conclusion rests on a direct observation, on curated metadata, or on the
monotonic-export assumption. Encoding "I don't know" as a first-class answer,
rather than guessing, is what keeps the resolver from the failure mode of stock
tooling, which will cheerfully "upgrade" a held package out from under you
because it had no way to say "I cannot prove this is safe".

## What UnifyWeaver adds

Everything so far could be a standalone Prolog program. What makes it a
UnifyWeaver example — and what earns it a place in this series — is that the
*same declarative model* is not meant to run only in SWI-Prolog.

UnifyWeaver is a declarative-to-imperative compiler: you write the relations
once, and the compiler emits them for a chosen target. The resolver is written
to be compiled, not just interpreted. SWI-Prolog serves as the *oracle* — the
reference implementation whose answers define "correct" — and the same
specification is compiled through other backends (a JavaScript WAM, among
others) and held to the oracle's answers by a differential test that must report
*zero* divergences. The underlying package resolver this ABI lane sits above even
offers a choice of fact-store backends behind one set of relations, so the same
queries can run against an in-memory term, an indexed file store, or an LMDB
database without the model changing a line.

That is the slogan made concrete: *one declarative model, many targets, one set
of answers.* A problem as detail-heavy and as unforgiving as ABI compatibility —
where a single confused axis gives a wrong verdict and a real program crashes —
is a demanding place to make that claim and defend it. Chapter 7 is where the
model meets the compiler; chapter 8 is where it meets scale.

## How to read this book

The chapters build in a line, from the problem to the model to the machinery:

- **Chapter 2 — The resolution problem.** The ground truth: sonames, versioned
  symbols, the loader's matching rule, and precisely what `ldd` and
  `dpkg-shlibdeps` do and do not answer. The motivation of this chapter, made
  rigorous.
- **Chapter 3 — Modeling in Prolog.** The two axes as data, the catalog-as-facts
  design, and how this ABI lane is built as a driver *above* the frozen package
  resolver rather than inside it.
- **Chapter 4 — Store & evidence tiers.** The on-disk store, and the distinction
  that governs everything downstream: *complete* evidence (where absence is a
  fact) versus *curated* evidence (where absence proves nothing).
- **Chapter 5 — Verdicts, floor, range.** How a verdict is derived and labelled,
  how the floor is computed to match `dpkg-shlibdeps`, and how the compatible
  range is read off the real release axis.
- **Chapter 6 — The CLI and `explain`** *(written)*. Driving the resolver from
  the command line against a small store, and reading a verdict as English.
- **Chapter 7 — Compiling to targets.** The oracle, the backends, and the
  zero-divergence differential that keeps them honest.
- **Chapter 8 — Pruning & scale.** What makes these queries expensive, and the
  defeasible assumption revisited as a performance lever.
- **Chapter 9 — Open problems.** The known hazards and the edges where the model
  is still rough.

A reader who wants the ideas can read chapters 1, 2, 5, and 9. A reader who wants
to drive the tool can read chapters 1, 4, and 6 and keep the store schema from
chapter 4 open. The linear path is written for a reader who is comfortable with
Prolog facts and queries and has a rough mental picture of shared libraries, but
has never had to think about symbol versioning before.

Where the book states how the resolver behaves, it is describing the real code in
`examples/pkg_resolver/abi/` of the UnifyWeaver repository — chiefly the resolver
core `examples/pkg_resolver/abi/abi_resolve.pl` and its reference,
`examples/pkg_resolver/abi/README.md`. The grammar of every verdict term shown in
this book is defined there.

## Next

Chapter 2: The resolution problem — the loader's rule, and the exact gap between
what the existing tools answer and what we need *(planned)*.
