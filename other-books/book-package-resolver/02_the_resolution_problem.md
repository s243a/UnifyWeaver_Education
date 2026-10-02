<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 2: The resolution problem

Chapter 1 ended on a promise and a crash. The promise was that the same soname
can carry a library across decades of releases; the crash was what happens when a
binary reaches for a symbol the library on *this* machine is too old to export.
This chapter makes that motivation rigorous. By the end of it you should be able
to say, precisely, what a verdict has to decide — and why the tools already in
the box stop short of deciding it. The spine is the same mundane example as
before: `/bin/ls` from coreutils on an Ubuntu 22.04 box.

## 2.1 From package dependencies to symbols

Start with the layer most people already know. When you look at a package's
metadata you see dependency lines like `libc6 (>= 2.34)`. That `>= 2.34` is a
*floor* on a whole package: the earliest release of `libc6` that will do. It is
the coarsest of three provider tiers the resolver works with, and the only one
the stock package manager consumes directly — the `Packages` `Depends:` tier.

A coarse floor is cheap and, as far as it goes, correct. But look at what it is:
one number, per library, computed once when the `.deb` was built. That shape
limits what it can tell you. It cannot answer "what is the *range* of releases
this binary is compatible with" — it only names a lower bound on that range, and
only the bound it happened to know about at build time. It cannot reason about a
frozen base layer you want to keep and a binary you want to drop on top of it,
because for that you need both ends, not just the floor. And it collapses a lot
of detail into a single integer-looking token, hiding the fact that the real
compatibility relation does not live at the level of packages at all.

It lives per *symbol*. `libc6 (>= 2.34)` is a summary; the thing being summarised
is a list of individual functions the binary calls and the earliest release of
the library that offers each of them. To answer the questions the coarse floor
cannot, we have to drill down to that list. The rest of this chapter is that
drill-down.

## 2.2 Sonames and the loader's promise

A *soname* is the name a shared library advertises itself under — `libc.so.6`,
`libselinux.so.1`. The trailing number is not a version in the ordinary sense; it
is a compatibility generation. The whole discipline of shared libraries rests on
holding the soname *fixed* across releases for as long as the library keeps its
promise: a binary linked against an older `libc.so.6` must keep running against a
newer one. When that promise cannot be kept — when the library changes in a way
that breaks old callers — the maintainers *bump the soname*, from `.6` to `.7`.
A soname bump is precisely the signal "the promise was broken; these are not
interchangeable."

The loader enforces the matching by the soname string, and only by the string.
Every dynamic binary records a `DT_NEEDED` list: the exact sonames it asks for.
For `/bin/ls` that list is `[libselinux.so.1, libc.so.6]`. At startup the loader
looks for libraries offering *those* strings — not strings that look similar,
not strings with a nearby number. Two facts the resolver reports fall straight
out of this. If a library's soname is not in the binary's `DT_NEEDED` list and is
not a declared stand-in for one that is, the verdict is `not_needed(So)`: the
question simply does not apply. And if the binary needs `libc.so.6` but you offer
`libc.so.7`, that is a `soname_mismatch` — the generations do not match, and the
loader would not pair them. (Appendix A traces how the loader turns a soname into
an actual file on disk — `ldconfig`, `/etc/ld.so.cache`, and the override knobs —
and why this book reasons statically instead of running the binary to find out.)

This is also why there is no name-stem heuristic anywhere in the resolver. It is
tempting to assume `libselinux.so.10` is the natural successor of
`libselinux.so.1` because they share a stem, but the shared stem means nothing to
the loader and nothing to the model. Offering `libselinux.so.10` against a binary
that needs `libselinux.so.1` yields `not_needed`, not a match. The only thing
that makes one soname count as a successor of another is a *declared* succession
relation — `replaces(libc.so.7, libc.so.6)` — supplied as evidence. With that
declaration, offering `libc.so.7` becomes a `soname_mismatch` against the binary;
without it, a same-stem library is just an unrelated library.

## 2.3 Versioned symbols and the exact-match rule

Keeping the soname fixed only works because a second mechanism tracks change at a
finer grain: *symbol versioning*. A library does not merely export `fopen`; it
exports `fopen@GLIBC_2.2.5`. When glibc needs to change `fopen` without breaking
old callers, it adds `fopen@GLIBC_2.17` alongside the old entry, and each binary
records exactly which versioned symbol it was linked against. The suffix after
the `@` is the *version node* — `GLIBC_2.2.5`, `LIBSELINUX_1.0` — and a symbol
can be exported at several nodes at once.

A node is exported in one of a few ways. A **default** export is written with a
double at-sign, `fopen@@GLIBC_2.17`: this is the version a fresh link picks up and
the one an unversioned reference binds to. A **hidden** export uses a single
at-sign, `fopen@GLIBC_2.2.5`: still present for binaries that explicitly asked for
it, but not offered to new or unversioned callers. And some symbols carry no node
at all — the unversioned case, which dpkg's `.symbols` files spell `Base`.

The rule that ties a requirement to a provider is blunt and it is the heart of
the model: **exact match, by string, never by number.** A requirement `foo@LIB_1`
is satisfied only by a provider `foo@LIB_1` on the *same* soname. `foo@LIB_2` is
not a match, even though `2` is "newer" than `1` — the version node has digits in
it but no ordering, and the resolver never compares it numerically. The loader
agrees, and says so plainly. The ELF fixtures build exactly this case: `foo@LIB_1`
required against `foo@LIB_2` exported under one soname, and running it fails with

```
undefined symbol: foo, version LIB_1
```

The resolver returns `incompatible([missing(foo@LIB_1)])` for the same inputs —
the same verdict the loader reaches at runtime, reached statically.

An *unversioned* requirement follows a companion rule, again matching what the
loader accepts: it binds to whatever the loader treats as the symbol's *default*
version — a `Base` export, a `@@` default export, or (a documented legacy case) a
hidden node at the first real version-definition index, which the loader still
counts as default. What it does not bind to is an ordinary *non-default* hidden
(`@`) node. The hidden-version fixture makes the distinction
concrete. A symbol `hid_fn` exported only at a *non-default* hidden `@HID_1` does **not**
satisfy an unversioned reference; the resolver returns
`incompatible([missing(hid_fn, no_default_export(...))])`, and the loader again
agrees — `undefined symbol: hid_fn`. The same symbol exported where the loader
*would* treat it as the default binds cleanly, and resolver and loader agree on
that too. The point of pinning both halves against a real loader is that the
exact-match rule is not an opinion the model holds; it is the behaviour the model
is obligated to reproduce.

## 2.4 The two axes, stated precisely

Everything above turns on keeping two kinds of version strictly apart. They look
alike — both are strings with dots and digits — and confusing them is *the*
classic bug in this space.

The first is the **version node**: `GLIBC_2.34`, `LIBSELINUX_1.0`. Despite the
digits, it is an opaque label with no ordering at all. It answers one question —
"does this release export `Sym@Node`?" — and it answers it by string equality.
There is no "greater than" between nodes; `GLIBC_2.34` either is or is not the
label on the export, and that is the whole of it.

The second is the **package version**: `2.34`, `3.1~`, `1:2.3-1`,
`2.35-0ubuntu3.15`. This one *is* ordered, by the full Debian comparison rules —
epochs, the `~` that sorts before everything (so `3.1~` precedes `3.1`),
revisions, components of any length. This is the axis along which "is release R
new enough?" is a meaningful question. The resolver does not reimplement these
rules; it delegates them to the frozen package resolver's `version_lt/2`,
operating on `deb(Epoch, Upstream, Revision)` terms parsed from the version
string. Ordering package versions is a solved problem, and the ABI lane reuses
the solution rather than risking a second, subtly-different copy.

The discipline is to never let one axis leak into the other: never compare a node
numerically, never match a package version by string equality. The resolver
enforces this structurally, by giving the two axes separate columns in its model
rather than relying on a programmer to remember which is which — the subject of
chapter 3.

## 2.5 What `ldd` and `dpkg-shlibdeps` actually answer

With the problem stated this precisely, it is worth being exact about why the two
obvious tools do not solve it. Neither is wrong; each answers a different
question.

`ldd` resolves sonames against *this machine's* libraries. It tells you which
files the loader would open here, and it is accurate about that. What it cannot do
is say anything about any other release: it reports the libraries present now, not
the versions a binary would tolerate elsewhere. That is the chapter-1 crash seen
from the other side — `ldd` was asked the right question ("will this run?") and
gave an answer to a narrower one ("which sonames resolve on *this* box?") —
accurate about the machine you are on, and silent about every release you are not.

`dpkg-shlibdeps` goes further and is genuinely useful. At build time it inspects
the binary, looks its symbols up in the `.symbols` files, and emits a single
floor — the `libc6 (>= 2.34)` from §2.1. That number is correct and the resolver
is built to reproduce it. But it is one number, computed once, at build time. It
names a curated lower bound and nothing else: no upper end, no
notion of a release it has no data for, no way to say "I cannot prove this is
safe" instead of guessing.

Laid out as a table, the gap is clear:

| Question | `ldd` | `dpkg-shlibdeps` | This book |
|----------|-------|------------------|-----------|
| Which sonames does this binary need? | yes | — | ch 2 |
| What is the floor — the earliest compatible release? | no | yes (one number) | ch 5 |
| What is the full compatible *range*? | no | no | ch 5 |
| Can "I don't know" be a first-class answer? | no | no | ch 4–5 |
| What is the verdict against a release nobody shipped a `.symbols` file for? | no | no | ch 5 |

The existing tools answer the top two rows. Everything below them is why this
book exists.

## 2.6 The questions this resolver answers

So what does a verdict have to decide? The full grammar waits for chapter 5; here
is the shape of the space. A `(binary, soname, release)` question resolves to one
of a small set of outcomes: **compatible**, **incompatible**, **unknown**, or
**not_needed** — and alongside the per-release verdict, two aggregate answers the
coarse floor could not give: the **floor** and the compatible **range**. The
`unknown` is the one that distinguishes this tool from the others: it is an honest
answer the resolver reaches when its evidence genuinely cannot settle the
question, rather than a guess dressed up as a yes.

Bring it back to `/bin/ls`. Its requirements decompose into **112 versioned plus
3 unversioned (weak)** symbol references against `NEEDED = [libselinux.so.1,
libc.so.6]`. Ask for the floor and the resolver computes `libc.so.6 (>= 2.34)`
and `libselinux.so.1 (>= 3.1~)` — exactly the `Pre-Depends` coreutils actually
declares, reproducing `dpkg-shlibdeps` from the same evidence. Then it answers
what the metadata cannot. Across the real release axis
`[2.35-0ubuntu3, 2.35-0ubuntu3.15]`, the range comes back
`[2.35-0ubuntu3, 2.35-0ubuntu3.15]`, both ends `compatible` (resting on `.symbols`
metadata, a distinction chapter 4 makes precise). And pointed at a release below
the floor — a hypothetical `2.31-0ubuntu9.9` — it returns

```
incompatible([below_floor(__libc_start_main@GLIBC_2.34, 2.34),
              below_floor(lstat@GLIBC_2.33, 2.33)])
```

— not a bare "no", but the specific symbols that fall below their curated minimum
and the release each is guaranteed from. That is the same evidence the floor was
computed from, turned to a different question and carrying its reasons with it.

To answer any of these as actual queries, though, we need a way to *write down*
the evidence and the two axes as data the resolver can reason over. That is the
model, and it is the next chapter.

## Next

Chapter 3: Modeling in Prolog — the two axes as data, and a driver built above
the frozen resolver.
