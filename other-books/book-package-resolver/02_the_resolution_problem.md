<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 2: The resolution problem

> **Status: skeleton / planned.** This is a section-and-bullet outline for a
> later writing pass, not finished prose. It fixes what the chapter will teach
> and which source facts it will draw on. The pedagogical voice is set by
> chapter 1.

Goal of the chapter: turn chapter 1's motivation into a precise statement of the
problem, so the reader knows exactly what a "verdict" has to decide and why the
existing tools stop short. Narrative throughout; worked `/bin/ls` example as the
spine.

## 2.1 From package dependencies to symbols

- Recap the coarse layer: a `Packages` `Depends:` line like `libc6 (>= 2.34)` is
  a floor on a whole package. This is tier 1 of three (see
  `examples/pkg_resolver/abi/README.md`, "The three provider tiers").
- Why the coarse floor is not enough on its own: it is one number per library,
  computed once at build time; it cannot answer "what is the compatible *range*"
  or reason about a frozen base layer.
- Set up the drill-down: the real relation is per *symbol*, not per package.

## 2.2 Sonames and the loader's promise

- What a soname is and why it is held fixed across releases (the ABI-stability
  promise); a soname bump is the signal that the promise was broken.
- `DT_NEEDED`: the exact list of sonames a binary asks for; the loader matches by
  exact soname string (grounds `not_needed` and `soname_mismatch` later).
- Teaching point: this is why there is no name-stem heuristic in the resolver —
  `libselinux.so.10` is not treated as a successor of `libselinux.so.1` without a
  *declared* `replaces` relation.

## 2.3 Versioned symbols and the exact-match rule

- Symbol versioning: `fopen@GLIBC_2.2.5`, default (`@@`) vs hidden (`@`) exports,
  and the `Base`/unversioned case (dpkg's spelling of "unversioned").
- The exact-match rule: a requirement `foo@LIB_1` is satisfied only by a provider
  `foo@LIB_1` on the same soname — string equality, never numeric. Ground it with
  the loader's own error on the ELF fixture (`undefined symbol: foo, version
  LIB_1`) from `examples/pkg_resolver/abi/README.md`.
- The unversioned-reference rule: binds only to a `Base` export or a *default*
  (`@@`) export, never a hidden one — again matching what the loader accepts.

## 2.4 The two axes, stated precisely

- Version node = opaque label, no ordering, matched by equality.
- Package version = ordered by the Debian rules (epoch, `~`, revision), delegated
  to the frozen resolver's `version_lt/2` over `deb/3` terms.
- Why mixing them is *the* classic bug; the resolver keeps them in separate
  columns of its model (forward reference to chapter 3).

## 2.5 What `ldd` and `dpkg-shlibdeps` actually answer

- `ldd`: resolves sonames for *this* machine's libraries; says nothing about
  other releases. The chapter-1 crash revisited as "right question, wrong tool".
- `dpkg-shlibdeps`: computes a single floor from `.symbols`; correct and useful,
  but one number, build-time, no range, no "unknown".
- The gap table: question / can the existing tool answer it / where this book
  answers it (floor → ch 5, range → ch 5, unknown-as-answer → ch 4-5,
  cross-release verdict → ch 5).

## 2.6 The questions this resolver answers

- Preview the verdict space without defining the grammar yet: compatible /
  incompatible / unknown / not_needed, plus floor and range.
- The `/bin/ls` numbers as a teaser: floor `libc.so.6` = 2.34, `libselinux.so.1`
  = 3.1~ (matches coreutils' declared `Pre-Depends`); a compatible range across
  the real release axis; a hypothetical older release going `incompatible` with a
  `below_floor` reason. All from `examples/pkg_resolver/abi/README.md`
  ("Verify on this machine").
- Hand off to chapter 3: to answer these as queries, we first need the model.

## Next

Chapter 3: Modeling in Prolog — the two axes as data, and a driver built above
the frozen resolver *(planned)*.
