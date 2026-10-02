<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Appendix C: The resolver as a test of UnifyWeaver

Chapter 1 noted, in one line, that compiling the resolver to other targets is a
*secondary* benefit for the resolver but a *primary* one for UnifyWeaver. This
appendix takes up that relationship properly: the two directions it pulls in, why a
demanding program is the right compiler test, the mechanism that makes the test
rigorous, and — kept deliberately honest — what is actually demonstrated today
versus what is still a goal.

## C.1 Two purposes, pulling opposite ways

The resolver has a job of its own: answer "which releases of this library will this
binary run against?" It does that in SWI-Prolog, and if targets never existed it
would still be useful. From the resolver's point of view, being compilable is a
convenience — eventually, a portable implementation.

UnifyWeaver has a different job: it is a declarative-to-imperative compiler, and a
compiler is only as trustworthy as the programs it has been made to compile
correctly. From UnifyWeaver's point of view, the resolver is not a tool at all — it
is a **test subject**, and a demanding one. Transpiling it is how the compiler earns
confidence.

So the same program wears two hats. Everything else in this book is about the first
hat. This appendix is about the second.

## C.2 Why a demanding program is the right test

A toy relation — `ancestor/2` over a handful of `parent/2` facts — exercises the
compiler's easy path and little else. The resolver exercises the parts where a
compiler is actually likely to be deficient:

- **Two kinds of comparison that must not be confused** — opaque-label equality for
  version nodes, full Debian ordering for package versions (chapters 2–3). A
  compiler that quietly conflates term equality with a numeric compare fails here
  and nowhere a toy would show it.
- **Defeasible reasoning** — the monotonic-export assumption, written with clause
  order and cut (chapters 3, 5, 8). Cut semantics are a classic place for a target
  to diverge from the oracle.
- **Mode-safety and determinism** — the `ident_status/5` fresh-variable pattern of
  §3.5 exists precisely because the naive version is a hazard; a target that handles
  modes differently is caught by it.
- **A real data layer** — a JSONL store loaded into dynamic predicates, aggregation
  over a whole requirement set, and a rich vocabulary of verdict terms (chapters
  4–5), not three facts and a query.

Each of these is a surface where a backend can be subtly wrong, and each is ordinary
in real code and absent from tutorials. This is the same philosophy the sibling
`plawk` example states in its own README — *"building it should expose the next set
of representation and runtime gaps that UnifyWeaver needs to support."* The ABI
resolver and `plawk` are a **family** of demanding programs chosen because they
press on different corners of the compiler.

## C.3 The oracle and the zero-divergence differential

What makes "did the compiler emit correct code?" a *checkable* question is the
oracle. SWI-Prolog running the relations directly is the reference whose answers
*define* correct. A target backend is trusted only when its answers match the
oracle's, across the test corpus, with **zero** divergences — not "close," not "a
few known differences," zero.

The strictness is not pedantry; it is forced by the subject. ABI compatibility is
unforgiving — one confused axis gives a wrong verdict and a real program crashes at
load time — so a differential that tolerated "nearly the same" would be blind to
exactly the class of bug that matters most. Zero-divergence turns each target into a
property that either holds or does not, per query, with a counterexample when it
fails. That counterexample is the deficiency, handed to you.

## C.4 Demonstrated today versus still a goal

It is worth being exact about status, because "compiled to many targets" is easy to
over-read.

- **Demonstrated:** the coarse package resolver this ABI lane sits *above* already
  runs this way — one set of relations over a choice of fact-store backends
  (in-memory term, indexed file store, or LMDB) without the model changing a line,
  held to the oracle.
- **A goal / in progress:** carrying the **ABI lane itself** end-to-end through the
  code-generating targets. The lane is *written* to be compiled — it reuses the
  frozen core as a black box and adds only new relations over new data (chapter 3),
  so it inherits the discipline by construction — but full transpilation of the lane
  is the standing next test, not a finished result. Chapter 7 is where the model
  meets the compiler; chapter 8 is where it meets scale.

Stating this plainly matters: this book teaches the *model* and the reasons it is
shaped to be compilable. It does not claim a finished multi-target deployment of the
ABI lane, and a reader should not infer one.

## C.5 The payoff loop

The two hats feed each other. Every deficiency the resolver (or `plawk`) surfaces
becomes a fix in the compiler; every fix makes the next demanding program
compilable; and each program compiled correctly is one more piece of evidence that
"write the model once, compile it to many targets" is a real capability rather than
a slogan. The resolver is at once a beneficiary of that loop — it stands to gain a
portable implementation — and an instrument of it, a probe shaped to find the places
the compiler is not done yet. That dual role is what earns a detail-heavy ABI
resolver a place in a book about UnifyWeaver, and not only in a book about linkers.
