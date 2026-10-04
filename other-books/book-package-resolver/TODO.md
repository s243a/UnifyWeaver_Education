<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)
-->

# book-package-resolver — remaining work

## Done
- Chapters 1–6 and Appendices A, B, C: written, reviewed (one deep review pass plus
  two follow-up review rounds), merged (PR #45).

## Remaining chapters — blocked on an authoritative cross-target status recon
Before drafting 7–9, fold in the recon's confirmed facts (and the three framing
corrections it flagged). Each chapter then follows the house process: draft →
verify every load-bearing claim against source (run the tools, don't trust shapes)
→ comprehension-panel review → external review.

- [ ] **Ch7 — Compiling to targets.** The SWI oracle and the cross-target
  zero-divergence differential for `examples/pkg_resolver/resolver.pl`:
  `run_differential.sh` (SWI vs the wamjs build, ≥2200 seeded catalogs); the
  `rust/` build (`resolver.pl` → `wam_rust`, `run_differential_rust.sh`,
  `run_corpus_rust.sh`); and the other target directories (`go`, `cpp`, `cljs`,
  `c`, plus `*_store` variants). State **demonstrated vs in-progress per target**;
  do not imply uniform parity.
- [ ] **Ch8 — Pruning & scale.** `docs/proposals/RESOLVER_PRUNING_DESIGN.md` —
  which pieces have shipped (G1 per-call index; the H1/H4 round) vs design-only;
  the SWI-vs-Rust scale crossover (`rust/run_scale_rust.sh`, `rust/swi_scale_ref.pl`,
  `rust/scale_to_case.mjs`); and the snapshot/dedup store
  (`resolver_snapshot_multi.pl`, `resolver_store_snapshot.pl`). Tie the pruning
  work to the performance result that motivated it.
- [ ] **Ch9 — Open problems.** Must include the verified gap: the `abi/` sub-lane
  is SWI-only — its only crosscheck is `readelf` vs `.symbols` evidence agreement
  (`abi/crosscheck.mjs`, `abi/run_abi_verify.sh`), not the resolver's cross-target
  differential.

## Engineering follow-on (code, not book work)
- [ ] Transpile the `abi/` sub-lane to a target and wire its SWI-oracle
  differential. Today it runs only in SWI; doing this upgrades Ch7's ABI story
  from goal to demonstrated and gives the chapter its strongest claim.

## Process notes
- Recon of in-repo source feeds a synthesis/first-draft pass; a stronger model
  does later edits and revisions; facts are verified against source before any
  review; then a multi-reader comprehension panel, then external review.
