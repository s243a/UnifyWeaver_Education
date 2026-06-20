<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Appendix B: UnifyWeaver-Internal Theory Beyond the Main Text

Appendix A pointed to external textbooks for theory outside the book's scope. This appendix is the opposite: UnifyWeaver-internal theory that *is* in the project's design documents but that the main chapters of book-18 did not unpack in full. Each entry summarises the concept, names why it matters, and points to the exact section in `docs/design/TREE_LIKENESS_INDEX.md` (the design note) or `docs/design/TREE_LIKENESS_INDEX_THEORY.md` (the theory doc) where the full treatment lives.

The appendix exists for two reasons. First, to flag to the reader of book-18 that the design notes are deeper than the book is — if a concept is named but not developed here, the design note is where to go. Second, to give the book itself a place to evolve: as future revisions move material from appendix to main chapter, the appendix shrinks and the chapters grow.

## B.1 The (graph, metric)-pair framing

**The concept.** Tree-likeness is not a property of a graph in isolation. It is a property of a *pair* (graph, metric) — the graph plus the choice of metric used to measure distances on it. The same graph can be highly tree-like under one metric and substantially non-tree-like under another. Chapter 4 named this framing; the design note develops it as the *thesis* of the entire document.

**Why it matters.** A common error in informal discussion is to ask "is this graph tree-like?" The right answer is *under which metric?* For Wikipedia categorisation:
- Under the *child-to-root shortest-path metric* (BFS depth), the graph is exactly tree-like — every node has a unique shortest path to root.
- Under the *weighted-power-mean metric `d_wPow`* with budget = BFS depth, the graph is *also* exactly tree-like (the empirical result of chapter 5).
- Under the *arbitrary-pair `d_wPow`* with loose budget, the graph is *not* tree-like — chapter 5 reports >39% of pairs have shortcut deviations.

Same graph; three different verdicts depending on the metric. The framing is what lets these statements coexist without contradiction.

**Where to read.** The design note's title — *Tree-likeness index: a (graph, metric)-pair statistic* — signals the framing. The full development is throughout the document, particularly §1 (the observation), §3 (the index definition), and §5.5 (convergence robustness, which formalises the per-pair dependence).

**Connections.** Appendix A.3 (graph minors and treewidth) names structural notions of "tree-like" that are about the graph alone — not (graph, metric)-pair properties. The distinction between *structural* and *metric* tree-likeness is one of the cleanest take-aways the book offers, and is not yet well-developed in the standard graph-theory literature.

## B.2 Statistical homogeneity (H1, H2, H3)

**The concept.** Chapter 4 introduced statistical homogeneity informally as the condition under which globally-computed calibration constants predict local behaviour. The theory doc §0.6 formalises homogeneity as three conditions:

- **H1 (local degree distribution).** The conditional degree distribution at a node, given its position in the graph (depth, topical region), is consistent with the global degree distribution. Formally: if `f(d | region)` is the local degree distribution and `f(d)` is the global, then `f(d | region) ≈ f(d)` for every region.

- **H2 (path-length distribution).** The number of intermediate parent-direction hops along typical paths between two nodes is approximately Poisson-distributed, with rate parameter consistent across regions.

- **H3 (convergence ratio).** The per-region convergence ratio `r = b'/(b_eff · D)` is approximately equal to the global `r`.

When H1, H2, and H3 hold, the global calibration constants `D` and `b_eff` predict local path-weight behaviour with bounded error.

**Why it matters.** The metric `d_wPow` uses global calibration. If the graph is inhomogeneous — if different regions have very different local properties — then the global calibration over-predicts in some regions and under-predicts in others. The aggregate metric still has a denotation, but its per-pair value diverges from the calibration-based prediction.

The empirical observation on Wikipedia: the full unrestricted categorisation graph is *inhomogeneous* (design note §4.4 reports the gap). Topical subgraphs are homogeneous to within measurement noise (design note §4.5). This drives the data-prep choice to build LMDBs rooted at topical nodes — see B.4 below.

**Where to read.** Theory doc §0.6 (definition). Design note §4.4 (empirical evidence of inhomogeneity on full graph). Design note §4.5 (empirical evidence of homogeneity on topical subgraph). Theory doc §1.5 (connection to friendship paradox under inhomogeneity).

**Open question.** A formal characterisation of *how much* inhomogeneity can be tolerated before the calibration breaks. Theory doc §5.2 (quantitative homogeneity ↔ calibration error) frames this as an open problem.

## B.3 Weights as path-count normalisers

**The concept.** Chapter 4 introduced the weights `w(p) = (1/D)^N · (1/(b_eff·D))^M` as the way the metric formula weights different paths. The construction can look ad-hoc on first reading. A cleaner restatement, developed in design note §5.6 and proved in theory doc §2.2: *the weights are chosen so that they normalise path counts to a probability distribution*.

**The principle.** Sum the weights over all paths from `u` to `v`. Under the homogeneity assumptions of B.2, the sum approximately equals 1. This means the weights are a *probability distribution* over the path set, not arbitrary positive numbers. The metric `d_wPow` is then a *weighted average* of path lengths, weighted by this distribution — not a weighted *sum* (which would be unbounded as path counts grow).

**Why this restatement is cleaner.** Three conceptual gains:

1. **Units make sense.** A weighted average of edge counts has units of edge count, matching BFS distance. A weighted sum would have units of edge count × probability — not directly comparable.

2. **The metric's value has an interpretation.** `d_wPow(u, v) = expected path length, drawn from the weight distribution`. This is meaningful in a way "magic combination of weights and power-means" is not.

3. **The connection to random walks becomes explicit.** The weight distribution corresponds to a specific random walk model: a walker that, at each step, chooses uniformly among available outgoing edges in each direction, with the friendship-paradox correction. The metric is the expected hop count from `u` to `v` under this walk.

**Where to read.** Design note §5.6 (the cleaner restatement). Theory doc §2.2 (the theorem: weights normalise path counts). Theory doc §5.7 (open question: weighting as expected-average over admissible paths — generalisations).

**Connection to chapter 4.** The child-step coherence principle (ch4) is a corollary: when the weight distribution concentrates on the BFS-shortest path (the regime where shortcuts are rare), the expected hop count equals the BFS distance + 1. The expectation interpretation makes the empirical "metric ≈ depth + 1" result conceptually inevitable rather than coincidental.

## B.4 Topical scoping as a sufficient condition for homogeneity

**The concept.** Wikipedia is structurally one big graph but conceptually many overlapping thematic clusters: Physics, History, Geography, Biology, etc. The full graph mixes these, producing an inhomogeneous calibration target. *Topical scoping* restricts attention to a single thematic subgraph — typically the descendants of a topical root node like `Category:Main_topic_classifications` or `Category:Physics`.

**The empirical claim (Conjecture 3.4 in the theory doc).** A topical subgraph of Wikipedia is statistically homogeneous in the sense of B.2 — H1, H2, and H3 hold to within measurement noise.

**The cross-wiki evidence.** Originally observed on simplewiki (~80k nodes). Confirmed on enwiki (~2.26M nodes, 30× larger). The cross-wiki anchor is recorded in the four-refinements update to the theory doc (theory doc §3.4 and §4.4) and in book-18 chapter 5.

**Why it matters.** Topical scoping is the *workaround* for the inhomogeneity gap of B.2. Rather than try to make calibration work on the full mixed-thematic graph, build the LMDB to contain only the topical subgraph and calibrate against that. The calibration then applies locally, and the metric behaves as theory predicts.

**Where to read.** Theory doc §3.4 (the conjecture and its statement). Theory doc §4.4 (empirical status with cross-wiki anchor). Design note §4.5 (the original simplewiki evidence). Design note §7.1 (data-prep consequences — building LMDB from a topical root).

**Open question.** Does topical scoping extend to non-Wikipedia hierarchies? The conjecture is well-supported on Wikipedia categorisation but has not been validated on citation networks, software dependency graphs, organisational charts, or other graphs with similar thematic-cluster structure.

## B.5 Symmetric DAGs have high TLI under any directional metric

**The concept (Conjecture 3.5 in the theory doc).** A DAG whose structure is symmetric — roughly, where the parent-direction and child-direction degree distributions match in some formal sense — has high tree-likeness index under *any* directional metric. The directional weighting cannot rescue tree-likeness on a symmetric DAG.

**Why it matters.** Identifies a class of graphs where TLI predictions systematically fail. Wikipedia is *not* symmetric — child and parent degree distributions are quite different — and the asymmetry is part of what makes the directional metric work. A graph that happens to be symmetric (some citation networks, some organisational structures) would not benefit from the same approach.

**Where to read.** Theory doc §3.5.

**Connection to forward direction.** This is a falsification target: a synthetic symmetric DAG should exhibit high TLI under `d_wPow`. The task is one of the open empirical items in chapter 11 (and is task #10 in the project tracker — *Test metric-tree-likeness hypothesis on a synthetic non-tree-like graph*).

## B.6 Routing-correction redundancy under topical scoping

**The concept (Conjecture 3.6 in the theory doc).** The metric formula includes a *routing-correction* term that accounts for parent-direction shortcuts (paths that go up and then back down through alternate ancestors). Under topical scoping, this correction may be *partially redundant* — because topical scoping already captures the parent-direction shortcut structure implicitly through the local subgraph topology, the explicit correction term double-counts.

**Why it matters.** If the correction is redundant under topical scoping, the metric formula can be simplified for the production case (which always uses topical scoping). The simpler formula would be easier to reason about, faster to compute, and more amenable to closed-form analysis.

**Status.** Conjectured but not empirically tested. The F# kernel infrastructure to run the experiment is now in place (task #17 completed). The experiment itself — measuring `d_wPow` with and without the routing correction on topically-scoped subgraphs — is the project's open task #14.

**Where to read.** Design note §5.4 (the original framing). Theory doc §3.6 (the conjecture). Theory doc §4.6 (empirical status — pending the experiment).

**Why it's in the book.** Chapter 11 §empirical-questions names this work item as one of the open project items. The conjecture is a good example of what the design-note theory predicts vs what the empirical work has not yet validated — and is the cleanest single experiment that would change the book's content.

## B.7 Convergence robustness — feature and trap

**The concept.** The metric is well-defined under the *convergence condition* `b_eff · D > b'` (equivalently `r < 1`). The condition is robust in two distinct senses, which interact unhelpfully:

- **Feature: certification robustness.** A small error in estimating `b_eff` does not flip the convergence verdict. If the true `r = 0.04` and the estimator gives `r̂ = 0.05`, the metric is still well-defined and the per-pair check still passes.

- **Trap: cost-model brittleness.** A cost model that assumes a *tight* convergence margin (e.g. "convergence is fast because `r` is small") is *not* robust. If the estimator slightly under-predicts `r`, the cost model's prediction of fast convergence may turn out wildly optimistic — convergence happens but slowly, and the per-query cost balloons.

**Why it matters.** The two senses pull in opposite directions for system design. For certificates (does the metric exist? is the per-pair check valid?), the robustness is helpful — you can trust the certificate. For cost models (how fast does the iteration converge? what is the per-query budget?), the robustness is a trap — small estimator errors translate to large cost-prediction errors.

**Where to read.** Design note §5.5 (the original framing — both senses named explicitly). Theory doc §5.6 (open question: convergence robustness under `ρ` miscalibration — how does the cost-model brittleness quantify formally?).

**Practical advice.** Compute *two* margins, not one. The certificate margin is `r - 1` (well-defined iff negative). The cost-model margin is the *effective* `r̂ - r` — the gap between the estimator and the truth, governing how badly the cost prediction can fail. Production systems should track both.

## B.8 Geometric-vs-metric decoupling

**The concept.** A graph has *geometric* properties (degree distribution, diameter, clustering coefficient) and *metric* properties (the distribution of pairwise `d_wPow` values, the per-node TLI distribution). One might expect the two to be tightly coupled: if the degree distribution is heavy-tailed, the metric distribution should reflect this; if the diameter is small, the metric values should be small.

The empirical finding of design note §4.6 (task #15): they are *not* as tightly coupled as one might think. Specifically, on the simplewiki topical-Articles subgraph, the BFS-distance distribution between random pairs follows a clear geometric (small-world) pattern, while the `d_wPow` distribution differs in shape — the metric integrates over paths in a way that decouples from raw BFS distance.

**Why it matters.** Cautions against drawing metric conclusions from geometric measurements. A graph that "looks small-world" by BFS-distance histograms does not automatically produce a tightly-concentrated `d_wPow` distribution. The reverse also holds. Calibration constants need to be computed against the metric being used; geometric proxies are unreliable.

**Where to read.** Design note §4.6 (the empirical finding).

**Open question.** A theoretical understanding of *when* geometric and metric properties decouple, and by how much. The empirical observation is suggestive but not predictive — given a graph's geometric profile, the book cannot yet predict the metric profile.

## B.9 The inhomogeneity gap

**The concept.** When global calibration constants are computed over the full unrestricted Wikipedia categorisation graph (not topically scoped), and then used to predict local `d_wPow` values, the predictions are systematically off. This is the *inhomogeneity gap*.

**The empirical evidence.** Design note §4.4 reports the gap: globally-calibrated `b_eff ≈ X` predicts local convergence ratio `r_predicted`; empirically-measured local `r` differs by a factor of `~Y` (where `Y` is large enough to be meaningful, not a rounding-error effect).

**Why it matters.** The gap is the diagnosis behind the topical-scoping prescription (B.4). Without an explanation, the metric formula appears to be poorly-tuned. With the inhomogeneity explanation, the formula is fine and the calibration domain was wrong.

**Where to read.** Design note §4.4 (the gap measurement). Design note §4.5 (topical scoping as the fix).

**Connection to other appendices.** Inhomogeneity (this entry) and topical scoping (B.4) are two sides of the same observation — the problem and its workaround. Future revisions of the book may merge them into a single subsection in the main text once the routing-correction-redundancy experiment (B.6) settles.

## B.10 Data-prep consequences

**The concept.** The choice of LMDB build strategy matters more than it might appear. There are at least three meaningful choices:

- **Full unrestricted ingest** — include every category Wikipedia defines. Inhomogeneous; calibration is unreliable; queries return semantically reasonable answers but the metric values do not match theory predictions.
- **Topical-root ingest** — include only categories reachable from a topical root (e.g. `Main_topic_classifications`). Homogeneous; calibration is reliable; queries match theory.
- **Articles-rooted ingest** — a specific variant rooted at `Category:Articles` rather than the topical root. Used in some early experiments; produces a different homogeneous subgraph.

**The recipe.** Design note §7.2 captures the calibration recipe associated with topical-root ingest: which constants to compute, when to compute them, what the per-query path looks like once calibration is in hand.

**Why it matters.** Choosing the wrong ingest strategy invalidates the calibration. The same kernel code, run against a full unrestricted LMDB, produces metric values that look correct but are *wrong* relative to the theory. Users not following the data-prep recipe should not be surprised when their results do not match the design note's predictions.

**Where to read.** Design note §7 (the full data-prep section), particularly §7.1 (rationale) and §7.2 (recipe summary).

**Future direction.** Chapter 11 §compilation-infrastructure names "cost-model integration with ingest decisions" as a forward-direction item. The right end state is that the compiler reads the algorithm manifest and chooses the ingest strategy automatically, rather than the user choosing it manually. This subsection captures what the manual choice currently looks like.

## B.11 Extensions: undirected graphs and effectively-infinite graphs

Two extensions of the theory are flagged as open in the theory doc.

**Undirected graphs (theory doc §5.4).** The directional weighting `(1/D)^N · (1/(b_eff · D))^M` distinguishes parent-direction from child-direction. On an undirected graph, this distinction is not meaningful. A formulation that works for undirected graphs — perhaps a uniform weighting, perhaps a different decomposition entirely — is an open theoretical question. Connection: social networks, citation graphs treated as undirected, road networks.

**Effectively-infinite graphs (theory doc §5.8).** The book has assumed throughout that the graph is finite. For graphs that are conceptually unbounded (the web at a snapshot is finite, but a continuously-growing graph is not), the convergence analysis needs revisiting. Path-counting metrics on an infinite graph have well-defined limits only under specific summability conditions that may or may not hold.

**Where to read.** Theory doc §5.4 (undirected), §5.8 (effectively-infinite). Neither is empirically tested; both are flagged as open theoretical questions in the design notes.

**Why it matters for the book.** Both extensions are flagged for completeness — to make clear what the book's framework does *not* cover. If a reader wants to apply the framework to undirected graphs or infinite graphs, the book's apparatus does not directly apply, and these design-note sections are the right starting point for the adaptation work.

## B.12 Connection to spectral expansion

**The concept (theory doc §5.5).** Spectral expansion is a graph property: the *Cheeger constant* `h(G)` measures the worst-case sparsity of cuts; the spectral gap of the Laplacian bounds it from both sides (Cheeger's inequalities). Both quantify how *well-mixed* the graph is — how quickly information or random walks spread.

**The conjecture.** TLI and spectral expansion should be related. Intuitively: a graph with strong spectral expansion has many short paths between random pairs, which should drive `d_wPow` toward tighter agreement with BFS distance (lower TLI). A graph with weak spectral expansion has bottleneck cuts that constrain path counts, possibly producing tighter alignment with shortest-path metric (also lower TLI) — or producing distinct path-clusters with high TLI between clusters.

The relationship is *predicted* by the theory doc to exist; the form has not been worked out.

**Why it matters.** Spectral expansion is a well-studied quantity with deep connections to random-walk mixing, expander graph constructions, and pseudo-random generation. If TLI relates to spectral expansion, the book's framework connects to a much larger body of established theory.

**Where to read.** Theory doc §5.5 (open question). Appendix A.1 (spectral graph theory background).

**Open question.** A formal characterisation of the TLI–spectral-expansion relationship. Likely depends on assumptions about the graph (sparsity, degree distribution, scale-free vs uniform-degree). This is research-grade work that has not been done.

## B.13 `r = b'/(b_eff·D)` as contraction rate: conjecture and tracked theory work

**The concept.** The convergence ratio `r = b'/(b_eff · D)` defined in `docs/design/TREE_LIKENESS_INDEX.md` §2 (UnifyWeaver main repo) is treated, throughout the project's design work, as the **spectral contraction rate** for the linearised `d_wPow` iteration operator. Theorem 2.3 of the theory doc gives the bound `r/(1−r)` on the per-step contribution from longer paths — a geometric-series bound that has the same form as the convergence rate of a contraction mapping with rate `r`.

**The conjecture.** That `r` (a graph-structural quantity, computed from degree statistics) actually equals (or is asymptotically equal to, in an appropriate sense) the spectral radius of the linearised `d_wPow` iteration operator (an operator-theoretic quantity that would be computed by spectral decomposition).

**Why this is a conjecture rather than a theorem.** A rigorous identification requires tightening three things at once:

1. **Which norm** the spectral analysis is being done in. The spectral radius `ρ(B)` of an operator `B` is well-defined; but its relationship to convergence rates depends on operator-norm choices that have to be made explicit.
2. **Which operator linearisation** of the `d_wPow` iteration is being analysed. The `d_wPow` recurrence is non-linear in its weight construction; the contraction-rate analysis assumes a specific linearisation (around the fixed point, or in a suitable functional setting). The choice of linearisation determines the operator whose spectrum is at stake.
3. **The assumption that the weighted-degree distribution is approximately uncorrelated.** The friendship-paradox quantity `E[d²]/E[d]` is a known estimator for the spectral radius of a configuration-model random graph's adjacency matrix. `b_eff` extends this to directional weighted edges; the extension is plausible under configuration-model-style independence assumptions but can diverge from the true spectral radius on graphs with strongly correlated degree distributions.

The intuition is sound, the bound has the right form, and empirical observations on Wikipedia categorisation (design note §3.3, §4.5) are consistent with the conjecture. But the precise proof that `r` equals the spectral radius of the linearised operator under stated assumptions has not been constructed in the project to date.

**Why it matters operationally — the dependence is load-bearing.** The Recurrence Evaluation Strategy module (`docs/design/RECURRENCE_EVALUATION_STRATEGY_*.md` in the main repo) uses `r` operationally for two purposes:

- **Gating `fixed_point` admissibility** for numeric recurrences: the selector refuses to admit `fixed_point` when `r ≥ 1` (no convergence guarantee). This is a hard test in `admissible_strategies/2`.
- **Cost-model rules** for predicting convergence speed when fixed_point is chosen: the iteration-count prediction is `~log(ε)/log(r)` for tolerance `ε`, derived from the geometric-series rate.

Both uses assume `r` reliably tracks what the linearised operator's spectral radius actually does. If the conjecture is wrong in a substantive way (e.g. on a graph class where the configuration-model assumption breaks down hard), the cost-model rules can be wildly off and the admissibility gate can mis-classify. The cost rules are robust to *small* estimator errors (see [B.7 — convergence robustness](#b7-convergence-robustness--feature-and-trap)) but not to a fundamentally wrong identification.

**Where to read.**

- `docs/design/RECURRENCE_EVALUATION_STRATEGY_PHILOSOPHY.md` §*Theory connection: `r` is conjectured to be the contraction rate* — the design's framing of the conjecture and the hedge that it's load-bearing-but-unproven.
- `docs/design/RECURRENCE_EVALUATION_STRATEGY_PHILOSOPHY.md` §*Spectral connection in plainer terms* — eigenvalues, condition number, spectral radius, diagonal dominance for readers approaching from linear algebra; includes scoping note on condition-number being SPD-specific while `d_wPow` is directed.
- `docs/design/RECURRENCE_EVALUATION_STRATEGY_IMPLEMENTATION_PLAN.md` risk-table row: "the r = diagonal-dominance conjecture turns out to be wrong in a substantive way" — names the conjecture as a tracked risk with mitigation framing.
- `docs/design/TREE_LIKENESS_INDEX.md` §2 — original definition of `r`.
- `docs/design/TREE_LIKENESS_INDEX_THEORY.md` §2.3 — the convergence theorem and the `r/(1−r)` bound.

**Status: tracked theory work.** This entry exists to make the conjecture's load-bearing dependence visible — the design depends on it; the rigorous proof is future work; the framing acknowledges both. When the proof is constructed (and possibly relocated to `TREE_LIKENESS_INDEX_THEORY.md` as a real theorem), this appendix entry can be revised to point at the theorem rather than the conjecture, and the RES design docs' hedges can be retired.

This is also a small but interesting research-grade theoretical question — the kind of thing a Master's thesis or a focused paper could address. Anyone wanting to contribute to the project's theoretical foundations could pick this up and work it out.

## Closing

Appendix B is the book's pointer to its own depth. The design notes are *more* than the book — they include conjectures the book does not state, empirical work the book summarises briefly, and theoretical extensions the book does not touch. The appendix exists to make that depth visible: a reader who wants more than book-18 provides knows where to go.

The appendix should shrink over time. As future revisions of book-18 close the open items chapter 11 names, the corresponding appendix B entries get promoted into main chapters and the appendix gets shorter. The current state is the maximum size; subsequent versions should be smaller.
