<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2026 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Appendix A: Theory Beyond the Book — Further Reading

The main chapters deliberately stop short of several adjacent areas of theory. This appendix is the *See Also* — for each topic, a brief description, why it might matter to the book's themes, and pointers to where to learn more. The recommended resources are textbooks, foundational papers, and open courseware that the book's authors have personally found helpful. URLs are given only when they are stable and canonical; for the rest, the author/year/title is enough to find the source.

## A.1 Spectral graph theory

Chapter 4 introduced `D` and `b_eff` as *loose* spectral estimators — equivalent-ish to the spectral quantities that govern path-count growth, computed as degree-distribution shortcuts without paying for spectral decomposition. This appendix entry develops the connection.

**What it is.** Spectral graph theory studies graphs via the eigenvalues and eigenvectors of matrices associated with the graph — typically the adjacency matrix `A`, the Laplacian `L = D − A` where `D` is the diagonal degree matrix, or the normalised Laplacian `I − D^{−1/2} A D^{−1/2}`. The eigenvalues encode global structural properties: the largest eigenvalue of `A` (the **spectral radius**, also called the Perron–Frobenius eigenvalue for non-negative matrices) governs the long-term growth rate of paths; the second-smallest eigenvalue of `L` (the **spectral gap** or **algebraic connectivity**) governs how well-connected the graph is; the eigenvalue distribution as a whole controls mixing times of random walks and convergence rates of iterative algorithms.

**The connection to `b_eff`.** For a graph drawn from a configuration model with degree sequence determined by `E[d]` and `E[d²]`, the spectral radius converges to `E[d²]/E[d]` as the graph grows. This is the friendship-paradox quantity (chapter 3 §5). The book's `b_eff` for the directional case takes the ratio of this quantity computed on child-edges to the same quantity on parent-edges — an estimator for the directional analog of the spectral radius. The estimator is much cheaper than spectral decomposition (which is `O(V³)` for a dense matrix, or iterative Lanczos for sparse) but is asymptotically equivalent for well-behaved degree distributions.

**Why it matters beyond the book.** Spectral methods underpin many graph algorithms beyond the metric-distortion focus of book-18: PageRank (power iteration on a stochastic matrix), spectral clustering (k-means on Laplacian eigenvectors), Cheeger inequalities (spectral gap bounds graph conductance), expander graph constructions, and random walks generally. The Laplacian's eigenvectors form a natural basis for graph signal processing.

**Reading.**
- Daniel A. Spielman, *Spectral and Algebraic Graph Theory*. Yale course notes, freely available online — the most accessible introduction at the graduate level.
- Fan R. K. Chung, *Spectral Graph Theory*. CBMS Regional Conference Series in Mathematics 92, AMS (1997). The canonical reference; quite dense.
- Mark Newman, *Networks: An Introduction* (2nd ed., 2018), chapters 6–7 — spectral methods in the context of network science.

## A.2 Random graph models

**What it is.** A random graph model specifies a probability distribution over graphs with given properties. The classic Erdős–Rényi model `G(n, p)` places each possible edge independently with probability `p`. The configuration model fixes a degree sequence and randomly matches half-edges. The Chung–Lu model extends this to *expected* degrees. Preferential-attachment models (Barabási–Albert) grow a graph dynamically, attaching new nodes with probability proportional to existing degree, and produce scale-free distributions.

**Why it matters.** Random graph models serve as *null models* — baselines for what "random" looks like with given constraints. When a real graph has properties (small diameter, high clustering, scale-free degree) that the random model also has, the property may not require explanation beyond random-graph statistics. When the real graph differs from the model, the difference is meaningful structure to explain.

The book invokes Chung–Lu implicitly: the friendship-paradox quantity `E[d²]/E[d]` is the configuration-model estimator for the spectral radius. The Cohen–Havlin ultra-small-world result (chapter 3 §6) applies to scale-free graphs that conform to the Chung–Lu degree distribution.

**Reading.**
- Béla Bollobás, *Random Graphs* (2nd ed., 2001). The classic mathematical treatment.
- Mark Newman, *Networks: An Introduction* (2nd ed., 2018), chapters 12–15.
- Albert-László Barabási, *Network Science* (2016), available online at `networksciencebook.com`. Accessible textbook; chapters 3–4 cover Erdős–Rényi and configuration models; chapter 5 covers preferential attachment.

## A.3 Graph minors and treewidth

**What it is.** A graph `H` is a **minor** of `G` if `H` can be obtained from `G` by deleting edges, deleting vertices, and contracting edges. Robertson and Seymour's *Graph Minor Theorem* (proved over a 20-paper series ending in 2004) states that any property closed under minors can be characterised by a finite set of *forbidden minors*. **Treewidth** is a numerical measure of how close a graph is to being a tree: a tree has treewidth 1; a cycle has treewidth 2; a `k × k` grid has treewidth `k`. Graphs of bounded treewidth admit linear-time algorithms for many problems that are NP-hard in general.

**How this differs from book-18's "tree-likeness".** Treewidth is a *structural* measure — about the graph's topology in the strictest sense. The tree-likeness index of this book is a *metric* measure — about how path-counting metrics behave. The two are not the same. A graph can have high treewidth but low TLI (if its non-tree edges contribute negligibly to the weighted-power-mean), or low treewidth but high TLI (if its near-tree structure carries metric-relevant deviations). The (graph, metric)-pair framing (appendix B.1) names the distinction.

**Why it might matter.** A more comprehensive treatment of "tree-like" structure on graphs would relate the metric-based TLI to the structural treewidth and to other notions (treedepth, hyperbolicity, doubling dimension). The book does not attempt this comparison; it is a research-grade topic in its own right.

**Reading.**
- Reinhard Diestel, *Graph Theory* (5th ed., 2017), Springer GTM 173, chapter 12.
- Lovász, "Graph Minor Theory" (2006), *Bull. AMS*, for a high-level survey.
- For treewidth and algorithmic uses, Hans L. Bodlaender's survey papers.

## A.4 Graph isomorphism and subgraph matching

**What it is.** Two graphs are **isomorphic** if there is a bijection between their nodes that preserves edges. The graph isomorphism problem asks whether two given graphs are isomorphic. **Subgraph isomorphism** asks whether one graph contains another as a subgraph.

**Where it sits.** Graph isomorphism's complexity status is unusual: not known to be in P, not known to be NP-complete. Babai's 2016 result places it in quasi-polynomial time, the current best bound. Subgraph isomorphism is NP-complete in general.

**Why it sits outside the book's themes.** UnifyWeaver queries *over* a graph; it does not query *between* graphs. The book's algorithms do not need isomorphism testing. The topic is included here because chapter 3 §9 named it as deliberately excluded, and a reader looking for "the graph theory topic UnifyWeaver doesn't do" should find it here.

**Reading.**
- László Babai, "Graph Isomorphism in Quasipolynomial Time" (2016), preprint and several survey treatments.
- For subgraph matching in practice (e.g. pattern matching on knowledge graphs), the literature on graph databases and Cypher-style query languages is more relevant than the classical complexity literature.

## A.5 Alternative metrics on graphs

Chapter 4 §9 listed several metrics the book did not cover. This entry expands the list with reading recommendations.

**Resistance distance.** Treats the graph as an electrical circuit: each edge is a unit resistor, and the resistance distance between two nodes is the effective resistance of the network between them. Equivalent to the *commute time* up to a factor. Captures the "many redundant paths" effect via parallel resistors — two nodes connected by many short paths have lower resistance than two connected by a single long one. Computationally expensive — typically requires solving a linear system involving the Laplacian.

Reading: Peter G. Doyle and J. Laurie Snell, *Random Walks and Electric Networks* (1984), available as a free Dartmouth PDF. The canonical introduction.

**Commute time and hitting time.** The *hitting time* `h(u, v)` is the expected number of steps for a random walk starting at `u` to first reach `v`. The *commute time* `c(u, v) = h(u, v) + h(v, u)` is symmetric. Both are well-defined on connected undirected graphs and on irreducible Markov chains. Commute time is proportional to resistance distance.

Reading: Newman, *Networks* (2018), chapter 7; Aldous and Fill, *Reversible Markov Chains and Random Walks on Graphs* (unfinished book, available online).

**Personalised PageRank.** A random walker that, at each step, with probability `α` continues along an outgoing edge and with probability `1−α` teleports back to a designated start node. The stationary distribution is the *personalised PageRank vector* and can be used as a similarity measure between the start node and every other node.

Reading: the original PageRank technical report (Page–Brin–Motwani–Winograd 1998); for personalised PageRank specifically, Haveliwala (2002) "Topic-Sensitive PageRank".

**SimRank.** Measures node similarity by the recursive principle "two nodes are similar if they are referenced by similar nodes". Computed by iterative refinement.

Reading: Jeh and Widom (2002) "SimRank: a measure of structural-context similarity", KDD.

## A.6 Datalog semantics and bottom-up evaluation

Chapter 7 invoked bottom-up Datalog evaluation as the canonical example of a difference-equation compilation strategy. This entry collects the underlying theory.

**The semantics.** A Datalog program is a finite set of rules of the form `H :- B1, ..., Bn` where `H` is a single positive literal (the head) and each `Bi` is a positive literal (the body). The semantics is the *least fixed point* of the immediate-consequence operator `T_P`: starting from the extensional database (the input facts), iterate `T_P` until no new facts are derived. The result is the unique minimal model of the program plus extensional database.

**Semi-naive evaluation.** The naive evaluation recomputes `T_P` from scratch at each iteration, redoing all the previous work. Semi-naive evaluation tracks the *delta* — only the facts derived at the previous iteration — and joins these against the rest. Each fact is derived at most once. The total work is bounded by the size of the final model times a constant per rule.

**Magic sets.** A program transformation that makes bottom-up evaluation goal-directed: instead of computing everything derivable, restrict the iteration to facts relevant to a specific query. Combines the benefits of bottom-up (no infinite recursion) with top-down (only do relevant work).

**Stratified negation.** Datalog with negation is not always well-defined (consider `p :- not p`). Stratification partitions predicates into strata such that each predicate depends only on lower-stratum predicates. Each stratum is evaluated bottom-up before higher strata begin.

**Reading.**
- Stefano Ceri, Georg Gottlob, Letizia Tanca, "What you Always Wanted to Know About Datalog (And Never Dared to Ask)" (1989), *IEEE TKDE*. The classic accessible survey.
- Serge Abiteboul, Richard Hull, Victor Vianu, *Foundations of Databases* (1995), available freely online. Comprehensive treatment, chapters 12–13 on Datalog.
- Todd J. Green, Shan Shan Huang, Boon Thau Loo, Wenchao Zhou, "Datalog and Recursive Query Processing" (2013), *Foundations and Trends in Databases*.

## A.7 Prolog evaluation and tabling

**Top-down SLD resolution.** Standard Prolog's evaluation strategy: starting from a query, resolve against rules left-to-right, using depth-first search. Backtracking on failure. Cuts (`!`) prune the search tree explicitly. Vulnerable to non-termination on left-recursive rules and to redundant work when subgoals are recomputed.

**Tabling (SLG resolution).** A more sophisticated strategy that memoises subgoal calls and answers. When a tabled subgoal is called repeatedly, the second and later calls return the memoised answers instead of recomputing. SLG resolution handles left recursion gracefully: a recursive call to a tabled predicate suspends until the answer set grows.

**Implementations.** XSB Prolog is the canonical tabling implementation. SWI-Prolog supports tabling natively (`:- table predicate/arity`). Both support *well-founded semantics* for negation, which is more permissive than stratification.

**Connection to bottom-up.** Tabling effectively bridges top-down and bottom-up: the tabled predicate's answers are computed bottom-up (via fixed-point iteration of the tabled subgoals), but the call pattern is top-down (only relevant subgoals are tabled).

**Reading.**
- Leon Sterling and Ehud Shapiro, *The Art of Prolog* (2nd ed., 1994). Classic Prolog text; covers SLD resolution and traditional Prolog idioms.
- Terrance Swift and David Warren, "XSB: Extending Prolog with Tabled Logic Programming" (2012), *Theory and Practice of Logic Programming*.
- SWI-Prolog tabling documentation.

## A.8 Tarski's fixed-point theorem and lattice theory

**The theorem.** A monotone function on a complete lattice has a least and a greatest fixed point. The least fixed point can be computed by iterating the function from the bottom of the lattice (ascending Kleene iteration); convergence is guaranteed for functions on finite lattices and for *continuous* functions on infinite lattices.

**Why graph algorithms care.** The immediate-consequence operator `T_P` of a Datalog program is a monotone function on the lattice of subsets of the Herbrand base (ordered by inclusion). Tarski's theorem guarantees that the iteration converges to the least fixed point — which is the program's intended semantics. Other graph computations (transitive closure, single-source shortest paths via Bellman–Ford, dataflow analyses generally) have the same shape.

**Reading.**
- Brian A. Davey and Hilary A. Priestley, *Introduction to Lattices and Order* (2nd ed., 2002). The standard accessible introduction.
- The theorem itself: Alfred Tarski, "A lattice-theoretical fixpoint theorem and its applications" (1955), *Pacific Journal of Mathematics*.

## A.9 Power-law fitting methodology

**The problem.** A common mistake in network-science papers: fit a power law `P(k) ∝ k^{−γ}` by linear regression on a log-log plot. This is statistically wrong — the noise structure is not what linear regression assumes, and the resulting `γ` estimate is biased.

**The Clauset–Shalizi–Newman method.** Use maximum likelihood estimation to fit the power law, and use the Kolmogorov–Smirnov statistic to test whether the power-law hypothesis is preferred over plausible alternatives (log-normal, exponential, stretched exponential). The paper provides estimators, hypothesis tests, and a methodology critique.

**Why it matters for book-18.** The Cohen–Havlin ultra-small-world result depends on `γ < 3`. Whether a graph satisfies this depends on a *correct* fit. The naïve log-log regression can give the wrong answer about which regime a graph is in. Several of the cited results (Watts–Strogatz exponents, Barabási–Albert predicted γ = 3) depend on careful fitting methodology to validate.

**Reading.**
- Aaron Clauset, Cosma R. Shalizi, Mark E. J. Newman, "Power-Law Distributions in Empirical Data" (2009), *SIAM Review* 51(4):661–703. Open-access via SIAM; the canonical reference and a model of careful empirical methodology.

## A.10 Scale-free graph theory: foundational papers

The chapter-3 primer named several foundational results. Full citations and brief context here.

**Watts and Strogatz, "Collective dynamics of small-world networks"** (1998, *Nature*). Introduced the small-world model — a regular lattice with a small fraction of random rewiring — and observed that real networks (the C. elegans neural network, the actor collaboration network, the US power grid) had small-world structure. The first widely-read paper on what "small-world" means quantitatively.

**Barabási and Albert, "Emergence of scaling in random networks"** (1999, *Science*). Introduced the preferential-attachment model and the scale-free property. The model produces a power-law degree distribution with `γ = 3` analytically. Sparked the modern field of network science.

**Cohen and Havlin, "Scale-Free Networks Are Ultrasmall"** (2003, *Physical Review Letters*). Proved that scale-free graphs with `γ < 3` have *doubly-logarithmic* typical distances — `O(log log N)` rather than the merely-logarithmic `O(log N)` of Watts–Strogatz. The result book-18 invokes when classifying Wikipedia as ultra-small-world.

**Feld, "Why your friends have more friends than you do"** (1991, *American Journal of Sociology*). The original friendship-paradox paper. Phrased as a sociological observation; the mathematical content is the configuration-model bias toward high-degree nodes when sampling by edge.

**Chung and Lu, *Complex Graphs and Networks*** (2006, CBMS Regional Conference Series 107). The book-length treatment of the Chung–Lu random graph model that book-18 implicitly relies on for the `b_eff = E[d²]/E[d]` estimator.

## A.11 Open courseware

Several university courses on graph algorithms, network science, and Datalog are openly available and worth recommending as alternative entry points.

- **Stanford CS224W** (Jure Leskovec), *Machine Learning with Graphs*. Lecture videos and slides freely posted. Covers spectral methods, random walks, GNNs, recommendation systems on graphs.
- **MIT 6.S897** (David Karger), *Algorithms for Graphs*. Classical graph algorithms; lecture notes online.
- **CMU 15-826** (Christos Faloutsos), *Multimedia Databases and Data Mining*. Covers graph mining, network science, the Clauset–Shalizi–Newman methodology and similar empirical techniques.
- **Cornell CS6113** (Joseph Halpern), *Decision Theory*. Covers fixed-point semantics and lattice theory in a setting accessible to CS-trained readers.

## A.12 Software libraries doubling as tutorials

Some open-source libraries are well-documented enough to serve as tutorials for the underlying theory.

- **NetworkX** (Python). Comprehensive graph library; documentation covers every algorithm with references to the source paper. Good for hands-on exploration of any concept in this appendix.
- **graph-tool** (Python/C++). Faster than NetworkX for large graphs; documentation includes mathematical background.
- **Soufflé**. A Datalog engine designed for static analysis; the tutorial and documentation are a practical introduction to Datalog programming and semi-naive evaluation.
- **DDlog**. Differential-Datalog for incremental computation; documentation explains the incremental-maintenance semantics.

## Closing

Appendix A is deliberately a *list of starting points*. Each entry assumes the reader has motivation to follow up — the book does not develop any of these topics in depth. If the reader wants the book's perspective on a topic, the relevant chapter is the right place; if the reader wants to leave the book and learn an area properly, the resources above are where to go.

Appendix B turns to the opposite direction: UnifyWeaver-internal concepts the main chapters did not unpack, with pointers to the design notes inside the project.
