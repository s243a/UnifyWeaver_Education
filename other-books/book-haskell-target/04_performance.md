# Chapter 4: Performance — Making Haskell Beat Rust

Haskell has a reputation for being slow. This chapter shows how the
UnifyWeaver Haskell WAM target actually ended up **outperforming the Rust
WAM target** on a realistic benchmark — and what Haskell-specific lessons
that journey taught.

> **Prerequisite:** This chapter assumes you've read the first three
> chapters. It references the WAM target, which compiles Prolog to a
> Warren Abstract Machine interpreter in Haskell. You can follow the
> perf lessons without deep WAM knowledge, but some context (registers,
> choice points, unification) will help.

## The benchmark

We use the *effective-distance* benchmark: for each seed category in a
Wikipedia-derived graph of ~6000 category-parent edges, compute the
weighted sum of reciprocal hop counts to a root category. 386 seeds,
moderate recursion depth (~10), lots of `HashMap` lookups.

This benchmark is representative of graph analytics workloads — the
kind of thing where you'd expect a mutable-state language like Rust
to have a comfortable lead.

## Starting point

Initial Haskell WAM target: **~4700ms** per 300-scale run.
SWI-Prolog on the same workload: **311ms**.
Hand-tuned Rust WAM target: **126ms**.

The Haskell target was **15x slower than Prolog**, **37x slower than Rust**.
Hence the reputation.

## Where we ended up

After systematic optimization, final numbers (median of 10 runs, 4 cores):

| Target | Total time | Query time |
|---|---|---|
| Haskell + FFI + parallel | **75ms** | **32ms** |
| Rust + FFI (single-threaded) | 126ms | — |
| SWI-Prolog | 311ms | — |
| Haskell initial | ~4700ms | — |

**Net Haskell speedup: ~63x.** Haskell now beats Rust by 1.75x on total
time and 4.5x on query time.

The rest of this chapter is the story of how.

## Lesson 1: Match containers to access patterns

The single biggest early win was replacing `Data.Map String Value` with
`IntMap Value` for register storage.

### Before

```haskell
-- Register file keyed by string names: "A1", "A2", "X3", ...
wsRegs :: Map.Map String Value
```

Every register access hashes the string. In a tight interpreter loop,
that's expensive. And the string key carries no information the compiler
didn't already have — register IDs are known at compile time.

### After

```haskell
-- Register IDs pre-interned to Ints at compile time:
--   A1-A99  -> 1-99
--   X1-X99  -> 101-199
--   Y1-Y99  -> 201-299
type RegId = Int

wsRegs :: IM.IntMap Value
```

`IntMap` uses the Int directly as a Patricia-trie key. No hashing, no
string comparison. O(log n) but with a very small constant.

**Haskell-specific takeaway:** `Data.HashMap` is great for truly arbitrary
keys, but `Data.IntMap` beats it when your keys are small integers.
Default to `Data.IntMap` for dense Int keys; only reach for `HashMap`
when your keys are String or user-defined types with Hashable instances.

## Lesson 2: Cache lengths the container doesn't track

Haskell lists don't cache their length. Neither does `Data.HashMap.Strict`
(it computes size by walking buckets). If you call `length` or `size`
in a hot path, you're re-computing it every time.

### The bug we hit twice

Both in the WAM choice point stack and later in the atom-intern table:

```haskell
-- BAD: wsCPsLen computed from wsCPs at every access
wsCPs :: [ChoicePoint]
currentDepth = length (wsCPs s)  -- O(n) per call!
```

vs.

```haskell
-- GOOD: cache the length as a strict field
wsCPs    :: ![ChoicePoint]
wsCPsLen :: {-# UNPACK #-} !Int  -- maintained invariant
```

The `UNPACK` pragma forces the Int to be unboxed — stored directly in the
record rather than as a pointer to a heap-allocated Int.

### Even sneakier: HashMap.size inside a fold

While building the atom intern table:

```haskell
-- O(n^2): HashMap.size walks the whole map every iteration
foldl' (\m s -> if Map.member s m
                  then m
                  else Map.insert s (Map.size m) m)  -- bug
       Map.empty atomStrings
```

For 4000 atoms this was ~10% of total runtime. Fix: thread a counter
variable alongside the map:

```haskell
-- O(n log n): no size call in the loop
foldl' (\(!m, !i) s -> if Map.member s m
                         then (m, i)
                         else (Map.insert s i m, i + 1))
       (Map.empty, 0 :: Int) atomStrings
```

**Haskell-specific takeaway:** Lazy evaluation makes it easy to
accidentally redo work. If something looks O(1) but walks a data
structure every call, Haskell won't warn you. Profile, then cache.

## Lesson 3: Split hot and cold state

A WAM interpreter updates some fields on every step (program counter,
registers, bindings) and some rarely (the code array, label map, foreign
fact index). Bundling them in one record means every step copies fields
it didn't change.

### Before

```haskell
data WamState = WamState
  { wsPC     :: !Int             -- hot
  , wsRegs   :: !(IM.IntMap Value)  -- hot
  , wsCode   :: !(Array Int Instruction)  -- cold (constant!)
  , wsLabels :: !(Map.Map String Int)     -- cold
  , wsForeignFacts :: !(Map.Map String ...) -- cold
  -- ... 15 more fields ...
  }
```

Every `step` function returns `s { wsPC = newPC }` — which in Haskell's
immutable model *copies all 15 pointers* to construct the new record.
Most of that is unnecessary.

### After

```haskell
-- Immutable, shared across all steps. Built once.
data WamContext = WamContext
  { wcCode         :: !(Array Int Instruction)
  , wcLabels       :: !(Map.Map String Int)
  , wcForeignFacts :: !(Map.Map String ...)
  }

-- Only the mutable, per-step fields live here.
data WamState = WamState
  { wsPC       :: {-# UNPACK #-} !Int
  , wsRegs     :: !(IM.IntMap Value)
  , wsBindings :: !(IM.IntMap Value)
  -- ... just the hot fields ...
  }

step :: WamContext -> WamState -> Instruction -> Maybe WamState
```

Now a step returns a `WamState` record with ~5 fields, not 15. The
`WamContext` is shared via the first argument — one pointer, not
a copy.

**Haskell-specific takeaway:** Record copy cost in Haskell is linear
in the *total* number of fields, not just the ones that changed.
Pass immutable data separately from the things that actually vary.

## Lesson 4: Profile the configuration you actually use

This one burned us hardest. For weeks, we tuned the WAM interpreter's
`step` function based on a profile that showed `step` at 59% of time.

```
COST CENTRE               %time
step                      59.7
run                       10.1
buildFact2Code            6.5   -- noise?
```

Classic hot-loop optimization. We added `INLINE` pragmas, pre-resolved
labels, cached lengths — the usual tricks. Got some gains.

Then we profiled the configuration that actually ran in production:
FFI enabled, kernels handling the hot predicate. Completely different
picture:

```
COST CENTRE               %time
buildFact2Code            42.8   -- what!?
hashWithSalt1             17.2
nativeKernel_ancestor     21.8
step                      2.8    -- not a bottleneck at all
```

With FFI active, `step` was running 20 times less frequently. The
*real* bottleneck was `buildFact2Code` — a function building WAM
instructions for facts that the FFI path never needed. **2GB of
allocation per run, 42% of runtime, for code that was never executed.**

Fixing that (the "skip-facts" optimization) dropped total time from
740ms to 225ms — a bigger single win than all our step-function tuning
combined.

**Haskell-specific takeaway:** Laziness can hide enormous amounts of
work. Code that's allocated but never forced doesn't appear in a naive
profile. Code that's forced but not where you expect (via closure
capture) shows up somewhere weird. Always profile the actual usage.

## Lesson 5: Immutability enables parallelism

This is the big one — the lesson that made Haskell beat Rust.

Our benchmark has 386 independent seed queries. Each seed:
- Reads the immutable `WamContext` (code, facts, labels)
- Creates a fresh `WamState`
- Runs the interpreter to completion
- Returns a `(seed_name, weight_sum)` pair

In Rust, parallelizing this requires `Arc<WamContext>` and careful
lifetime management; each worker thread needs explicit state. It's
doable but non-trivial, which is why the Rust target didn't bother.

In Haskell, *it's one function call*:

### Before

```haskell
seedResults <- mapM querySeed seedCats
```

### After

```haskell
let !seedResults = parMap rdeepseq querySeed seedCats
```

That's the entire change to exploit all available cores. The compiler
already knew that:
- `WamContext` was immutable (it's a pure value)
- `WamState` didn't cross spark boundaries (each `querySeed` is a
  pure computation returning only its result)
- The result type `(String, Double)` has an `NFData` instance

No locks, no `Arc`, no lifetime annotations, no data-race risk. The
type system had already proven that parallelization was safe.

Result: **3.3x speedup on 4 cores.** Sequential Haskell query was 107ms;
parallel Haskell is 32ms. Rust at 126ms (single-threaded) is now
1.75x slower than our parallel Haskell total time.

**Haskell-specific takeaway:** The cost of immutability (a few percent
of allocation) is *massively* outweighed by the parallelization it
enables. A functional language that would have been "20% slower
sequential" ends up "3x faster parallel" — and the parallelization
was free.

## Lesson 6: BangPatterns and UNPACK are not optional

Haskell defaults to lazy evaluation everywhere. For numerical and
data-structural code, that's usually wrong — you want strict fields
and forced evaluation.

```haskell
-- Lazy: every field is a pointer to a thunk that might not evaluate
-- until consumed later. Allocates more, runs cold-cache.
data ChoicePoint = ChoicePoint
  { cpNextPC   :: Int
  , cpTrailLen :: Int
  , cpHeapLen  :: Int
  }

-- Strict + UNPACK: Ints stored directly in the record body. No thunk,
-- no indirection, cache-friendly.
data ChoicePoint = ChoicePoint
  { cpNextPC   :: {-# UNPACK #-} !Int
  , cpTrailLen :: {-# UNPACK #-} !Int
  , cpHeapLen  :: {-# UNPACK #-} !Int
  }
```

Combined with `{-# LANGUAGE BangPatterns #-}` at the module top and
`!` on bindings inside functions, this closes the gap with C/Rust
for numerical work. GHC's `-O2` will happily vectorize and inline
tight loops once laziness is out of the way.

## The big picture

Here's the final profile breakdown for the optimized Haskell target:

```
COST CENTRE                      %time
nativeKernel_category_ancestor   ~35   -- actual useful work
step (WAM interpreter)            2.8   -- essentially free
hashWithSalt1                     9.7   -- residual, mostly startup
loadTsvPairs                      5.3   -- file I/O
buildFact2Code                    0     -- eliminated by skip-facts
```

Notice what's on top: **the kernel doing actual graph traversal.**
That's what we *want* to be the bottleneck. We've pushed overhead
down to "mostly zero" and the remaining time is work that has to
happen.

## Lesson 7: Know when in-memory stops winning

For most of this chapter, the fix was "give Haskell the right data
structure and it beats Rust on a graph-analytics workload." At
somewhere between 1M and 10M edges, that story has a wrinkle worth
knowing about.

### The abstraction that made this lesson possible

When we added an LMDB backend to the WAM target, we resisted the
temptation to fork the kernel code. Instead we declared a type:

```haskell
type EdgeLookup = Int -> [Int]
```

That's it. Give the kernel a function from node-id to parent-list
and it doesn't care where the parents live. Two implementations:

```haskell
-- In-memory: O(log n) IntMap lookup, pure, cache-friendly.
intMapEdgeLookup :: IM.IntMap [Int] -> EdgeLookup
intMapEdgeLookup m = \k -> IM.findWithDefault [] k m

-- On-disk: zero-copy mmap via raw LMDB.  unsafePerformIO bridges
-- the IO boundary because the read transaction is observationally pure.
lmdbRawEdgeLookup :: MDB_txn -> MDB_dbi' -> EdgeLookup
lmdbRawEdgeLookup txn dbi k = unsafePerformIO $ ...
```

Both have the same type. The kernel code (the DFS, the
transitive-closure step, the FFI entry point) is literally identical
across backends — a flag at compile time picks which function gets
passed in.

This is Haskell's quiet advantage for this problem. A language with
strict evaluation everywhere (say, Rust) would force each backend to
be a different concrete type; you'd need traits, dynamic dispatch, or
template specialization to preserve the same API. Here we have a
plain function type and two ways to inhabit it.

### The measured scaling curve

We ran the same DFS workload at three scales, IntMap vs LMDB-raw:

| Scale | Edges | IntMap | LMDB (warm) | Ratio |
|-------|------:|-------:|------------:|------:|
| simplewiki 10k | ~25k | 532 ms | 684 ms | 1.29× IntMap wins |
| 100k_cats | ~197k | 172,000 ms | 181,263 ms | 1.05× IntMap wins |
| **enwiki 10k seeds** | **~10M** | **1,656 ms** | **1,593 ms** | **0.96× LMDB wins** |

At 25k edges (simplewiki), IntMap beats LMDB by 29% — the in-memory
version has no paging, no syscalls, no mmap overhead. At 200k edges
(a full category-only graph), the ratio has narrowed to 5%. At 10M
edges (full English Wikipedia), **LMDB is 4% faster**.

Both sides visit identical nodes at the 10M run (50,163 nodes across
10k random seeds, avg 5 per seed, max depth 3-4) — the comparison is
apples-to-apples. The DFS algorithm is the same, the seeds are the
same, the cycle detection is the same. The storage is the only
variable.

### Why the crossover happens

Two effects accumulate against IntMap as the graph grows:

1. **GC cost.** A 10M-edge `IntMap [Int]` is a sizable structure in
   the GHC heap. Every major GC traverses it. LMDB's pages live in
   the OS page cache — they are invisible to the GHC runtime. Large
   IntMaps pay GC tax; LMDB doesn't.
2. **Startup cost.** Materializing the IntMap in our benchmark took
   51 seconds at 10M edges. LMDB's startup is mmapping a file —
   microseconds. For a long-running process this is amortized, but
   for short-lived queries it matters.

And one effect that makes LMDB punch above its weight at scale:

3. **mmap + warm page cache ≈ in-memory access.** After the first
   read from a page, it lives in the kernel's buffer cache. A
   subsequent `mdb_get'` on that page is a direct memory access
   guarded by a pointer-chase in the B+ tree — not fundamentally
   slower than an IntMap lookup.

### What the `EdgeLookup` abstraction buys you

Beyond the raw measurement: the kernel code doesn't have to know
which backend won. When the next platform appears (SQLite for
Termux, raw mmap for pre-processed artifacts, a remote fact store
over gRPC), you add a function of type `EdgeLookup` and a flag to
select it. The kernel code, the ~6 kernel templates, the purity
analysis — none of it changes.

This is the lesson for library-scale Haskell: the right type lets
you postpone the performance question until you have the data.
`Int -> [Int]` is a small type, but it has opinions about what you
can swap underneath it. IntMap and LMDB-mmap both inhabit that type
cleanly. Thinking that way once and encoding it once paid off
across every kernel the WAM target can emit.

## Summary: the "skill issue" principle

Every optimization in this chapter was a case where the original code
was asking for trouble. None of them were "Haskell is inherently slow"
— they were all "we wrote code that didn't match how Haskell runs."

- Wrong container for the key type → hash storms
- Missing strictness annotations → thunk buildup
- Mixing hot and cold state → unnecessary copying
- Trusting the wrong profile → optimizing the wrong code
- Ignoring the parallelism that immutability makes trivial

Fix those, and Haskell beats Rust on a real graph-analytics workload.
The "functional languages are slow" narrative comes from programs
that make these mistakes, not from any fundamental limitation of the
language.

## Further reading

- `docs/design/WAM_PERF_OPTIMIZATION_LOG.md` in the main project —
  detailed commit-by-commit history
- `docs/design/WAM_HASKELL_FFI_PROFILING_REPORT.md` — the profiling
  matrix that unlocked the Phase-D wins
- `docs/vision/HASKELL_TARGET_PHILOSOPHY.md` — why Haskell as a target
- `docs/vision/HASKELL_TARGET_ROADMAP.md` — next optimizations in the
  pipeline (intra-query parallelism, demand-driven mutable sections)
