# Chapter 4: WAM as a Hybrid Routing Layer

The WAM target occupies a unique position in the UnifyWeaver ecosystem. It is
not only a last-resort fallback; it is also the shared semantic layer that lets
targets choose between interpretation, lowered host-language code, foreign
calls, indexed fact access, and specialized kernels.

## The Routing Pattern

When a Prolog predicate is too complex for native lowering, UnifyWeaver can
route it through WAM-shaped execution. When a predicate has a profitable shape,
a hybrid target can lower the relevant WAM items directly into host-language
structures or helper functions.

```text
Prolog source
      |
      v
WAM items / target-ready WAM data
      |
      +--> target runtime interpretation
      +--> lowered host-language helper
      +--> foreign call or kernel dispatch
      +--> optional symbolic WAM text for debug/interchange
```

## Compilation Pipeline

1. **Prolog to WAM items**: structured items preserve logic semantics without
   requiring a text round trip.
2. **Target routing**: the emitter chooses interpretation, lowering, FFI,
   indexing, or kernel dispatch based on target capabilities and predicate
   shape.
3. **Execution**: the resulting host-language code or runtime data preserves
   Prolog behavior while using target-specific strengths.

## Symbolic Text Path

Symbolic WAM text is still important, but it should be understood as a readable
view of the item stream. It is useful for teaching, debugging, golden tests,
and interchange with external tools. Hybrid targets should normally skip the
print-and-parse path when they can consume structured items directly.

## Foreign Calls, Fact Sources, And Kernels

Hybrid WAM becomes interesting when the target does not simply interpret every
instruction. Three common escape hatches are foreign calls, external fact
sources, and kernels.

### Foreign calls

A foreign call is a controlled boundary from WAM execution into host code. The
WAM side prepares arguments in registers, the host function computes or looks
up results, and the WAM side unifies those results back into variables.

```text
WAM registers -> host function -> returned terms -> WAM unification
```

For example, a semantic distance predicate might look declarative in Prolog:

```prolog
near(A, B) :- semantic_distance(A, B, D), D < 0.3.
```

A hybrid target can route `semantic_distance/3` to a host function rather than
interpreting the distance calculation in WAM. The predicate still behaves like
a logic predicate because success, failure, and output bindings return through
the WAM unifier.

### External fact sources

A fact predicate does not have to be stored as generated source code. The
target can back it with an external source: an in-memory index, a TSV scanner,
LMDB, a database cursor, or another target-specific store. The WAM call asks
for facts matching the current bound arguments. The source returns candidates;
the runtime unifies candidates with open variables and creates choice points
when more answers remain.

| Query mode | Runtime request | Fact-source behavior |
|---|---|---|
| `edge(a, B)` | arg1 bound | indexed lookup for outgoing edges from `a` |
| `edge(A, b)` | arg2 bound | reverse index or scan depending on target support |
| `edge(A, B)` | both open | stream all facts with choice points |

### Kernels

A kernel is a larger foreign lowering. Instead of replacing one builtin, it
recognizes a recurring relational shape such as transitive closure, shortest
path, or aggregate reachability. The target emits setup data and a host-native
algorithm, then exposes it through the same WAM call/unify boundary.

The important rule is that kernels are accelerators, not alternate semantics.
If a kernel handles `reachable/2`, it must return the same answers that the
ordinary WAM-shaped program would have produced, including failure behavior and
answer multiplicity where relevant.

## Benefits

- **Semantic stability**: WAM gives every target a common account of
  unification, choice points, calls, and backtracking.
- **Target freedom**: each target can choose the concrete runtime shape that
  fits its host language.
- **Performance options**: deterministic lowering, indexed fact sources, FFI,
  and kernels can accelerate specific workloads without weakening parity.
