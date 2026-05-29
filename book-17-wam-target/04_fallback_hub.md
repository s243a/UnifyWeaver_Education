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

## Benefits

- **Semantic stability**: WAM gives every target a common account of
  unification, choice points, calls, and backtracking.
- **Target freedom**: each target can choose the concrete runtime shape that
  fits its host language.
- **Performance options**: deterministic lowering, indexed fact sources, FFI,
  and kernels can accelerate specific workloads without weakening parity.
