# Examples

This directory contains example programs for the Java target.

## Available Examples

| File | Description | Status |
|------|-------------|--------|
| family_tree.pl | Family tree facts and rules | ✅ Source |
| FilterPipeline.java | JSONL filtering example | ✅ Working |

## Current Capabilities

The Java target currently supports **JSONL pipeline processing**:

```bash
# Process JSONL records
echo '{"name": "alice", "value": 75}' | java FilterPipeline
```

## Not Yet Implemented

The following features from the Bash target are **not yet available** for Java:

- `compile_recursive/3` for transitive closure (ancestor)
- Direct fact querying (Bash: `parent abraham`)
- Test runner generation

These would require implementing a Java-based query runtime similar to the C# query target.

## Workaround

For recursive queries, consider:
1. Using the C# query target (has runtime query engine)
2. Using the Bash target and calling from Java via ProcessBuilder
3. Implementing custom recursive logic in Java

## See Also

- [family_tree.pl](../../book-02-bash-target/examples/family_tree.pl) - Original Prolog source
- [JVM_TARGET.md](../../../../docs/JVM_TARGET.md) - JVM target documentation
