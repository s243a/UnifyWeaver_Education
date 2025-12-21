# Chapter 7 Example: Family Tree (Recursive Queries)

This example demonstrates BFS-based transitive closure using `compile_recursive/3`.

## Files

| File | Description |
|------|-------------|
| `family_tree.pl` | Prolog source with `parent/2` facts and `ancestor/2` rules |
| `compile.pl` | Script to compile predicates to Go |
| `ancestor.go` | Generated Go code (BFS implementation) |
| `ancestor` | Compiled binary |
| `run_test.sh` | Test script |

## How to Run

### Using the Pre-built Binary

```bash
# Find all ancestors of abraham
echo "abraham:isaac
isaac:jacob
jacob:joseph" | ./ancestor abraham

# Output:
# abraham:isaac
# abraham:jacob
# abraham:joseph
```

### Recompiling from Source

```bash
# Generate Go code
swipl -s ../../../../init.pl -s compile.pl -g halt

# Build binary
go build ancestor.go

# Run
./ancestor abraham
```

## How It Works

The `compile_recursive/3` predicate generates BFS-based Go code:

1. **Adjacency list**: `map[string][]string`
2. **BFS queue**: `[]string` slice
3. **Visited set**: `map[string]bool`

This achieves O(V + E) complexity for transitive closure.

## See Also

- [Chapter 7: Recursive Queries](../../07_recursive_queries.md)
- [Appendix B: Complexity Guide](../../A2_complexity_guide.md)
