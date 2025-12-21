# Go Target Examples

This directory contains runnable examples organized by chapter.

## Directory Structure

```
examples/
├── chapter-2-basic/      # Basic compilation (facts, rules)
├── chapter-4-json/       # JSON processing examples
├── chapter-6-generator/  # Generator mode with fixpoint
└── chapter-7-recursion/  # Recursive query examples
    ├── family_tree.pl    # Source predicates
    ├── ancestor.go       # Generated Go code
    └── run_test.sh       # Test script
```

## Quick Start

### Chapter 7: Recursive Queries (Family Tree)

```bash
cd chapter-7-recursion

# Build the Go program
go build ancestor.go

# Run with test data
./ancestor abraham
```

## Requirements

- Go 1.16+ installed
- SWI-Prolog (for recompiling from source)

## Adding New Examples

Each subdirectory should contain:
1. Source `.pl` file with predicates
2. `compile.pl` script to generate Go code
3. Generated `.go` file
4. `run_test.sh` to verify functionality
