# Go Target Example: Family Tree

This example demonstrates how to compile Prolog predicates to a standalone Go program using the **Generator Mode** (Semi-Naive Evaluation).

## Files

- `family_tree.pl`: Prolog source file containing `parent/2` facts and `ancestor/2` rules.
- `compile.pl`: Prolog script to compile the predicates.
- `ancestor.go`: Generated Go source code.

## How to Run

1. **Compile the Prolog code:**
   ```bash
   swipl -s ../../../init.pl -s compile.pl -g halt
   ```

2. **Build and Run the Go program:**
   ```bash
   go mod init example/ancestor  # Initialize module (once)
   go build ancestor.go
   ./ancestor
   ```

   *Note: The generator mode outputs all derived facts to stdout as JSON lines.*

## Expected Output

You should see a stream of JSON objects representing both parent and ancestor facts, including transitively derived ancestors (e.g., `ancestor(abraham, jacob)`).
