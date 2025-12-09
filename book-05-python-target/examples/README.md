# Python Target Example: Family Tree

This example demonstrates how to compile Prolog predicates to Python using the **Generator Mode** (Semi-Naive Evaluation).

## Files

- `family_tree.pl`: Prolog source file containing `parent/2` facts and `ancestor/2` rules.
- `compile.pl`: Prolog script to compile the predicates.
- `parent.py`: Generated Python script for `parent/2`.
- `ancestor.py`: Generated Python script for `ancestor/2`.

## How to Run

1. **Compile the Prolog code:**
   ```bash
   swipl -s ../../../init.pl -s compile.pl -g halt
   ```

2. **Run the pipeline:**
   ```bash
   # parent.py generates facts, ancestor.py infers relationships
   echo "" | python3 parent.py | python3 ancestor.py
   ```

## Expected Output

You should see a stream of JSON objects representing both parent and ancestor facts.
