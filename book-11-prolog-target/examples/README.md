# Prolog Target Example: Factorial

This example demonstrates the "transpilation" capabilities of the Prolog target, converting standard Prolog code into a self-contained, executable script (or binary).

## Files

- `factorial.pl`: Source Prolog logic.
- `compile.pl`: Script to generate the standalone artifact.
- `factorial_script.pl`: The generated artifact (created after running compile).

## How to Run

1. **Compile:**
   ```bash
   swipl -s ../../../init.pl -s compile.pl -g halt
   ```

2. **Run:**
   ```bash
   # Run the generated script directly (interprets with swipl)
   swipl -s factorial_script.pl -g "run_test, halt" -t halt
   ```

## Expected Output

```
Factorial of 5 is 120
```
