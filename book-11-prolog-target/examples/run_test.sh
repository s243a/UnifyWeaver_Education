#!/bin/bash
set -e

# Switch to script directory
cd "$(dirname "$0")"

# Project root relative to this script
PROJECT_ROOT="../../../"

# 1. Compile Prolog to Prolog (Transpile)
echo "Transpiling Prolog..."
swipl -q -s "$PROJECT_ROOT/init.pl" -s compile.pl -g halt

# 2. Run the generated script
echo "Running generated script..."
# The generated script typically needs to be loaded. 
# It might not have an initialization directive unless we added one or the target adds it.
# Let's try running the 'run_test' predicate.
OUTPUT=$(swipl -q -s factorial_script.pl -g "run_test, halt" -t halt)

echo "$OUTPUT"

# 3. Verify
if [[ "$OUTPUT" == *"Factorial of 5 is 120"* ]]; then
    echo "✅ Test Passed: Factorial calculated correctly"
else
    echo "❌ Test Failed: Output mismatch"
    exit 1
fi
