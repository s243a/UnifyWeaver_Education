#!/bin/bash
set -e

# Switch to script directory
cd "$(dirname "$0")"

# Project root relative to this script
PROJECT_ROOT="../../../"

# Compile
echo "Compiling..."
swipl -q -s "$PROJECT_ROOT/init.pl" -s compile.pl -g halt

# Run
echo "Running Pipeline..."
# Use grep to verify output contains specific ancestors
OUTPUT=$(echo "" | python3 parent.py | python3 ancestor.py)

# Check for a transitive relationship: abraham -> jacob
if echo "$OUTPUT" | grep -q '"arg0": "abraham", "arg1": "jacob", "relation": "ancestor"'; then
    echo "✅ Test Passed: Found ancestor(abraham, jacob)"
else
    echo "❌ Test Failed: ancestor(abraham, jacob) not found"
    echo "Output sample:"
    echo "$OUTPUT" | head -n 5
    exit 1
fi
