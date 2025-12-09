#!/bin/bash
set -e

# Switch to script directory
cd "$(dirname "$0")"

# Project root relative to this script
PROJECT_ROOT="../../../"

# Run Prolog script
echo "Generating SQL..."
OUTPUT=$(swipl -q -s "$PROJECT_ROOT/init.pl" -s sql_intro.pl -g halt)

echo "$OUTPUT"

# Normalize output (replace newlines with spaces) for easier grepping
NORMALIZED_OUTPUT=$(echo "$OUTPUT" | tr '\n' ' ')

# Verify Output
echo "Verifying SQL generation..."

if echo "$NORMALIZED_OUTPUT" | grep -q "SELECT id, name, dept, salary FROM employees;" && \
   echo "$NORMALIZED_OUTPUT" | grep -q "SELECT name, salary FROM employees WHERE dept = 'Engineering';" && \
   echo "$NORMALIZED_OUTPUT" | grep -q "CREATE VIEW IF NOT EXISTS top_earners"; then
    echo "✅ Test Passed: SQL generated correctly"
else
    echo "❌ Test Failed: Output mismatch"
    exit 1
fi
