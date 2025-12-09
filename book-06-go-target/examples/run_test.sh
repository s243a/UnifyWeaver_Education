#!/bin/bash
set -e

# Switch to script directory
cd "$(dirname "$0")"

# Project root relative to this script
PROJECT_ROOT="../../../"

# Compile Prolog to Go
echo "Compiling Prolog to Go..."
swipl -q -s "$PROJECT_ROOT/init.pl" -s compile.pl -g halt

# Build Go program
echo "Building Go program..."
# Go requires a module usually, but for a single file main package it might work.
# If not, we'll need to go mod init.
if [ ! -f "go.mod" ]; then
    go mod init example/ancestor
fi
go build ancestor.go

# Run
echo "Running Go program..."
# Go generator mode usually reads from stdin, but since we have facts compiled in, 
# it might just run and output results if configured that way.
# Or it might expect input triggering.
# Based on Python generator, it might expect input records to trigger processing.
# Let's try sending an empty JSON object or empty line.
echo "" | ./ancestor > output.txt

# Verify Output
echo "Verifying Output..."
if grep -F "abraham" output.txt | grep -F "jacob" | grep -F "ancestor" > /dev/null; then
    echo "✅ Test Passed: Found ancestor(abraham, jacob)"
else
    echo "❌ Test Failed: ancestor(abraham, jacob) not found"
    echo "Output sample:"
    head -n 5 output.txt
    exit 1
fi
