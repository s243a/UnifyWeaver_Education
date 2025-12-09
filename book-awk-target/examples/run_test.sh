#!/bin/bash
set -e

# Switch to script directory
cd "$(dirname "$0")"

# Project root relative to this script
PROJECT_ROOT="../../../"

# 1. Compile Prolog to AWK
echo "Compiling Prolog to AWK..."
swipl -q -s "$PROJECT_ROOT/init.pl" -s compile.pl -g halt

# 2. Create Test Data
echo "Creating test data..."
cat <<EOF > sales.txt
Laptop	Electronics	1200
Chair	Furniture	150
Mouse	Electronics	25
Desk	Furniture	300
Phone	Electronics	800
EOF

# 3. Run Pipeline: Filter -> Cut Amount -> Sum
echo "Running Pipeline..."
# Filter: Selects Laptop (1200) and Phone (800). Skips Mouse (25) and others.
./filter_sales.awk sales.txt > filtered.txt

echo "Filtered output:"
cat filtered.txt

# Extract just the amount (column 3)
cut -f3 filtered.txt > amounts.txt

# Sum
TOTAL=$(./sum_sales.awk amounts.txt)
echo "Total High Value Electronics: $TOTAL"

# 4. Verify
if [[ "$TOTAL" == "2000" ]]; then
    echo "✅ Test Passed: Total is 2000 (1200 + 800)"
else
    echo "❌ Test Failed: Expected 2000, got $TOTAL"
    exit 1
fi
