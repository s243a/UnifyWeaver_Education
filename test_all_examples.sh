#!/bin/bash
# test_all_examples.sh - Validate all code examples from education chapters
# Tests examples from Chapters 4, 9, 10, and 11

set -e  # Exit on error

echo "╔════════════════════════════════════════════════════════════╗"
echo "║  EDUCATION MATERIALS - EXAMPLE VALIDATION                  ║"
echo "╚════════════════════════════════════════════════════════════╝"
echo ""

# Track test results
TOTAL_TESTS=0
PASSED_TESTS=0
FAILED_TESTS=0

# Helper function to run a test
run_test() {
    local test_name="$1"
    local test_command="$2"

    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo -n "  Testing: $test_name ... "

    if eval "$test_command" > /dev/null 2>&1; then
        echo "✓ PASS"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        echo "✗ FAIL"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

# Helper function to run a test with output check
run_test_with_output() {
    local test_name="$1"
    local test_command="$2"
    local expected_pattern="$3"

    TOTAL_TESTS=$((TOTAL_TESTS + 1))
    echo -n "  Testing: $test_name ... "

    OUTPUT=$(eval "$test_command" 2>&1)
    if echo "$OUTPUT" | grep -q "$expected_pattern"; then
        echo "✓ PASS"
        PASSED_TESTS=$((PASSED_TESTS + 1))
        return 0
    else
        echo "✗ FAIL (expected output containing: $expected_pattern)"
        echo "    Got: $OUTPUT"
        FAILED_TESTS=$((FAILED_TESTS + 1))
        return 1
    fi
}

echo "════════════════════════════════════════════════════════════"
echo "Chapter 4: Your First Program"
echo "════════════════════════════════════════════════════════════"
echo ""

# Check if init.pl exists
if [[ -f "init.pl" ]]; then
    echo "✓ Found init.pl"
else
    echo "✗ Missing init.pl"
    FAILED_TESTS=$((FAILED_TESTS + 1))
fi

# Check if family_tree.pl exists
if [[ -f "family_tree.pl" ]]; then
    echo "✓ Found family_tree.pl"
else
    echo "✗ Missing family_tree.pl"
    FAILED_TESTS=$((FAILED_TESTS + 1))
fi

# Check if family_tree.sh exists (generated script)
if [[ -f "family_tree.sh" ]]; then
    echo "✓ Found family_tree.sh (generated script)"

    # Test if it's sourceable
    run_test "Source family_tree.sh" "source family_tree.sh"

    # Test ancestor function exists
    run_test "ancestor function exists" "type ancestor &>/dev/null"

    # Note: ancestor requires parent data, which should be in family_tree.pl
    # Skip runtime test for now - just verify the script is syntactically valid
    echo "  ⚠ Skipping runtime test (requires parent data source)"
else
    echo "✗ Missing family_tree.sh"
    FAILED_TESTS=$((FAILED_TESTS + 1))
fi

echo ""

echo "════════════════════════════════════════════════════════════"
echo "Chapter 9: Advanced Recursion Patterns"
echo "════════════════════════════════════════════════════════════"
echo ""

# Check output/advanced directory
if [[ ! -d "../output/advanced" ]]; then
    echo "⚠ Warning: ../output/advanced directory not found"
    echo "  Creating directory for testing..."
    mkdir -p ../output/advanced
fi

# Test if advanced recursion scripts exist
ADVANCED_SCRIPTS=(
    "../output/advanced/count_items.sh"
    "../output/advanced/factorial.sh"
    "../output/advanced/list_length.sh"
    "../output/advanced/tree_sum.sh"
    "../output/advanced/even_odd.sh"
)

echo "Checking for generated advanced recursion scripts:"
for script in "${ADVANCED_SCRIPTS[@]}"; do
    if [[ -f "$script" ]]; then
        echo "  ✓ Found $(basename $script)"
    else
        echo "  ⚠ Missing $(basename $script) - may need to regenerate"
    fi
done

echo ""

# Test count_items.sh (tail recursion)
if [[ -f "../output/advanced/count_items.sh" ]]; then
    echo "Testing count_items.sh (Tail Recursion):"
    source ../output/advanced/count_items.sh 2>/dev/null

    run_test_with_output "count_items empty list" \
        "count_items '[]' 0 ''" \
        "0"

    run_test_with_output "count_items [a,b,c]" \
        "count_items '[a,b,c]' 0 ''" \
        "3"

    echo ""
fi

# Test factorial.sh (linear recursion with fold)
if [[ -f "../output/advanced/factorial.sh" ]]; then
    echo "Testing factorial.sh (Linear Recursion with Fold):"
    source ../output/advanced/factorial.sh 2>/dev/null

    run_test_with_output "factorial 0" \
        "factorial 0 ''" \
        "0:1"

    run_test_with_output "factorial 5" \
        "factorial 5 ''" \
        "5:120"

    echo ""
fi

# Test list_length.sh (linear recursion with fold)
if [[ -f "../output/advanced/list_length.sh" ]]; then
    echo "Testing list_length.sh (Linear Recursion with Fold):"
    source ../output/advanced/list_length.sh 2>/dev/null

    run_test_with_output "list_length []" \
        "list_length '[]' ''" \
        "0"

    run_test_with_output "list_length [a,b,c]" \
        "list_length '[a,b,c]' ''" \
        "3"

    echo ""
fi

# Test tree_sum.sh (tree recursion)
if [[ -f "../output/advanced/tree_sum.sh" ]]; then
    echo "Testing tree_sum.sh (Tree Recursion):"
    source ../output/advanced/tree_sum.sh 2>/dev/null

    run_test_with_output "tree_sum empty tree" \
        "tree_sum '[]'" \
        "0"

    run_test_with_output "tree_sum [5,[3,[1,[],[]]],[2,[],[]]]" \
        "tree_sum '[5,[3,[1,[],[]]],[2,[],[]]]'" \
        "11"

    echo ""
fi

# Test even_odd.sh (mutual recursion)
if [[ -f "../output/advanced/even_odd.sh" ]]; then
    echo "Testing even_odd.sh (Mutual Recursion):"
    source ../output/advanced/even_odd.sh 2>/dev/null

    run_test "is_even 0 returns true" \
        "is_even 0 && echo 'true'"

    run_test "is_even 2 returns true" \
        "is_even 2 && echo 'true'"

    run_test "is_odd 3 returns true" \
        "is_odd 3 && echo 'true'"

    run_test "is_even 5 returns false" \
        "! is_even 5"

    echo ""
fi

echo "════════════════════════════════════════════════════════════"
echo "Chapter 10: Prolog Introspection and Theory"
echo "════════════════════════════════════════════════════════════"
echo ""

echo "Testing Prolog introspection examples:"

# Test if we can load UnifyWeaver modules
run_test "Load call_graph module" \
    "swipl -g 'use_module(\"../src/unifyweaver/core/advanced/call_graph\"), halt.' 2>&1"

run_test "Load scc_detection module" \
    "swipl -g 'use_module(\"../src/unifyweaver/core/advanced/scc_detection\"), halt.' 2>&1"

run_test "Load pattern_matchers module" \
    "swipl -g 'use_module(\"../src/unifyweaver/core/advanced/pattern_matchers\"), halt.' 2>&1"

# Test call graph construction
run_test "Build call graph" \
    "swipl -g 'use_module(\"../src/unifyweaver/core/advanced/call_graph\"), assertz(user:ancestor(X,Y) :- parent(X,Y)), assertz(user:(ancestor(X,Z) :- parent(X,Y), ancestor(Y,Z))), is_self_recursive(ancestor/2), halt.' 2>&1"

# Test SCC detection
run_test "Detect SCCs" \
    "swipl -g 'use_module(\"../src/unifyweaver/core/advanced/scc_detection\"), find_sccs([a->b, b->a], SCCs), halt.' 2>&1"

echo ""

echo "════════════════════════════════════════════════════════════"
echo "Chapter 11: Test Runner Inference"
echo "════════════════════════════════════════════════════════════"
echo ""

echo "Testing test runner inference:"

# Test if test_runner_inference module loads
run_test "Load test_runner_inference module" \
    "swipl -g 'use_module(\"../src/unifyweaver/core/advanced/test_runner_inference\"), halt.' 2>&1"

# Check if inferred test runner exists
if [[ -f "../output/advanced/inferred_test_runner.sh" ]]; then
    echo "  ✓ Found inferred_test_runner.sh"

    # Test if it's executable
    run_test "Inferred test runner is sourceable" \
        "bash -n ../output/advanced/inferred_test_runner.sh"

    echo ""
    echo "  Note: To regenerate test runner, run:"
    echo "    swipl -g \"use_module(unifyweaver(core/advanced/test_runner_inference)), generate_test_runner_inferred, halt.\""
else
    echo "  ⚠ Missing inferred_test_runner.sh - may need to generate"
fi

echo ""

echo "════════════════════════════════════════════════════════════"
echo "Additional Validation"
echo "════════════════════════════════════════════════════════════"
echo ""

# Check if all markdown files are valid
echo "Checking markdown files:"
for md_file in *.md; do
    if [[ -f "$md_file" ]]; then
        # Basic check: file is readable and non-empty
        if [[ -s "$md_file" ]]; then
            echo "  ✓ $md_file ($(wc -l < "$md_file") lines)"
        else
            echo "  ✗ $md_file is empty"
            FAILED_TESTS=$((FAILED_TESTS + 1))
        fi
    fi
done

echo ""

echo "════════════════════════════════════════════════════════════"
echo "Test Summary"
echo "════════════════════════════════════════════════════════════"
echo ""
echo "Total Tests: $TOTAL_TESTS"
echo "Passed:      $PASSED_TESTS"
echo "Failed:      $FAILED_TESTS"
echo ""

if [[ $FAILED_TESTS -eq 0 ]]; then
    echo "✓ ALL TESTS PASSED"
    exit 0
else
    echo "✗ SOME TESTS FAILED"
    echo ""
    echo "Common fixes:"
    echo "  1. Regenerate advanced scripts:"
    echo "     cd .. && swipl -g 'use_module(\"test/test_advanced\"), test_advanced, halt.'"
    echo "  2. Regenerate test runner:"
    echo "     swipl -g 'use_module(unifyweaver(core/advanced/test_runner_inference)), generate_test_runner_inferred, halt.'"
    echo "  3. Check that UnifyWeaver is properly installed"
    exit 1
fi
