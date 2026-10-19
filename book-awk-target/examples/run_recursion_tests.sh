#!/usr/bin/env bash
# SPDX-License-Identifier: MIT OR Apache-2.0
# Copyright (c) 2026 John William Creighton (s243a)
# Copyright (c) 2026 UnifyWeaver contributors
# Run from the UnifyWeaver compiler root:
# bash "$EDUCATION_ROOT/book-awk-target/examples/run_recursion_tests.sh"
set -euo pipefail

examples_dir="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
output_dir="$(mktemp -d)"
trap 'rm -rf -- "$output_dir"' EXIT

swipl -q -s init.pl -s "$examples_dir/factorial.pl" -- "$output_dir"
swipl -q -s init.pl -s "$examples_dir/graph_recursion.pl" -- "$output_dir"

expect_output() {
    local expected="$1"
    shift
    local actual
    actual="$("$@")"
    if [[ "$actual" != "$expected" ]]; then
        printf 'Unexpected output from %s\nExpected:\n%s\nActual:\n%s\n' "$*" "$expected" "$actual" >&2
        exit 1
    fi
}

printf '5\t1\t0\n0\t1\t0\n6\t1\t0\n' > "$output_dir/factorial.tsv"
expect_output $'120\n1\n720' awk -f "$output_dir/factorial.awk" "$output_dir/factorial.tsv"
expect_output '120' awk 'function f(n){return n<2?1:n*f(n-1)} BEGIN{print f(5)}'

awk -f "$output_dir/path.awk" -f "$examples_dir/graph_query.awk" -v start=a /dev/null | LC_ALL=C sort > "$output_dir/reachable.txt"
expect_output $'a:b\na:c\na:d' cat "$output_dir/reachable.txt"
expect_output 'a:d' awk -f "$output_dir/path.awk" -f "$examples_dir/graph_query.awk" -v start=a -v target=d /dev/null

expect_false() {
    local status=0
    awk -f "$output_dir/path.awk" -f "$examples_dir/graph_query.awk" -v "start=$1" -v "target=$2" /dev/null > "$output_dir/false.txt" || status=$?
    [[ "$status" == 1 && ! -s "$output_dir/false.txt" ]]
}
expect_false a missing
expect_false b b

awk -f "$output_dir/path.awk" -f "$examples_dir/graph_query.awk" -v start=b /dev/null > "$output_dir/cycle.txt"
expect_output 'b:d' cat "$output_dir/cycle.txt"

awk -f "$output_dir/path_hops.awk" /dev/null | LC_ALL=C sort > "$output_dir/hops.tsv"
expect_output $'a\tb\t1\na\tb\t3\na\tc\t1\na\td\t2\na\td\t2\nb\td\t1\nc\tb\t2\nc\td\t1\nd\tb\t1' cat "$output_dir/hops.tsv"
printf 'PASS: factorial; reachable vertices; true/false checks; cycle/source exclusion; simple-path rows and duplicates.\n'
