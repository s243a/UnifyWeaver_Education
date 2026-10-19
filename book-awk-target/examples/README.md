<!-- SPDX-License-Identifier: MIT AND CC-BY-4.0 -->

# AWK Target Examples

## Recursion examples

Run these from a separate **UnifyWeaver compiler checkout root**, with the
Education repository path supplied through the shell environment. Use a full
compiler checkout, including `templates/` for shared graph compilation:

```bash
export EDUCATION_ROOT=/absolute/path/to/UnifyWeaver_Education
bash scripts/setup_local.sh
mkdir -p output/advanced
bash "$EDUCATION_ROOT/book-awk-target/examples/run_recursion_tests.sh"
```

The runner compiles and executes the examples in a temporary directory and
checks their output. To keep the generated scripts for inspection:

```bash
swipl -q -s init.pl -s "$EDUCATION_ROOT/book-awk-target/examples/factorial.pl" -- output/awk-book
printf '5\t1\t0\n' | awk -f output/awk-book/factorial.awk
# 120

swipl -q -s init.pl -s "$EDUCATION_ROOT/book-awk-target/examples/graph_recursion.pl" -- output/awk-book
awk -f output/awk-book/path.awk \
    -f "$EDUCATION_ROOT/book-awk-target/examples/graph_query.awk" \
    -v start=a /dev/null | LC_ALL=C sort
# a:b
# a:c
# a:d

awk -f output/awk-book/path_hops.awk /dev/null | LC_ALL=C sort
```

The `.pl` recipes accept an optional output-directory argument, defaulting to
`output/awk-book`. Use a directory without whitespace because the current
compiler writer passes its filename to `chmod` without shell quoting.
`factorial.pl` uses the direct AWK API. `graph_recursion.pl` uses the shared
recursive compiler for binary reachability and the direct AWK API for ternary
simple-path hop rows. `graph_query.awk` calls the generated embedded library's
`check` and `find_all` functions; embedded mode itself has no CLI.

See [VALIDATION.md](VALIDATION.md) for the tested compiler revision, exact
results, cycle semantics, and expected failure cases. The ternary output reports
source, target, and hop count per simple path; distinct paths can produce
identical rows.

## Sales report

This example demonstrates how to compile Prolog predicates to AWK scripts for efficient text processing pipelines.

## Files

- `sales_filter.pl`: Prolog source defining filtering logic (`Category = 'Electronics', Amount > 500`).
- `compile.pl`: Prolog script to compile the filter and aggregation logic.
- `filter_sales.awk`: Generated AWK script to filter rows.
- `sum_sales.awk`: Generated AWK script to sum values.

## How to Run

1. **Compile the Prolog code:**
   ```bash
   swipl -s ../../../init.pl -s compile.pl -g halt
   ```

2. **Run the pipeline:**
   ```bash
   # Create dummy data
   echo -e "Laptop\tElectronics\t1200" > sales.txt
   echo -e "Chair\tFurniture\t150" >> sales.txt
   
   # Run filter -> cut -> sum
   ./filter_sales.awk sales.txt | cut -f3 | ./sum_sales.awk
   ```

## Expected Output

For the standard test case, the total should be **2000**.

## License

Documentation in this directory is dual-licensed under
[MIT](../../LICENSE-MIT) and [CC-BY-4.0](../../LICENSE-CC-BY-4.0)
(`MIT AND CC-BY-4.0`). New executable recursion examples carry
`MIT OR Apache-2.0` SPDX headers as required by
[CONTRIBUTING.md](../../CONTRIBUTING.md).
