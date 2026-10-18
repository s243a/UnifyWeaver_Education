<!-- SPDX-License-Identifier: MIT AND CC-BY-4.0 -->

# AWK recursion validation snapshot

Validated on 2026-10-18 against UnifyWeaver compiler commit
`a56714b46d5758e6de93b1e329e36b4bb623a0fe` (main), with the Education
checkout initially at `03ca889545fee741183b75207b52981cd8f2193b`.
Compiler source files were unchanged; generated scripts were kept in temporary
directories or ignored `output/awk-book`. This is a focused behavior snapshot,
not a benchmark or a guarantee about other compiler revisions.

Environment: Ubuntu under WSL, SWI-Prolog 9.2.9 (x86_64-linux), GNU Bash
5.1.16, GNU Awk 5.1.0. Commands run from the compiler root, after
`bash scripts/setup_local.sh` and `mkdir -p output/advanced`.

## Executed recipes

```bash
export EDUCATION_ROOT=/absolute/path/to/UnifyWeaver_Education
bash "$EDUCATION_ROOT/book-awk-target/examples/run_recursion_tests.sh"
```

Result: exit 0, ending with:

```text
PASS: factorial; reachable vertices; true/false checks; cycle/source exclusion; simple-path rows and duplicates.
```

The runner invokes both `.pl` recipes through `swipl -q -s init.pl -s FILE --
OUTPUT_DIR`. The environment variable locates the Education sources; the output
directory is passed to the Prolog recipe. See [README.md](README.md) for the
individual commands that retain generated artifacts.

| Case | Observed result |
| --- | --- |
| Direct `compile_predicate_to_awk(factorial/3, [], Code)` | Generated a `while` loop; TSV rows `5 TAB 1 TAB 0`, `0 TAB 1 TAB 0`, `6 TAB 1 TAB 0` yielded `120`, `1`, `720` |
| Shared `compile_recursive(path/2, [target(awk), input(embedded)], Code)` | Classified as `transitive_closure(edge)`; generated adjacency arrays, BFS `check`/`find_all`, and embedded seed facts |
| Embedded library alone, `-v start=a /dev/null` | No stdout: embedded mode intentionally emits no query interface |
| Embedded library plus `graph_query.awk`, `start=a` | Sorted lines `a:b`, `a:c`, `a:d`, exit 0 |
| Same caller, `start=a,target=d` | `a:d`, exit 0 |
| Same caller, `start=a,target=missing` | No stdout, exit 1 |
| Same caller, `start=b` | `b:d`, exit 0 despite the `b-d-b` cycle |
| Same caller, `start=b,target=b` | No stdout, exit 1; source is excluded even on a nonempty cycle |
| Direct `compile_predicate_to_awk(path_hops/3, [], Code)` | Explicit-stack DFS, per-path visited state; nine rows below, exit 0 |

For the fixture `a->b`, `a->c`, `b->d`, `c->d`, `d->b`, the sorted
ternary output is TSV:

```text
a	b	1
a	b	3
a	c	1
a	d	2
a	d	2
b	d	1
c	b	2
c	d	1
d	b	1
```

There are nine rows, eight distinct triples, and seven distinct ordered
source/target pairs. Two simple paths from `a` to `d` have the same hop count;
the generated traversal prints that triple twice. The two paths from `a` to
`b` have different hop counts. No row repeats a vertex within its path.
This computes simple-path hop rows from every source with outgoing edges,
whereas `find_all(a, results)` computes unique reachable vertices from one
source. The generated code does not print the full vertex sequence.

The original unguarded Prolog `path_hops/3` relation can recurse indefinitely
on this cyclic graph; the generated simple-path guard is backend behavior,
not a claim of equivalent unrestricted Prolog enumeration.

GNU Awk itself supports recursive function calls. The runner also executed:

```bash
awk 'function f(n){return n<2?1:n*f(n-1)} BEGIN{print f(5)}'
# 120
```

## Expected failures and route differences

These commands are diagnostic probes, not working compilation recipes.
They were executed with the same environment and compiler revision. Run the
graph recipe first to create `output/awk-book`.

Shared factorial probe:

```bash
swipl -q -s init.pl -g "consult('$EDUCATION_ROOT/book-awk-target/examples/factorial.pl'), use_module(unifyweaver(core/recursive_compiler)), (recursive_compiler:compile_recursive(factorial/3,[target(awk)],_)->halt(0);halt(1))"
```

Observed classification `tail_recursion`, then
`Error: No tail recursion support for target awk`; exit 1. The shared
multifile `compile_tail_pattern/9` has no matching AWK implementation for this
case. The direct `awk_target` factorial recipe succeeds, so the limitation
belongs to this dispatch route rather than AWK arithmetic in general.

Direct binary closure probe:

```bash
swipl -q -s init.pl -g "consult('$EDUCATION_ROOT/book-awk-target/examples/graph_recursion.pl'), awk_target:compile_predicate_to_awk(path/2,[],Code), awk_target:write_awk_script(Code,'output/awk-book/direct_path.awk'), halt"
awk -f output/awk-book/direct_path.awk /dev/null
```

Compilation returned code, but the first native clause-lowering pass intercepted
`path/2` before the direct general-recursion recognizer. The generated function
contained `# unsupported output goal` and a misplaced `else return`;
GNU Awk rejected it with a syntax error at `else`, exit 1. Successful code
generation alone therefore does not establish a runnable graph recipe.

For comparison, a separate source-inspection probe explicitly called the
**private** helper `awk_target:compile_general_recursive_to_awk/6` with the
collected binary `path/2` clauses. Its generated pair-fixpoint code ran and
produced nine pairs, including `b TAB b` and `d TAB d`. This confirms that
relation-wide fixpoint closure can retain self-pairs caused by cycles, unlike
the shared BFS functions, which pre-mark the source visited and exclude it.
That private helper bypasses public dispatch and is not a supported recipe.

## Source and additional checks

The checked implementation points are:

- `src/unifyweaver/targets/awk_target.pl`: public native-first dispatch,
  tail-recursion recognizer and loop emitter, general recursion binary fixpoint
  and ternary DFS, and `write_awk_script(Code, FilePath)` argument order.
- `src/unifyweaver/core/recursive_compiler.pl`: pattern classification and
  composable AWK transitive-closure dispatch. Embedded/function modes omit the
  CLI interface; stdin/file modes include it.
- `templates/targets/awk/tc_definitions.mustache` and
  `tc_input_embedded.mustache`: adjacency arrays, queue/visited traversal, and
  embedded fact loading.
- `src/unifyweaver/core/advanced/tail_recursion.pl`: shared target hook dispatch
  and its missing-support diagnostic.

The required core built-ins also completed with exit 0 in separate processes:

```bash
swipl -q -s init.pl -g "use_module(unifyweaver(core/template_system)), test_template_system, halt"
swipl -q -s init.pl -g "use_module(unifyweaver(core/stream_compiler)), test_stream_compiler, halt"
swipl -q -s init.pl -g "use_module(unifyweaver(core/recursive_compiler)), test_recursive_compiler, halt"
```

The chapter's complete `factorial_recipe.pl` code block was also extracted
verbatim and executed separately, with `field_separator('\t')`. Its input rows
`5 TAB 1 TAB 0` and `0 TAB 1 TAB 0` yielded `120` and `1`.

No compiler fixes or benchmark investigations are included.
