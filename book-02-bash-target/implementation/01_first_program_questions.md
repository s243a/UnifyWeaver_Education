<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 1: Your First Program - Questions

**Q&A companion to [01_first_program_impl.md](./01_first_program_impl.md)**

---

<a id="b02c01-q-compilation-workflow"></a>
## Q: What is the complete compilation workflow?

1. Load `init.pl` → Set up module paths
2. Load your Prolog file → Load facts and rules
3. `compile_facts` → Generate base fact script
4. `compile_recursive` → Generate recursive rule script
5. Save to files → Write `.sh` files
6. Source and run → Execute generated scripts

**Reference**: [Compilation Workflow](./01_first_program_impl.md#compilation-workflow)

---

<a id="b02c01-q-compile-recursive"></a>
## Q: How do I use compile_recursive/3?

```prolog
?- compile_recursive(ancestor/2, [], BashCode).
```

Parameters:
- `Pred/Arity`: e.g., `ancestor/2`
- `Options`: list of options (e.g., `[unique(false)]`)
- `BashCode`: output variable for generated code

**Reference**: [compile_recursive/3](./01_first_program_impl.md#compile_recursive3)

---

<a id="b02c01-q-compile-facts"></a>
## Q: How do I compile facts to Bash?

```prolog
?- stream_compiler:compile_facts(parent, 2, [], BashCode).
```

This generates:
- `parent_data` - associative array
- `parent()` - point lookup function
- `parent_stream()` - stream all facts

**Reference**: [compile_facts/4](./01_first_program_impl.md#compile_facts4)

---

<a id="b02c01-q-generated-structure"></a>
## Q: What does generated Bash code look like?

**Facts script**:
```bash
declare -A parent_data=(["abraham:isaac"]=1 ...)
parent() { local key="$1:$2"; [[ -n "${parent_data[$key]}" ]] && echo "$key"; }
parent_stream() { for key in "${!parent_data[@]}"; do echo "$key"; done; }
```

**Recursive script**: Contains BFS with visited tracking, queue management, and deduplication.

**Reference**: [Generated Script Structure](./01_first_program_impl.md#generated-script-structure)

---

<a id="b02c01-q-script-dependencies"></a>
## Q: In what order should I source generated scripts?

Base facts first, then dependent scripts:

```bash
source parent.sh      # Base facts
source ancestor.sh    # Depends on parent
```

Wrong order causes "function not found" errors.

**Reference**: [Script Dependencies](./01_first_program_impl.md#script-dependencies)

---

<a id="b02c01-q-test-runner"></a>
## Q: How do I generate automatic tests?

```prolog
?- use_module('src/unifyweaver/core/advanced/test_runner_inference').
?- generate_test_runner_inferred('output/test_runner.sh', [output_dir('output')]).
```

This scans scripts, extracts signatures, and generates test cases.

**Reference**: [Test Runner Generation](./01_first_program_impl.md#test-runner-generation)

---

<a id="b02c01-q-unknown-procedure"></a>
## Q: How do I fix "Unknown procedure" error?

```prolog
ERROR: Unknown procedure: my_pred/2
```

**Fix**: Load your predicate file:
```prolog
?- ['my_predicates.pl'].
```

**Reference**: [Troubleshooting](./01_first_program_impl.md#troubleshooting)

---

<a id="b02c01-q-source-sink-error"></a>
## Q: How do I fix "source_sink does not exist" error?

```prolog
ERROR: source_sink `unifyweaver(...)' does not exist
```

**Fix**: Load init.pl first:
```prolog
?- ['education/init'].
```

And make sure you started `swipl` from the project root.

**Reference**: [Troubleshooting](./01_first_program_impl.md#troubleshooting)

---

<a id="b02c01-q-empty-output"></a>
## Q: Why is my script giving empty output?

Check:
1. Base facts loaded? (`source parent.sh`)
2. Correct source order?
3. Predicate has facts?

Debug:
```bash
declare -f parent_stream  # Check function exists
parent_stream | head -5   # Test base facts
```

**Reference**: [Troubleshooting](./01_first_program_impl.md#troubleshooting)

---

## Question Index

| ID | Topic |
|----|-------|
| [b02c01-q-compilation-workflow](#b02c01-q-compilation-workflow) | Compilation workflow |
| [b02c01-q-compile-recursive](#b02c01-q-compile-recursive) | compile_recursive/3 |
| [b02c01-q-compile-facts](#b02c01-q-compile-facts) | compile_facts/4 |
| [b02c01-q-generated-structure](#b02c01-q-generated-structure) | Generated code structure |
| [b02c01-q-script-dependencies](#b02c01-q-script-dependencies) | Script dependencies |
| [b02c01-q-test-runner](#b02c01-q-test-runner) | Test runner generation |
| [b02c01-q-unknown-procedure](#b02c01-q-unknown-procedure) | Unknown procedure error |
| [b02c01-q-source-sink-error](#b02c01-q-source-sink-error) | Source sink error |
| [b02c01-q-empty-output](#b02c01-q-empty-output) | Empty output |
