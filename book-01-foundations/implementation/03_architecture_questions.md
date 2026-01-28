<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: UnifyWeaver Architecture - Questions

Q&A companion for [03_architecture_impl.md](./03_architecture_impl.md).

---

## Question Index

1. [What are the stages of the compilation pipeline?](#b01c03-q-pipeline-stages)
2. [What does the compile/3 predicate do?](#b01c03-q-compile)
3. [What is template_system.pl?](#b01c03-q-template-system)
4. [What is stream_compiler.pl?](#b01c03-q-stream-compiler)
5. [What is recursive_compiler.pl?](#b01c03-q-recursive-compiler)
6. [What is constraint_analyzer.pl?](#b01c03-q-constraint-analyzer)
7. [How are facts compiled to Bash?](#b01c03-q-facts-to-bash)
8. [How are rules compiled to Bash?](#b01c03-q-rules-to-bash)
9. [What is incremental compilation?](#b01c03-q-incremental)
10. [How do I disable incremental compilation?](#b01c03-q-disable-incremental)
11. [What architecture variants exist?](#b01c03-q-variants)
12. [What constraints are available?](#b01c03-q-constraints)

---

## Questions and Answers

### <a id="b01c03-q-pipeline-stages"></a>Q1: What are the stages of the compilation pipeline?

**Answer**: The compilation pipeline has 5 stages:

1. **Pattern Analysis** - Classify predicate (facts, non-recursive, recursive)
2. **Strategy Selection** - Route to stream_compiler or advanced_recursive_compiler
3. **Advanced Pattern Matching** - For recursive: try tail → linear → graph → mutual
4. **Constraint Analysis** - Fetch optimization hints (unique, ordered)
5. **Template Rendering** - Generate final code from templates

**See**: [Compilation Pipeline Overview](./03_architecture_impl.md#compilation-pipeline-overview)

---

### <a id="b01c03-q-compile"></a>Q2: What does the compile/3 predicate do?

**Answer**: `compile/3` is the main entry point for compilation:

```prolog
compile(+Predicate/Arity, +Options, -Scripts)
```

It:
1. Analyzes the predicate structure
2. Selects appropriate compilation strategy
3. Generates code using templates
4. Writes output files

Example:
```prolog
?- compile(edge/2, [target(bash)], Scripts).
```

**See**: [compile/3](./03_architecture_impl.md#compile3)

---

### <a id="b01c03-q-template-system"></a>Q3: What is template_system.pl?

**Answer**: The template system is a flexible templating engine for code generation. It uses mustache-like syntax:

```
#!/bin/bash
declare -A {{predicate_name}}_data=(
{{#facts}}
    [{{key}}]=1
{{/facts}}
)
```

The `render_template/3` predicate fills in variables and generates output code.

**See**: [template_system.pl](./03_architecture_impl.md#template_systempl)

---

### <a id="b01c03-q-stream-compiler"></a>Q4: What is stream_compiler.pl?

**Answer**: The stream compiler handles non-recursive predicates by generating Unix pipeline-style code. It converts Prolog rules into chains of filters, joins, and transformations.

```prolog
stream_compiler:compile_stream(predicate/2, [], Code)
```

**See**: [stream_compiler.pl](./03_architecture_impl.md#stream_compilerpl)

---

### <a id="b01c03-q-recursive-compiler"></a>Q5: What is recursive_compiler.pl?

**Answer**: The recursive compiler is the main dispatcher. It:

1. **Classifies** predicates using `classify_predicate/2`:
   - `facts` - Pure facts
   - `non_recursive` - Rules without self-reference
   - `recursive` - Contains recursive calls

2. **Routes** to the appropriate specialized compiler

**See**: [recursive_compiler.pl](./03_architecture_impl.md#recursive_compilerpl)

---

### <a id="b01c03-q-constraint-analyzer"></a>Q6: What is constraint_analyzer.pl?

**Answer**: The constraint analyzer manages optimization hints for predicates:

```prolog
get_constraints(predicate/2, Constraints)
% Returns: [unique(true), unordered(true), ...]
```

Constraints guide code generation (e.g., whether to deduplicate, preserve order).

**See**: [constraint_analyzer.pl](./03_architecture_impl.md#constraint_analyzerpl)

---

### <a id="b01c03-q-facts-to-bash"></a>Q7: How are facts compiled to Bash?

**Answer**: Facts become Bash associative arrays for O(1) lookup:

```prolog
parent(alice, bob).
```

Generates:
```bash
declare -A parent_data=(
    ["alice:bob"]=1
)

parent() {
    local key="$1:$2"
    [[ -n "${parent_data[$key]}" ]] && echo "$key"
}
```

**See**: [Facts as Associative Arrays](./03_architecture_impl.md#facts-as-associative-arrays)

---

### <a id="b01c03-q-rules-to-bash"></a>Q8: How are rules compiled to Bash?

**Answer**: Rules become functions with pipelines:

```prolog
grandparent(GP, GC) :- parent(GP, P), parent(P, GC).
```

Generates:
```bash
grandparent() {
    parent_stream | while IFS=: read -r gp p; do
        parent "$p" | while IFS=: read -r _ gc; do
            echo "$gp:$gc"
        done
    done | sort -u
}
```

**See**: [Rules as Functions](./03_architecture_impl.md#rules-as-functions)

---

### <a id="b01c03-q-incremental"></a>Q9: What is incremental compilation?

**Answer**: Incremental compilation caches compiled code to avoid recompiling unchanged predicates:

1. Hash predicate content with `term_hash/2`
2. Check if hash matches cached version
3. Return cached code if match, otherwise compile fresh

Benefits:
- **~11x speedup** from cache hits
- Survives restarts (disk persistence)
- Per-target caching

**See**: [Incremental Compilation](./03_architecture_impl.md#incremental-compilation)

---

### <a id="b01c03-q-disable-incremental"></a>Q10: How do I disable incremental compilation?

**Answer**: Three levels:

| Level | Method |
|-------|--------|
| Per-call | `[incremental(false)]` option |
| Per-session | `set_prolog_flag(unifyweaver_incremental, false)` |
| Environment | `UNIFYWEAVER_CACHE=0` |

Example:
```prolog
?- compile_incremental(edge/2, bash, [incremental(false)], Code).
```

**See**: [Disabling Incremental Compilation](./03_architecture_impl.md#disabling-incremental-compilation)

---

### <a id="b01c03-q-variants"></a>Q11: What architecture variants exist?

**Answer**: Four main variants:

| Variant | Targets | Execution Model |
|---------|---------|-----------------|
| Principal (Stream) | Bash, AWK, Go, Rust | Unix pipelines |
| Fixed-Point | C# Query | Iterative fixpoint |
| Generator | Python | Lazy evaluation |
| Query Engine | C# IR | Plan nodes + runtime |

Core concepts (classification, constraints, templates) apply across all.

**See**: [Architecture Variants](./03_architecture_impl.md#architecture-variants)

---

### <a id="b01c03-q-constraints"></a>Q12: What constraints are available?

**Answer**: Three main constraints:

| Constraint | Values | Effect |
|------------|--------|--------|
| `unique(B)` | `true`/`false` | Enable deduplication |
| `unordered(B)` | `true`/`false` | Allow hash-based dedup |
| `ordered(B)` | `true`/`false` | Preserve input order |

Set in Prolog:
```prolog
:- constraint(my_pred/2, unique(true)).
```

**See**: [Constraint System](./03_architecture_impl.md#constraint-system)

---

## Summary

The UnifyWeaver architecture provides:
- Multi-stage compilation pipeline
- Multiple specialized compilers
- Constraint-based optimization
- Template-driven code generation
- Incremental compilation with caching
