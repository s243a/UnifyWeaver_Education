<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: Architecture - Questions

**Q&A companion to [03_architecture_impl.md](./03_architecture_impl.md)**

This document contains questions about the UnifyWeaver architecture implementation. Each question links to the relevant section in the implementation documentation.

---

<a id="b01c03-q-what-is-constraint-analyzer"></a>
## Q: What is the constraint analyzer module?

The constraint analyzer manages predicate constraints that guide optimization decisions during compilation. It determines whether results should be deduplicated and whether order matters.

**Key predicates**: `get_constraints/2`, `declare_constraint/2`, `get_dedup_strategy/2`

**Reference**: [Constraint Analyzer Module](./03_architecture_impl.md#constraint-analyzer-module)

---

<a id="b01c03-q-default-constraints"></a>
## Q: What are the default constraints in UnifyWeaver?

The system defaults to `unique(true), unordered(true)`.

**Rationale**:
- Most Prolog queries don't care about result order
- Allows efficient `sort -u` instead of hash tables
- Easy to override for temporal/ordered data

**Reference**: [Default Constraints](./03_architecture_impl.md#default-constraints)

---

<a id="b01c03-q-get-constraints-usage"></a>
## Q: How does `get_constraints/2` work?

`get_constraints(+Pred, -Constraints)` gets the effective constraints for a predicate by merging defaults with any declared overrides.

**Algorithm**:
1. Check if predicate has declared constraints
2. If yes, merge with defaults (declared takes precedence)
3. If no, return defaults

**Example**:
```prolog
?- get_constraints(undeclared_pred/2, C).
C = [unique(true), unordered(true)].
```

**Reference**: [get_constraints/2](./03_architecture_impl.md#get_constraints2)

---

<a id="b01c03-q-declare-constraint"></a>
## Q: How do I declare custom constraints for a predicate?

Use `declare_constraint/2` with the predicate indicator and a list of constraints:

```prolog
declare_constraint(temporal_query/2, [unique, ordered]).
declare_constraint(allow_dupes/2, [unique(false)]).
```

**Shorthands**:
- `unique` → `unique(true)`
- `ordered` → `unordered(false)`
- `unordered` → `unordered(true)`

**Reference**: [declare_constraint/2](./03_architecture_impl.md#declare_constraint2)

---

<a id="b01c03-q-dedup-strategies"></a>
## Q: What deduplication strategies are available?

Three strategies based on constraints:

| Strategy | When Used | Bash Code |
|----------|-----------|-----------|
| `sort_u` | unique=true, unordered=true | `| sort -u` |
| `hash_dedup` | unique=true, unordered=false | `declare -A seen; ...` |
| `no_dedup` | unique=false | No processing |

**Reference**: [get_dedup_strategy/2](./03_architecture_impl.md#get_dedup_strategy2)

---

<a id="b01c03-q-pragma-constraint"></a>
## Q: Can I declare constraints using pragma directives?

Yes, use the `:- constraint/2` directive:

```prolog
:- constraint(grandparent/2, [unique, unordered]).
:- constraint(temporal_query/2, [unique, ordered]).
```

This is expanded via `term_expansion/2` to call `declare_constraint/2` at initialization.

**Reference**: [Pragma Directive Support](./03_architecture_impl.md#pragma-directive-support)

---

<a id="b01c03-q-what-is-template-system"></a>
## Q: What is the template system module?

The template system provides named placeholder substitution (`{{name}}` syntax) and composable template units for code generation.

**Core concepts**:
- Templates: Strings with `{{placeholder}}` markers
- Dictionary: List of `Key=Value` pairs
- Sources: Where templates come from (generated, file, cached)

**Reference**: [Template System Module](./03_architecture_impl.md#template-system-module)

---

<a id="b01c03-q-render-template"></a>
## Q: How does `render_template/3` work?

`render_template(+Template, +Dict, -Result)` replaces `{{name}}` placeholders with dictionary values.

**Example**:
```prolog
?- render_template('Hello {{name}}!', [name='World'], R).
R = "Hello World!".

?- render_template('{{greeting}} {{name}}', [greeting='Hi', name='Alice'], R).
R = "Hi Alice".
```

**Reference**: [render_template/3](./03_architecture_impl.md#render_template3)

---

<a id="b01c03-q-replace-substring"></a>
## Q: How does substring replacement work in the template system?

`replace_substring/4` recursively replaces all occurrences:

1. Find first occurrence of `Find` in `String`
2. Split into prefix and suffix
3. Recursively process suffix
4. Concatenate: `Prefix + Replace + ProcessedSuffix`

**Reference**: [replace_substring/4](./03_architecture_impl.md#replace_substring4)

---

<a id="b01c03-q-template-sources"></a>
## Q: Where can templates be loaded from?

Three sources in configurable priority order:

| Source | Description |
|--------|-------------|
| `generated` | Hardcoded `template/2` facts in Prolog |
| `file` | External `.tmpl.sh` files |
| `cached` | Previously cached templates |

Default: `source_order([generated])` - only hardcoded templates.

**Reference**: [load_template/2, load_template/3](./03_architecture_impl.md#load_template2-load_template3)

---

<a id="b01c03-q-define-template"></a>
## Q: How do I define a template in Prolog?

Use `template/2` facts with string or list body:

```prolog
% Single string
template(bash_header, '#!/bin/bash
# {{description}}
').

% List of lines (joined with newlines)
template('facts/lookup_binary', [
"{{pred}}() {",
"  local key=\"$1:$2\"",
"  [[ -n \"${{{pred}}_data[$key]}\" ]] && echo \"$key\"",
"}",
""
]).
```

**Reference**: [template/2](./03_architecture_impl.md#template2)

---

<a id="b01c03-q-render-named-template"></a>
## Q: What is `render_named_template/3`?

Loads a template by name and renders it with a dictionary in one step:

```prolog
?- render_named_template(bash_header, [description='My Script'], R).
R = "#!/bin/bash\n# My Script\n".
```

Equivalent to `load_template/2` + `render_template/3`.

**Reference**: [render_named_template/3, render_named_template/4](./03_architecture_impl.md#render_named_template3-render_named_template4)

---

<a id="b01c03-q-compose-templates"></a>
## Q: How do I combine multiple templates?

Use `compose_templates/3` to concatenate multiple rendered templates:

```prolog
compose_templates([bash_header, function, stream_wrapper], Dict, Result)
```

Each template is rendered with the same dictionary and concatenated.

**Reference**: [compose_templates/3](./03_architecture_impl.md#compose_templates3)

---

<a id="b01c03-q-template-caching"></a>
## Q: How does template caching work?

- `cache_template(Name, Template)` - Store in memory with timestamp
- `get_cached_template(Name, Template)` - Retrieve if exists
- `clear_template_cache/0` - Clear all cached templates
- `clear_template_cache(Name)` - Clear specific template

When `auto_cache(true)` is set, loaded templates are automatically cached.

**Reference**: [Template Caching](./03_architecture_impl.md#template-caching)

---

<a id="b01c03-q-generate-transitive-closure"></a>
## Q: How does `generate_transitive_closure/4` generate BFS code?

`generate_transitive_closure(+PredName, +BaseName, +Options, -Code)` generates complete Bash code for transitive closure using BFS.

**Generated functions**:
- `{base}_get_stream` - Finds base fact stream
- `{pred}_all` - BFS traversal
- `{pred}_check` - Tests specific relationship
- `{pred}` - Main entry with deduplication

**Reference**: [generate_transitive_closure/4](./03_architecture_impl.md#generate_transitive_closure4)

---

<a id="b01c03-q-incremental-compilation"></a>
## Q: What is incremental compilation?

Incremental compilation caches compiled code to avoid recompiling unchanged predicates.

**How it works**:
1. Hash predicate source with `term_hash/2`
2. Check cache for matching hash
3. Return cached code or compile fresh

**Performance**: ~11x speedup from cache hits.

**Reference**: [Incremental Compilation](./03_architecture_impl.md#incremental-compilation)

---

<a id="b01c03-q-disable-incremental"></a>
## Q: How do I disable incremental compilation?

Three levels:

| Level | Method |
|-------|--------|
| Per-call | `[incremental(false)]` option |
| Per-session | `set_prolog_flag(unifyweaver_incremental, false)` |
| Environment | `UNIFYWEAVER_CACHE=0` |

**Reference**: [Incremental Compilation](./03_architecture_impl.md#incremental-compilation)

---

<a id="b01c03-q-sort-u-strategy"></a>
## Q: When is the `sort_u` deduplication strategy used?

When `unique(true)` AND `unordered(true)` (the default).

**Generated Bash**:
```bash
{{pred}}() {
    {{pred}}_all "$start" | sort -u
}
```

**Characteristics**: Simple, efficient, changes result order.

**Reference**: [Strategy: sort_u](./03_architecture_impl.md#strategy-sort_u)

---

<a id="b01c03-q-hash-dedup-strategy"></a>
## Q: When is the `hash_dedup` deduplication strategy used?

When `unique(true)` AND `unordered(false)` (order preserved).

**Generated Bash**:
```bash
declare -A seen
{{pred}}_all "$start" | while IFS= read -r line; do
    if [[ -z "${seen[$line]}" ]]; then
        seen[$line]=1
        echo "$line"
    fi
done
```

**Characteristics**: Preserves order, streaming, higher memory.

**Reference**: [Strategy: hash_dedup](./03_architecture_impl.md#strategy-hash_dedup)

---

<a id="b01c03-q-config-merging"></a>
## Q: How does configuration merging work in the template system?

Three-level merge with priority:

```
Runtime Options > Per-Template Config > Global Defaults
```

Options from higher priority override lower priority on a per-key basis.

**Reference**: [Configuration Merging](./03_architecture_impl.md#configuration-merging)

---

## Question Index

| ID | Topic |
|----|-------|
| [b01c03-q-what-is-constraint-analyzer](#b01c03-q-what-is-constraint-analyzer) | Constraint analyzer overview |
| [b01c03-q-default-constraints](#b01c03-q-default-constraints) | Default constraints |
| [b01c03-q-get-constraints-usage](#b01c03-q-get-constraints-usage) | get_constraints/2 |
| [b01c03-q-declare-constraint](#b01c03-q-declare-constraint) | declare_constraint/2 |
| [b01c03-q-dedup-strategies](#b01c03-q-dedup-strategies) | Deduplication strategies |
| [b01c03-q-pragma-constraint](#b01c03-q-pragma-constraint) | Pragma directives |
| [b01c03-q-what-is-template-system](#b01c03-q-what-is-template-system) | Template system overview |
| [b01c03-q-render-template](#b01c03-q-render-template) | render_template/3 |
| [b01c03-q-replace-substring](#b01c03-q-replace-substring) | replace_substring/4 |
| [b01c03-q-template-sources](#b01c03-q-template-sources) | Template sources |
| [b01c03-q-define-template](#b01c03-q-define-template) | Defining templates |
| [b01c03-q-render-named-template](#b01c03-q-render-named-template) | render_named_template/3 |
| [b01c03-q-compose-templates](#b01c03-q-compose-templates) | compose_templates/3 |
| [b01c03-q-template-caching](#b01c03-q-template-caching) | Template caching |
| [b01c03-q-generate-transitive-closure](#b01c03-q-generate-transitive-closure) | Transitive closure generation |
| [b01c03-q-incremental-compilation](#b01c03-q-incremental-compilation) | Incremental compilation |
| [b01c03-q-disable-incremental](#b01c03-q-disable-incremental) | Disabling incremental |
| [b01c03-q-sort-u-strategy](#b01c03-q-sort-u-strategy) | sort_u strategy |
| [b01c03-q-hash-dedup-strategy](#b01c03-q-hash-dedup-strategy) | hash_dedup strategy |
| [b01c03-q-config-merging](#b01c03-q-config-merging) | Configuration merging |
