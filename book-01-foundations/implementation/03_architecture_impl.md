<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3 Implementation: UnifyWeaver Architecture

**Detailed function documentation for RAG systems**

This document provides implementation details for the core modules of the UnifyWeaver compilation pipeline: constraint analyzer, template system, and incremental compilation.

---

## Table of Contents

1. [Constraint Analyzer Module](#constraint-analyzer-module)
2. [Template System Module](#template-system-module)
3. [Incremental Compilation](#incremental-compilation)
4. [Deduplication Strategies](#deduplication-strategies)

---

## Constraint Analyzer Module

**Source**: `src/unifyweaver/core/constraint_analyzer.pl`

The constraint analyzer manages predicate constraints that guide optimization decisions during compilation.

### Core Concepts

| Constraint | Values | Meaning |
|------------|--------|---------|
| `unique` | `true`/`false` | Whether duplicate results should be eliminated |
| `unordered` | `true`/`false` | Whether result order matters |

### Default Constraints

The system defaults to `unique(true), unordered(true)`:

```prolog
:- assertz(default_constraint(unique(true))).
:- assertz(default_constraint(unordered(true))).
```

**Rationale**:
- Most Prolog queries don't care about result order
- Allows use of efficient `sort -u` instead of hash tables
- Easy to override for temporal/ordered data

---

### get_constraints/2

```prolog
get_constraints(+Pred, -Constraints)
```

**Purpose**: Gets the effective constraints for a predicate by merging defaults with any declared overrides.

**Algorithm**:
1. Check if predicate has declared constraints (`predicate_constraint/2`)
2. If yes, merge with defaults (declared takes precedence)
3. If no, return defaults

**Implementation**:

```prolog
get_constraints(Pred, Constraints) :-
    (   predicate_constraint(Pred, DeclaredConstraints) ->
        get_default_constraints(Defaults),
        merge_constraints(Defaults, DeclaredConstraints, Constraints)
    ;   get_default_constraints(Constraints)
    ).
```

**Example**:

```prolog
?- get_constraints(undeclared_pred/2, C).
C = [unique(true), unordered(true)].

?- declare_constraint(temporal/2, [ordered]).
?- get_constraints(temporal/2, C).
C = [unique(true), unordered(false)].
```

---

### declare_constraint/2

```prolog
declare_constraint(+Pred, +Constraints)
```

**Purpose**: Declares constraints for a specific predicate, overriding defaults.

**Shorthand Normalization**:

| Shorthand | Normalized Form |
|-----------|-----------------|
| `unique` | `unique(true)` |
| `unordered` | `unordered(true)` |
| `ordered` | `unordered(false)` |

**Implementation**:

```prolog
declare_constraint(Pred, Constraints) :-
    retractall(predicate_constraint(Pred, _)),
    normalize_constraints(Constraints, Normalized),
    assertz(predicate_constraint(Pred, Normalized)).
```

**Normalization Rules**:

```prolog
normalize_constraints([unique|Rest], [unique(true)|NormRest]) :- !,
    normalize_constraints(Rest, NormRest).
normalize_constraints([ordered|Rest], [unordered(false)|NormRest]) :- !,
    normalize_constraints(Rest, NormRest).
```

---

### get_dedup_strategy/2

```prolog
get_dedup_strategy(+Constraints, -Strategy)
```

**Purpose**: Determines the deduplication strategy for code generation based on constraints.

**Strategy Selection**:

| unique | unordered | Strategy |
|--------|-----------|----------|
| `true` | `true` | `sort_u` |
| `true` | `false` | `hash_dedup` |
| `false` | any | `no_dedup` |

**Implementation**:

```prolog
get_dedup_strategy(Constraints, Strategy) :-
    (   constraint_implies_sort_u(Constraints) ->
        Strategy = sort_u
    ;   constraint_implies_hash(Constraints) ->
        Strategy = hash_dedup
    ;   member(unique(false), Constraints) ->
        Strategy = no_dedup
    ;   Strategy = sort_u  % Default fallback
    ).

constraint_implies_sort_u(Constraints) :-
    member(unique(true), Constraints),
    member(unordered(true), Constraints).

constraint_implies_hash(Constraints) :-
    member(unique(true), Constraints),
    member(unordered(false), Constraints).
```

---

### Pragma Directive Support

Constraints can be declared using pragma-style directives:

```prolog
:- constraint(grandparent/2, [unique, unordered]).
:- constraint(temporal_query/2, [unique, ordered]).
```

**Implementation** (term expansion):

```prolog
user:term_expansion(
    (:- constraint(Pred, Constraints)),
    (:- initialization(constraint_analyzer:declare_constraint(Pred, Constraints)))
).
```

---

## Template System Module

**Source**: `src/unifyweaver/core/template_system.pl`

The template system provides named placeholder substitution and composable template units for code generation.

### Core Concepts

- **Templates**: Strings with `{{placeholder}}` markers
- **Dictionary**: List of `Key=Value` pairs for substitution
- **Sources**: Where templates come from (generated, file, cached)

---

### render_template/3

```prolog
render_template(+Template, +Dict, -Result)
```

**Purpose**: Replaces `{{name}}` placeholders with corresponding values from dictionary.

**Algorithm**:
1. For each `Key=Value` in dictionary:
   a. Create placeholder string `{{Key}}`
   b. Replace all occurrences in template
2. Return result after all substitutions

**Implementation**:

```prolog
render_template(Template, Dict, Result) :-
    atom_string(Template, TStr),
    render_template_string(TStr, Dict, Result).

render_template_string(Template, [], Template) :- !.
render_template_string(Template, [Key=Value|Rest], Result) :-
    format(atom(Placeholder), '{{~w}}', [Key]),
    atom_string(Value, ValueStr),
    atom_string(Template, TemplateStr),
    atom_string(Placeholder, PlaceholderStr),
    replace_substring(TemplateStr, PlaceholderStr, ValueStr, Mid),
    render_template_string(Mid, Rest, Result).
```

**Example**:

```prolog
?- render_template('Hello {{name}}!', [name='World'], R).
R = "Hello World!".

?- render_template('{{greeting}} {{name}}', [greeting='Hi', name='Alice'], R).
R = "Hi Alice".
```

---

### replace_substring/4

```prolog
replace_substring(+String, +Find, +Replace, -Result)
```

**Purpose**: Replaces all occurrences of a substring.

**Algorithm** (recursive):
1. Find first occurrence of `Find` in `String`
2. Split into prefix and suffix
3. Recursively process suffix
4. Concatenate: `Prefix + Replace + ProcessedSuffix`

**Implementation**:

```prolog
replace_substring(String, Find, Replace, Result) :-
    string_length(Find, FindLen),
    (   sub_string(String, Before, FindLen, After, Find)
    ->  sub_string(String, 0, Before, _, Prefix),
        Start is Before + FindLen,
        sub_string(String, Start, After, 0, Suffix),
        replace_substring(Suffix, Find, Replace, RestResult),
        string_concat(Prefix, Replace, Part1),
        string_concat(Part1, RestResult, Result)
    ;   Result = String
    ).
```

---

### load_template/2, load_template/3

```prolog
load_template(+TemplateName, -TemplateString)
load_template(+TemplateName, +Options, -TemplateString)
```

**Purpose**: Loads a template by name using configurable strategies.

**Source Order** (configurable):

| Strategy | Description |
|----------|-------------|
| `generated` | Hardcoded `template/2` facts in Prolog |
| `file` | External `.tmpl.sh` files |
| `cached` | Previously cached templates |

**Default Configuration**:

```prolog
template_config_default([
    source_order([generated]),  % Only hardcoded by default
    template_dir('templates'),
    cache_dir('templates/cache'),
    template_extension('.tmpl.sh'),
    auto_cache(false)
]).
```

**Strategy Resolution**:

```prolog
try_sources(Name, [Strategy|Rest], Config, Template) :-
    (   try_source(Name, Strategy, Config, Template)
    ->  % Success - optionally cache
        ...
    ;   % Fail - try next strategy
        try_sources(Name, Rest, Config, Template)
    ).
```

---

### template/2

```prolog
template(+Name, +TemplateBody)
```

**Purpose**: Defines a named template (as Prolog fact).

**Body Formats**:
- Single string: `'#!/bin/bash\n...'`
- List of lines: `["line1", "line2", ...]`

**Example Templates**:

```prolog
template(bash_header, '#!/bin/bash
# {{description}}
').

template(function, '
{{name}}() {
{{body}}
}').

template('facts/lookup_binary', [
"{{pred}}() {",
"  local key=\"$1:$2\"",
"  [[ -n \"${{{pred}}_data[$key]}\" ]] && echo \"$key\"",
"}",
""
]).
```

---

### render_named_template/3, render_named_template/4

```prolog
render_named_template(+TemplateName, +Dict, -Result)
render_named_template(+TemplateName, +Dict, +Options, -Result)
```

**Purpose**: Loads a template by name and renders it with dictionary.

**Implementation**:

```prolog
render_named_template(TemplateName, Dict, Options, Result) :-
    load_template(TemplateName, Options, Template),
    render_template(Template, Dict, Result).
```

**Example**:

```prolog
?- render_named_template(bash_header, [description='My Script'], R).
R = "#!/bin/bash\n# My Script\n".
```

---

### compose_templates/3

```prolog
compose_templates(+TemplateNames, +Dict, -Result)
```

**Purpose**: Concatenates multiple rendered templates.

**Implementation**:

```prolog
compose_templates([], _, "") :- !.
compose_templates([Name|Rest], Dict, Result) :-
    render_named_template(Name, Dict, [source_order([generated, file])], R1),
    compose_templates(Rest, Dict, Rs),
    string_concat(R1, Rs, Result).
```

---

### Template Caching

**cache_template/2**: Stores template in memory with timestamp.

```prolog
cache_template(Name, Template) :-
    get_time(Timestamp),
    retractall(cached_template(Name, _, _)),
    assertz(cached_template(Name, Template, Timestamp)).
```

**clear_template_cache/0, clear_template_cache/1**: Removes cached templates.

---

### generate_transitive_closure/4

```prolog
generate_transitive_closure(+PredName, +BaseName, +Options, -Code)
```

**Purpose**: Generates complete Bash code for transitive closure (BFS algorithm).

**Generated Functions**:

| Function | Purpose |
|----------|---------|
| `{base}_get_stream` | Finds base fact stream function |
| `{pred}_all` | BFS traversal from starting node |
| `{pred}_check` | Tests specific relationship |
| `{pred}` | Main entry point with deduplication |

**Implementation** (key parts):

```prolog
generate_transitive_closure(PredName, BaseName, Options, Code) :-
    atom_string(PredName, PredStr),
    atom_string(BaseName, BaseStr),
    constraint_analyzer:get_dedup_strategy(Options, Strategy),

    Template = '#!/bin/bash
# {{pred}} - transitive closure of {{base}}
...
{{pred}}() {
    ...
    if [[ "{{strategy}}" == "sort_u" ]]; then
        {{pred}}_all "$start" | sort -u
    elif [[ "{{strategy}}" == "hash_dedup" ]]; then
        declare -A seen
        {{pred}}_all "$start" | while IFS= read -r line; do
            if [[ -z "${seen[$line]}" ]]; then
                seen[$line]=1
                echo "$line"
            fi
        done
    ...
}',

    render_template(Template, [
        pred = PredStr,
        base = BaseStr,
        strategy = Strategy
    ], Code).
```

---

## Incremental Compilation

**Source**: `src/unifyweaver/incremental/incremental_compiler.pl`

The incremental compilation system caches compiled code to avoid recompiling unchanged predicates.

### How It Works

```
Predicate → Hash (term_hash/2) → Cache Lookup
                                      │
                        ┌─────────────┴─────────────┐
                        │ Hit                       │ Miss
                        ▼                           ▼
                  Return Cached              Compile Fresh
                                             Store in Cache
```

### Key Features

| Feature | Implementation |
|---------|----------------|
| Predicate Hashing | `term_hash/2` with variable normalization |
| Dependency Tracking | Automatic invalidation of dependents |
| Multi-Target Support | Independent caches per target |
| Disk Persistence | Saved to `.unifyweaver_cache/` |

### Usage

```prolog
% Compile with caching (default)
?- compile_incremental(edge/2, bash, [], Code).

% Force fresh compilation
?- compile_incremental(edge/2, bash, [incremental(false)], Code).

% View statistics
?- incremental_stats.

% Save/clear cache
?- save_cache.
?- clear_all_cache.
```

### Disabling Incremental Compilation

| Level | Method |
|-------|--------|
| Per-call | `[incremental(false)]` option |
| Per-session | `set_prolog_flag(unifyweaver_incremental, false)` |
| Environment | `UNIFYWEAVER_CACHE=0` |

---

## Deduplication Strategies

The constraint system drives deduplication strategy selection for generated code.

### Strategy: `sort_u`

**When**: `unique(true), unordered(true)`

**Generated Bash**:

```bash
{{pred}}() {
    {{pred}}_all "$start" | sort -u
}
```

**Characteristics**:
- Simple and efficient
- Requires buffering entire output
- Changes result order (sorted)

---

### Strategy: `hash_dedup`

**When**: `unique(true), unordered(false)`

**Generated Bash**:

```bash
{{pred}}() {
    declare -A seen
    {{pred}}_all "$start" | while IFS= read -r line; do
        if [[ -z "${seen[$line]}" ]]; then
            seen[$line]=1
            echo "$line"
        fi
    done
}
```

**Characteristics**:
- Preserves original order
- Streaming (no buffering)
- Higher memory for large result sets

---

### Strategy: `no_dedup`

**When**: `unique(false)`

**Generated Bash**:

```bash
{{pred}}() {
    {{pred}}_all "$start"
}
```

**Characteristics**:
- No processing overhead
- May return duplicates
- Fastest option

---

## Configuration Merging

The template system uses a three-level configuration merge:

```
Runtime Options > Per-Template Config > Global Defaults
```

**Implementation**:

```prolog
get_template_config(TemplateName, RuntimeOptions, MergedConfig) :-
    template_config_default(DefaultConfig),
    (   template_config(TemplateName, TemplateConfig)
    ->  true
    ;   TemplateConfig = []
    ),
    merge_options(RuntimeOptions, TemplateConfig, Temp),
    merge_options(Temp, DefaultConfig, MergedConfig).
```

**merge_options/3** gives priority list precedence:

```prolog
merge_options([], Fallback, Fallback) :- !.
merge_options([H|T], Fallback, Merged) :-
    H =.. [Key, _],
    delete_option(Fallback, Key, Fallback2),
    merge_options(T, Fallback2, Temp),
    Merged = [H|Temp].
```

---

## Source Files

- `src/unifyweaver/core/constraint_analyzer.pl`
- `src/unifyweaver/core/template_system.pl`
- `src/unifyweaver/core/recursive_compiler.pl`
- `src/unifyweaver/incremental/incremental_compiler.pl`

## See Also

- Chapter 3: UnifyWeaver Architecture (conceptual overview)
- Book 2: Bash Target (practical examples)
- API Reference: Complete predicate documentation
