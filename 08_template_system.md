# Chapter 8: The Template System

## Introduction

UnifyWeaver generates bash code using a **template system** rather than building strings with concatenation. This approach keeps code generation clean, maintainable, and readable.

This chapter explains how the template system works and how you can use or modify templates.

## Why Templates?

### The Problem with String Concatenation

**Bad approach (concatenation):**
```prolog
% ❌ Hard to read and maintain
generate_function(Name, Body, Code) :-
    format(string(Code), '#!/bin/bash\n~w() {\n~w\n}\n', [Name, Body]).
```

Problems:
- Hard to see the overall structure
- Escape sequences (`\n`) clutter the code
- Difficult to format multi-line bash code
- Hard to modify templates later

**Good approach (templates):**
```prolog
% ✅ Clear, readable, maintainable
Template = '#!/bin/bash
{{name}}() {
{{body}}
}',
render_template(Template, [name=Name, body=Body], Code).
```

Benefits:
- Template looks like actual bash code
- Easy to see structure
- Simple placeholder syntax `{{name}}`
- Easy to modify and extend

## The `render_template/3` Predicate

**Signature:**
```prolog
render_template(+Template, +Dict, -Result)
```

**Parameters:**
- `Template`: String or atom with `{{placeholders}}`
- `Dict`: List of `key=value` pairs
- `Result`: Generated code with placeholders replaced

### Example Usage

```prolog
?- render_template('Hello {{name}}!', [name='World'], Result).
Result = 'Hello World!'.

?- render_template(
    '#!/bin/bash\n{{cmd}}() { echo "{{msg}}"; }',
    [cmd=greet, msg='Hello'],
    Code
  ).
Code = '#!/bin/bash\ngreet() { echo "Hello"; }'.
```

## List-of-Strings Pattern

For complex bash code, UnifyWeaver uses the **list-of-strings** pattern:

**From `tail_recursion.pl`:**
```prolog
generate_ternary_tail_loop(PredStr, AccPos, StepOp, BashCode) :-
    % Convert step operation to bash code
    step_op_to_bash(StepOp, BashStepOp),

    % Use list-of-strings template style
    TemplateLines = [
        "#!/bin/bash",
        "# {{pred}} - tail recursive accumulator pattern",
        "# Compiled to iterative while loop",
        "",
        "{{pred}}() {",
        "    local input=\"$1\"",
        "    local acc=\"$2\"",
        "    local result_var=\"$3\"",
        "    ",
        "    # Convert input to array if it's a list notation",
        "    if [[ \"$input\" =~ ^\\[.*\\]$ ]]; then",
        "        input=\"${input#[}\"",
        "        input=\"${input%]}\"",
        "        IFS=',' read -ra items <<< \"$input\"",
        "    else",
        "        items=()",
        "    fi",
        "    ",
        "    local current_acc=\"$acc\"",
        "    ",
        "    # Iterative loop (tail recursion optimization)",
        "    for item in \"${items[@]}\"; do",
        "        # Step operation",
        "        {{step_op}}",
        "    done",
        "    ",
        "    # Return result",
        "    if [[ -n \"$result_var\" ]]; then",
        "        eval \"$result_var=$current_acc\"",
        "    else",
        "        echo \"$current_acc\"",
        "    fi",
        "}",
        "",
        "# Helper function for common use case",
        "{{pred}}_eval() {",
        "    {{pred}} \"$1\" 0 result",
        "    echo \"$result\"",
        "}"
    ],

    % Join lines and render template
    atomic_list_concat(TemplateLines, '\n', Template),
    render_template(Template, [pred=PredStr, step_op=BashStepOp], BashCode).
```

**Why list-of-strings:**
1. Each bash line is a separate Prolog string
2. No escape sequence confusion
3. Better syntax highlighting in editor
4. Easy to see overall structure
5. Clear indentation

## Built-in Templates

UnifyWeaver provides reusable templates in `template_system.pl`:

### Bash Header Template
```prolog
template(bash_header, '#!/bin/bash
# {{description}}
').
```

### Function Definition Template
```prolog
template(function, '
{{name}}() {
{{body}}
}').
```

### Stream Check Template
```prolog
template(stream_check, '
# Check if {{base}}_stream or {{base}} exists
{{base}}_get_stream() {
    if declare -f {{base}}_stream >/dev/null 2>&1; then
        {{base}}_stream
    elif declare -f {{base}} >/dev/null 2>&1; then
        {{base}}
    else
        echo "Error: neither {{base}}_stream nor {{base}} found" >&2
        return 1
    fi
}').
```

### BFS Initialization Template
```prolog
template(bfs_init, '
    local start="$1"
    declare -A visited
    declare -A output_seen
    local queue_file="/tmp/{{prefix}}_queue_$$"
    local next_queue="/tmp/{{prefix}}_next_$$"
    trap "rm -f $queue_file $next_queue" EXIT
    echo "$start" > "$queue_file"
    visited["$start"]=1').
```

## Complete Example: Transitive Closure Template

The `generate_transitive_closure/3` predicate shows a complete template:

```prolog
generate_transitive_closure(PredName, BaseName, Code) :-
    atom_string(PredName, PredStr),
    atom_string(BaseName, BaseStr),

    Template = '#!/bin/bash
# {{pred}} - transitive closure of {{base}}

# Check for base stream function
{{base}}_get_stream() {
    if declare -f {{base}}_stream >/dev/null 2>&1; then
        {{base}}_stream
    elif declare -f {{base}} >/dev/null 2>&1; then
        {{base}}
    else
        echo "Error: {{base}} not found" >&2
        return 1
    fi
}

# Main function
{{pred}}() {
    local start="$1"
    local target="$2"

    if [[ -z "$target" ]]; then
        {{pred}}_all "$start"
    else
        {{pred}}_check "$start" "$target"
    fi
}

# Find all reachable using BFS
{{pred}}_all() {
    local start="$1"
    declare -A visited
    local queue_file="/tmp/{{pred}}_queue_$$"
    local next_queue="/tmp/{{pred}}_next_$$"

    trap "rm -f $queue_file $next_queue" EXIT PIPE

    echo "$start" > "$queue_file"
    visited["$start"]=1

    while [[ -s "$queue_file" ]]; do
        > "$next_queue"

        while IFS= read -r current; do
            while IFS=":" read -r from to; do
                if [[ "$from" == "$current" && -z "${visited[$to]}" ]]; then
                    visited["$to"]=1
                    echo "$to" >> "$next_queue"
                    echo "$start:$to"
                fi
            done < <({{base}}_get_stream | grep "^$current:")
        done < "$queue_file"

        mv "$next_queue" "$queue_file"
    done

    rm -f "$queue_file" "$next_queue"
}

# Check specific relationship
{{pred}}_check() {
    local start="$1"
    local target="$2"
    {{pred}}_all "$start" | grep -q "^$start:$target$" && echo "$start:$target"
}

# Stream function
{{pred}}_stream() {
    {{pred}}_all "$1"
}',

    % Render with dictionary
    render_template(Template, [
        pred = PredStr,
        base = BaseStr
    ], Code).
```

**Usage:**
```prolog
?- generate_transitive_closure(ancestor, parent, Code).
Code = '#!/bin/bash\n# ancestor - transitive closure of parent\n...'
```

This generates the complete `ancestor.sh` file!

## How Template Rendering Works

### Step 1: Parse Placeholders

The template system finds all `{{name}}` placeholders in the template.

### Step 2: Substitute Values

For each placeholder, look up the value in the dictionary:
```prolog
[pred='ancestor', base='parent']
```

Replace `{{pred}}` with `'ancestor'`
Replace `{{base}}` with `'parent'`

### Step 3: Return Result

All placeholders replaced, return final string.

### Implementation Detail

**From `template_system.pl`:**
```prolog
render_template_string(Template, [], Template) :- !.
render_template_string(Template, [Key=Value|Rest], Result) :-
    format(atom(Placeholder), '{{~w}}', [Key]),
    atom_string(Value, ValueStr),
    atom_string(Template, TemplateStr),
    atom_string(Placeholder, PlaceholderStr),
    replace_substring(TemplateStr, PlaceholderStr, ValueStr, Mid),
    render_template_string(Mid, Rest, Result).

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

**How it works:**
1. Recursively process key=value pairs
2. For each pair, create placeholder string `{{key}}`
3. Find all occurrences in template
4. Replace with value
5. Continue with remaining pairs

## Composing Templates

You can combine multiple templates:

```prolog
compose_templates([Template1, Template2, Template3], Dict, Result).
```

Each template is rendered with the same dictionary, then concatenated.

## Creating Your Own Templates

### Example: Adding a New Pattern

```prolog
% Define template
my_template(
    'myfunction() {\n    echo "{{message}}"\n}\n'
).

% Use it
generate_my_code(Message, Code) :-
    my_template(Template),
    render_template(Template, [message=Message], Code).
```

### Best Practices

1. **Keep templates close to actual bash**
   - Template should look like the output
   - Use actual bash indentation

2. **Use descriptive placeholder names**
   - `{{pred}}` not `{{p}}`
   - `{{base_name}}` not `{{bn}}`

3. **List-of-strings for complex code**
   - Each line is a separate string
   - Join with `atomic_list_concat(Lines, '\n', Template)`

4. **Test templates separately**
   - Use `render_template/3` directly
   - Verify output before integrating

## Testing Templates

UnifyWeaver includes template tests:

```prolog
?- test_template_system.
=== Testing Template System ===
Test 1 - Simple substitution: PASS
Test 2 - Multiple substitutions: PASS
Test 3 - Generate transitive closure: PASS - contains ancestor_all function
=== Template System Tests Complete ===
```

## Summary

**Key Points:**

1. **Templates separate structure from data**
   - Template = structure (bash code)
   - Dictionary = data (predicate names, etc.)

2. **Simple placeholder syntax**
   - `{{name}}` in template
   - `name=value` in dictionary

3. **List-of-strings for readability**
   - One string per bash line
   - Join with `atomic_list_concat`

4. **Reusable templates**
   - Define once, use many times
   - Compose templates together

5. **Easy to test and modify**
   - Test rendering separately
   - Change template without touching logic

**Benefits:**
- ✅ Maintainable code generation
- ✅ Readable templates
- ✅ Easy to debug
- ✅ Extensible system

---

## Next Steps

Now that you understand how code is generated, the next chapter will explore **advanced recursion patterns** - the sophisticated techniques UnifyWeaver uses to optimize tail recursion, linear recursion, and mutual recursion.
