<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
-->

# Chapter 5: JSON and Pipeline Modes

The Perl target includes options for generating executable scripts that integrate with Unix pipelines.

## JSON Output Mode

The `json_output` option generates a wrapper function that collects results and outputs JSON:

```prolog
?- compile_predicate_to_perl(parent/2, [json_output], Code).
```

### Generated Code Structure

```perl
#!/usr/bin/env perl
use strict;
use warnings;
use JSON;

sub parent {
    my $callback = shift;
    my @facts = (
        ['alice', 'bob'],
        ['bob', 'charlie']
    );
    foreach my $fact (@facts) {
        $callback->(@$fact);
    }
}

# JSON output wrapper
sub parent_json {
    my @results;
    parent(sub {
        push @results, [$_[1], $_[2]];
    });
    print encode_json(\@results);
}

# Run if executed directly
parent_json() unless caller;
```

### Key Features

1. **`use JSON;`**: Imports the JSON module for encoding
2. **`_json` wrapper**: Collects results into `@results` array
3. **Auto-execution**: `parent_json() unless caller` runs when executed directly
4. **Library mode**: `unless caller` allows use as a module without auto-execution

### Usage

```bash
# Direct execution
perl parent.pl
# Output: [["alice","bob"],["bob","charlie"]]

# Pipe to jq for formatting
perl parent.pl | jq .
# Output:
# [
#   ["alice", "bob"],
#   ["bob", "charlie"]
# ]

# Use as library
perl -e 'require "parent.pl"; parent(sub { print "@_\n"; });'
```

## Pipeline Mode

The `pipeline` option generates similar code optimized for stdin/stdout pipelines:

```prolog
?- compile_predicate_to_perl(transform/2, [pipeline], Code).
```

### Generated Code

```perl
#!/usr/bin/env perl
use strict;
use warnings;
use JSON;

sub transform {
    my $callback = shift;
    # ... compiled predicate ...
}

# Pipeline mode - reads JSON from stdin, outputs JSON to stdout
sub run_pipeline {
    my @results;
    transform(sub {
        push @results, [$_[1], $_[2]];
    });
    print encode_json(\@results) . "\n";
}

run_pipeline() unless caller;
```

### Difference from JSON Output

| Feature | `json_output` | `pipeline` |
|---------|---------------|------------|
| Function name | `predicate_json` | `run_pipeline` |
| Output trailing newline | No | Yes (`\n`) |
| Designed for | Direct execution | Chained pipelines |

## Unix Pipeline Integration

### Simple Pipeline

```bash
# Generate data, transform, filter
perl generate_data.pl | perl transform.pl | jq '.[] | select(.[0] == "alice")'
```

### Pipeline with Other Tools

```bash
# Combine with standard Unix tools
perl parent.pl | jq -r '.[] | @tsv' | sort | uniq -c

# Feed into another program
perl parent.pl | python analyze.py

# Store intermediate results
perl parent.pl | tee results.json | jq length
```

### Chaining Generated Scripts

```bash
# Query 1: Find all parents
perl parent.pl > parents.json

# Query 2: Process parents (hypothetical)
cat parents.json | perl find_grandparents.pl > grandparents.json
```

## Practical Examples

### ETL Pipeline

```prolog
% Extract: read employees
:- dynamic employee/3.
employee(1, alice, engineering).
employee(2, bob, marketing).
employee(3, charlie, engineering).

% Transform: filter engineers
engineer(Id, Name) :-
    employee(Id, Name, engineering).
```

Compile and run:

```bash
# Generate scripts
swipl -g "compile_predicate_to_perl(engineer/2, [json_output], C), format('~s', [C]), halt" > engineers.pl

# Run
perl engineers.pl | jq '.[] | {id: .[0], name: .[1]}'
```

Output:
```json
{"id": 1, "name": "alice"}
{"id": 3, "name": "charlie"}
```

### Aggregation Pipeline

```prolog
order_total(Total) :-
    aggregate_all(sum(Amount), order(_, _, Amount), Total).
```

```bash
perl order_total.pl | jq '.[0]'
# Output: 800
```

## Best Practices

### 1. Use Meaningful Wrapper Names

The `_json` suffix is automatic, so choose clear predicate names:

```prolog
active_users/1      → active_users_json
pending_orders/1    → pending_orders_json
```

### 2. Handle Empty Results

Empty result sets produce `[]`:

```bash
perl empty_query.pl
# Output: []
```

### 3. Error Handling

Add error checking in wrapper scripts:

```bash
#!/bin/bash
result=$(perl query.pl 2>&1)
if [ $? -ne 0 ]; then
    echo "Error: $result" >&2
    exit 1
fi
echo "$result" | jq .
```

### 4. Streaming Large Results

For very large result sets, consider:

```perl
# Instead of collecting all results
parent(sub {
    print encode_json([@_]) . "\n";  # Stream one per line
});
```

This outputs newline-delimited JSON (NDJSON):
```
["alice","bob"]
["bob","charlie"]
```

## Exercises

### Exercise 5.1: Build a Pipeline

Create a pipeline that:
1. Lists all parent relationships
2. Formats as `parent -> child`
3. Sorts alphabetically

<details>
<summary>Solution</summary>

```bash
perl parent.pl | jq -r '.[] | "\(.[0]) -> \(.[1])"' | sort
```

Output:
```
alice -> bob
bob -> charlie
```

</details>

### Exercise 5.2: Combine with AWK

Use AWK to count relationships per parent:

<details>
<summary>Solution</summary>

```bash
perl parent.pl | jq -r '.[] | .[0]' | sort | uniq -c
```

Or with AWK:
```bash
perl parent.pl | jq -r '.[][] ' | awk 'NR%2==1 {count[$1]++} END {for (p in count) print p, count[p]}'
```

</details>

## Summary

| Mode | Option | Use Case |
|------|--------|----------|
| Basic | `[]` | Library usage, custom callbacks |
| JSON | `[json_output]` | Direct execution, API endpoints |
| Pipeline | `[pipeline]` | Unix pipeline chains |

## Related Topics

- [Book 2: Bash Target](../book-02-bash-target/README.md) - Native shell pipelines
- [Book 7: Cross-Target Glue](../book-07-cross-target-glue/README.md) - Combining targets
