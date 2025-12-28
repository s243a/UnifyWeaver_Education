<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
-->

# Chapter 1: Introduction to the Perl Target

## Why Perl?

Perl excels at text processing, regex operations, and Unix pipeline integration. The Perl target generates code that:

- **Streams results** via callbacks, never materializing full result sets
- **Integrates with Unix pipelines** through JSON output modes
- **Leverages CPAN** for thousands of libraries via FFI bindings
- **Runs everywhere** Perl is installed (ubiquitous on Unix systems)

## Architecture Overview

The Perl target uses **Continuation-Passing Style (CPS)**, where every generated subroutine takes a callback function as its first argument:

```
┌─────────────────┐
│  Prolog Clause  │
└────────┬────────┘
         │ compile_predicate_to_perl/3
         ▼
┌─────────────────┐
│   Perl Sub      │
│ (CPS callback)  │
└────────┬────────┘
         │
         ▼
┌─────────────────┐
│  Results via    │
│  $callback->()  │
└─────────────────┘
```

### Why CPS?

CPS enables:
1. **Streaming**: Results are produced one at a time
2. **Composability**: Subroutines can be nested for joins
3. **Early termination**: Callbacks can stop iteration
4. **Memory efficiency**: No intermediate arrays needed

## Comparison with Other Targets

| Feature | Perl | Ruby | Bash |
|---------|------|------|------|
| Result mechanism | Callbacks | Blocks/yield | Print to stdout |
| Join style | Nested callbacks | Nested blocks | Temp files |
| Recursion | Semi-naive + memo | Semi-naive + memo | Memoized functions |
| JSON support | `use JSON;` | `require 'json'` | `jq` external |
| Best for | Text processing | Rails apps | Quick scripts |

## Compilation API

```prolog
compile_predicate_to_perl(+Pred/Arity, +Options, -PerlCode)
```

### Parameters

- `Pred/Arity`: The predicate to compile (e.g., `parent/2`)
- `Options`: List of compilation options
- `PerlCode`: Output string containing Perl code

### Options

| Option | Effect |
|--------|--------|
| `[]` | Basic compilation |
| `[json_output]` | Add `_json` wrapper with `encode_json` |
| `[pipeline]` | Add `run_pipeline` for stdin/stdout |

## Your First Compilation

```prolog
:- use_module('src/unifyweaver/targets/perl_target').

% Define a simple predicate
:- dynamic color/1.
color(red).
color(green).
color(blue).

% Compile it
?- compile_predicate_to_perl(color/1, [], Code),
   format('~s', [Code]).
```

Output:
```perl
#!/usr/bin/env perl
use strict;
use warnings;

sub color {
    my $callback = shift;
    my @facts = (
        ['red'],
        ['green'],
        ['blue']
    );
    foreach my $fact (@facts) {
        $callback->(@$fact);
    }
}
```

## Using Generated Code

Save the output to a file and use it:

```perl
#!/usr/bin/env perl
use strict;
use warnings;

# ... generated sub color { ... } ...

# Print all colors
color(sub {
    my ($c) = @_;
    print "Color: $c\n";
});

# Collect into array
my @colors;
color(sub { push @colors, $_[0]; });
print "Found " . scalar(@colors) . " colors\n";

# Early termination
my $found;
color(sub {
    my ($c) = @_;
    if ($c eq 'green') {
        $found = $c;
        return;  # Note: doesn't stop iteration in basic form
    }
});
```

## Next Steps

In the next chapter, we'll explore how facts and rules compile to Perl code, and how join conditions are generated for multi-goal rules.

→ [Chapter 2: Basic Compilation](02_basic_compilation.md)
