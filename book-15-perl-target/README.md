<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Book 15: Perl Target

**Continuation-Passing Style for Unix Pipelines**

*Part of the [UnifyWeaver Education Series](../README.md)*

This book covers how to use UnifyWeaver to compile Prolog predicates into Perl subroutines. The Perl target generates callback-based code ideal for streaming data processing and Unix pipeline integration.

## What You'll Learn

By completing this book, you will be able to:

- Compile Prolog predicates to Perl subroutines using continuation-passing style (CPS)
- Understand how joins are compiled to nested callbacks with guard conditions
- Use tail recursion optimization for accumulator-style predicates
- Apply memoization for linear recursive predicates (fibonacci-style)
- Generate aggregations (count, sum, min, max, avg) from `aggregate_all/3`
- Create JSON output wrappers for pipeline integration
- Use the 150+ FFI bindings for Perl standard library functions

## Prerequisites

**Required:**
- [Book 1: Foundations](../book-01-foundations/README.md)

**Recommended:**
- [Book 2: Bash Target](../book-02-bash-target/README.md) - stream compilation concepts
- Basic Perl knowledge (subroutines, references, callbacks)

**Technical:**
- Perl 5.10+ installed
- UnifyWeaver with Perl target support

## Learning Path

**1. Introduction** (`01_introduction.md`)
- Why use the Perl target?
- CPS architecture overview
- Comparison with Bash and Ruby targets

**2. Basic Compilation** (`02_basic_compilation.md`)
- Compiling facts to arrays
- Compiling rules with callbacks
- Understanding join conditions

**3. Recursion Patterns** (`03_recursion_patterns.md`)
- Semi-naive iteration for transitive closure
- Tail recursion optimization
- Linear recursion with memoization

**4. Aggregations** (`04_aggregations.md`)
- Using `aggregate_all/3`
- Supported templates: count, sum, min, max, avg
- Custom aggregation patterns

**5. JSON and Pipeline Modes** (`05_json_pipeline.md`)
- The `json_output` option
- The `pipeline` option
- Unix pipeline integration

**6. FFI Bindings** (`06_ffi_bindings.md`)
- String operations
- Array and hash operations
- I/O and file operations
- Math functions

## Quick Start

```prolog
% Load the Perl target
:- use_module('src/unifyweaver/targets/perl_target').

% Define facts
:- dynamic parent/2.
parent(alice, bob).
parent(bob, charlie).

% Compile to Perl
?- compile_predicate_to_perl(parent/2, [json_output], Code),
   format('~s', [Code]).
```

Output:
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

sub parent_json {
    my @results;
    parent(sub { push @results, [$_[1], $_[2]]; });
    print encode_json(\@results);
}

parent_json() unless caller;
```

## Key Concepts

### Continuation-Passing Style

Every generated subroutine takes a callback as its first argument:

```perl
sub predicate {
    my $callback = shift;
    # ... compute results ...
    $callback->(@result);  # Pass results to callback
}

# Usage
predicate(sub { my @args = @_; print "@args\n"; });
```

### Join Compilation

Multi-goal rules compile to nested callbacks with guard conditions:

```prolog
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

```perl
sub grandparent {
    my $callback = shift;
    parent(sub {
        my ($p0, $p1) = @_;
        parent(sub {
            my ($p2, $p3) = @_;
            return unless $p2 eq $p1;  # Join condition
            $callback->($p0, $p3);
        });
    });
}
```

### Recursion Optimization

| Pattern | Optimization | Example |
|---------|--------------|---------|
| Transitive closure | Semi-naive with `%seen` | `ancestor/2` |
| Accumulator | While loop | `factorial/3` |
| Multiple calls | Memoization with `%memo` | `fib/2` |

## Related Books

- [Book 16: Ruby Target](../book-16-ruby-target/README.md) - Block-based CPS in Ruby
- [Book 7: Cross-Target Glue](../book-07-cross-target-glue/README.md) - Orchestrating multiple targets

## License

This educational content is licensed under CC BY 4.0.
Code examples are dual-licensed under MIT OR Apache-2.0.
