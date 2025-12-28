# Perl Target

The Perl target (`target(perl)`) generates Perl subroutines from Prolog predicates using continuation-passing style (CPS). It provides streaming evaluation with callback-based output, suitable for Unix pipeline integration.

## Overview

Perl programs use callback functions to stream results, avoiding the need to materialize complete result sets in memory. This makes them efficient for processing large data sets in a pipeline context.

```prolog
% Compile to Perl
?- compile_predicate_to_perl(my_predicate/2, [], PerlCode).
```

## Features

### Core Capabilities

| Feature | Status | Description |
|---------|--------|-------------|
| Facts | ✅ | Direct fact compilation to arrays |
| Single Rules | ✅ | Body-to-code translation with CPS |
| Multiple Rules (OR) | ✅ | Sequential clause evaluation |
| Recursion | ✅ | Semi-naive iteration with deduplication |
| Joins | ✅ | Inner joins with proper variable binding |

## Continuation-Passing Style

All predicates generate subroutines that take a callback as the first argument:

```perl
sub parent {
    my $callback = shift;
    my @facts = (
        ["alice", "bob"],
        ["bob", "charlie"]
    );
    foreach my $fact (@facts) {
        $callback->(@$fact);
    }
}

# Usage: stream all parent relationships
parent(sub { my ($x, $y) = @_; print "$x -> $y\n"; });
```

## Join Handling

When rules contain multiple goals sharing variables, the compiler generates proper join conditions:

```prolog
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

Generates:

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

Key implementation details:
- **Unique parameter names** (`$p0`, `$p1`, etc.) avoid variable shadowing in nested callbacks
- **Join conditions** (`return unless`) enforce variable equality between goals
- **Immediate projection** returns only the head variables to the callback

## Recursive Predicates

Recursive predicates use semi-naive iteration to ensure termination and avoid duplicate outputs:

```prolog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
```

Generates:

```perl
sub ancestor {
    my $callback = shift;
    my @delta;
    my %seen;

    # Base cases - seed the worklist
    parent(sub {
        my ($p0, $p1) = @_;
        my $key = join('\0', $p0, $p1);
        unless ($seen{$key}++) { push @delta, [$p0, $p1]; }
    });

    # Semi-naive iteration
    while (@delta) {
        my $item = shift @delta;
        $callback->(@$item);  # Output current result

        # Expand: find new results via recursive clause
        parent(sub {
            my ($p2, $p3) = @_;
            return unless $p3 eq $item->[0];  # Join on shared variable
            my $key = join('\0', $p2, $item->[1]);
            unless ($seen{$key}++) { push @delta, [$p2, $item->[1]]; }
        });
    }
}
```

Key features:
- **`@delta` worklist**: Contains tuples to process; new results are appended here
- **`%seen` hash**: Prevents duplicate outputs using null-separated key strings
- **Fixpoint computation**: Loop terminates when no new tuples are discovered

## Performance Characteristics

- **Memory**: O(n) where n is the number of unique output tuples (required for deduplication)
- **Time**: Nested-loop joins; adequate for modest data sets
- **Streaming**: Results are emitted immediately via callback as they're discovered

## Limitations

- No aggregation support yet
- No window functions
- Inner joins only (no outer join patterns)
- String-based comparison (`eq`) - no numeric comparison

## Usage Example

```perl
#!/usr/bin/env perl
use strict;
use warnings;

# ... generated code ...

# Print all ancestors
print "Ancestors:\n";
ancestor(sub {
    my ($x, $y) = @_;
    print "  $x is ancestor of $y\n";
});

# Count results
my $count = 0;
ancestor(sub { $count++; });
print "Total: $count\n";
```

## Roadmap

1. Add aggregation support (`count`, `sum`, `min`, `max`)
2. Implement outer join patterns
3. Add numeric comparison operators
4. Support constant filtering in goals
