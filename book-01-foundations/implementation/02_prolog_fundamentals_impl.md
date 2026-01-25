<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2 Implementation: Prolog Fundamentals

**Detailed function documentation for RAG systems**

This document provides implementation details for Prolog fundamentals as used in UnifyWeaver.

---

## Table of Contents

1. [Term Types](#term-types)
2. [Predicate Notation](#predicate-notation)
3. [Unification Algorithm](#unification-algorithm)
4. [Module System](#module-system)
5. [Dynamic Predicates](#dynamic-predicates)
6. [File I/O Patterns](#file-io-patterns)
7. [Common Directives](#common-directives)

---

## Term Types

Prolog has four fundamental term types.

### Atoms

**Definition**: Named constants starting with lowercase or enclosed in single quotes.

```prolog
% Simple atoms
alice
bob
parent

% Quoted atoms (for special characters or uppercase start)
'Main.c'
'Starts with capital'
'has spaces'
```

**Checking**:
```prolog
?- atom(alice).
true.

?- atom('Hello World').
true.

?- atom(X).
false.  % Variables are not atoms
```

### Numbers

**Definition**: Integer or floating-point values.

```prolog
% Integers
42
-100
0

% Floats
3.14
-2.5
```

**Checking**:
```prolog
?- number(42).
true.

?- integer(42).
true.

?- float(3.14).
true.
```

### Variables

**Definition**: Placeholders starting with uppercase letter or underscore.

```prolog
X           % Named variable
Parent      % Named variable
_           % Anonymous variable (matches anything, no binding)
_Ignored    % Named but conventionally ignored
```

**Anonymous Variable**:
```prolog
% We don't care about the second argument
?- parent(alice, _).

% Multiple _ are independent (don't unify with each other)
?- foo(_, _).  % Two different anonymous variables
```

**Checking**:
```prolog
?- var(X).
true.  % X is unbound

?- X = 5, var(X).
false.  % X is now bound to 5
```

### Complex Terms (Structures)

**Definition**: Functor with arguments: `functor(arg1, arg2, ...)`.

```prolog
parent(alice, bob)        % functor: parent, arity: 2
file('main.c', 1024)      % functor: file, arity: 2
point(X, Y, Z)            % functor: point, arity: 3 (with variables)
```

**Arity**: Number of arguments. Written as `functor/arity`.

```prolog
parent/2    % parent with 2 arguments
file/2      % file with 2 arguments
edge/2      % edge with 2 arguments
```

**Decomposition**:
```prolog
?- T = parent(alice, bob), functor(T, F, A).
F = parent,
A = 2.

?- T = parent(alice, bob), T =.. [F|Args].
F = parent,
Args = [alice, bob].
```

---

## Predicate Notation

### Functor/Arity Convention

Predicates are identified by name and arity:

| Notation | Meaning |
|----------|---------|
| `parent/2` | `parent` with 2 arguments |
| `edge/2` | `edge` with 2 arguments |
| `compile/3` | `compile` with 3 arguments |

**Why arity matters**: `foo/1` and `foo/2` are completely different predicates.

```prolog
foo(X) :- ...       % foo/1
foo(X, Y) :- ...    % foo/2 - different predicate!
```

### Facts vs Rules

**Fact**: A term asserted as true (no body).

```prolog
parent(alice, bob).
edge(a, b).
```

**Rule**: Head true if body is true.

```prolog
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
%    head         :-           body
```

**Reading rules**: `:-` means "if" or "is true when".

```
grandparent(X, Z) is true if
    parent(X, Y) is true AND
    parent(Y, Z) is true
```

---

## Unification Algorithm

Unification is Prolog's core matching mechanism.

### Rules

1. **Atoms**: Unify only with themselves
   ```prolog
   ?- alice = alice.  % true
   ?- alice = bob.    % false
   ```

2. **Variables**: Unify with any term (become bound)
   ```prolog
   ?- X = alice.      % X = alice
   ?- X = Y.          % X and Y share (both unbound)
   ```

3. **Numbers**: Unify only with equal numbers
   ```prolog
   ?- 42 = 42.        % true
   ?- 42 = 43.        % false
   ```

4. **Structures**: Same functor, same arity, arguments unify
   ```prolog
   ?- foo(X, b) = foo(a, Y).
   X = a, Y = b.

   ?- foo(X) = bar(X).
   false.  % Different functors
   ```

### Occurs Check

Standard Prolog skips the "occurs check" for performance. This can create cyclic terms:

```prolog
?- X = f(X).
X = f(f(f(f(...)))).  % Infinite structure!
```

Use `unify_with_occurs_check/2` for safe unification:

```prolog
?- unify_with_occurs_check(X, f(X)).
false.  % Correctly fails
```

### Unification in Practice

When you query `parent(alice, X)`:

1. Prolog finds fact `parent(alice, bob)`
2. Unifies `parent(alice, X)` with `parent(alice, bob)`
3. `alice = alice` ✓
4. `X = bob` ✓ (X becomes bound)
5. Returns `X = bob`

---

## Module System

### Module Declaration

```prolog
% At top of file: mymodule.pl
:- module(mymodule, [
    public_pred/2,      % Exported predicates
    another_pred/1
]).

% Private predicate (not exported)
helper(X) :- ...

% Public predicate
public_pred(X, Y) :- helper(X), ...
```

### Loading Modules

```prolog
% Load standard library
:- use_module(library(lists)).

% Load with alias (UnifyWeaver pattern)
:- use_module(unifyweaver(core/compiler_driver)).

% Load specific predicates only
:- use_module(library(lists), [member/2, append/3]).
```

### Module-Qualified Calls

Call a predicate from a specific module:

```prolog
% Explicit module prefix
?- lists:member(X, [1,2,3]).

% Call private predicate (bypass export)
?- mymodule:helper(X).
```

### Library Aliases

UnifyWeaver uses `file_search_path/2` for aliases:

```prolog
% In init.pl
file_search_path(unifyweaver, 'src/unifyweaver').

% Usage
use_module(unifyweaver(core/template_system))
% Resolves to: src/unifyweaver/core/template_system.pl
```

---

## Dynamic Predicates

### Declaration

Dynamic predicates can be modified at runtime:

```prolog
:- dynamic my_fact/2.
:- dynamic cached_result/3.
```

### Assert and Retract

```prolog
% Add facts
?- assertz(my_fact(a, b)).     % Add at end
?- asserta(my_fact(x, y)).     % Add at beginning

% Remove facts
?- retract(my_fact(a, b)).     % Remove first matching
?- retractall(my_fact(_, _)).  % Remove all matching

% Modify (retract + assert)
?- retract(counter(N)), N1 is N + 1, assertz(counter(N1)).
```

### Findall Pattern

Collect all solutions:

```prolog
?- findall(X, parent(alice, X), Children).
Children = [bob, carol, dave].

?- findall(X-Y, edge(X, Y), Edges).
Edges = [a-b, b-c, c-d].
```

---

## File I/O Patterns

### Basic Read/Write

```prolog
% Write to file
open('output.txt', write, Stream),
write(Stream, 'Hello World'),
nl(Stream),  % Newline
close(Stream).

% Read from file
open('input.txt', read, Stream),
read_line_to_string(Stream, Line),
close(Stream).
```

### Read Entire File

```prolog
% To string
read_file_to_string('file.txt', Content, []).

% To list of lines
read_file_to_string('file.txt', Content, []),
split_string(Content, "\n", "", Lines).
```

### Safe File Handling

Use `setup_call_cleanup/3` to ensure cleanup:

```prolog
setup_call_cleanup(
    open('file.txt', write, S),
    (
        write(S, Line1),
        nl(S),
        write(S, Line2)
    ),
    close(S)
).
```

### Format Strings

```prolog
% format/2 - write to current output
format('Name: ~w, Age: ~d~n', [alice, 30]).

% format/3 - write to stream
format(Stream, '~w:~w~n', [Key, Value]).
```

**Format specifiers**:

| Specifier | Meaning |
|-----------|---------|
| `~w` | Write term |
| `~d` | Decimal integer |
| `~f` | Float |
| `~a` | Atom |
| `~s` | String (list of codes) |
| `~n` | Newline |
| `~~` | Literal tilde |

### Directory Operations

```prolog
% Create directory (with parents)
make_directory_path('output/scripts').

% Check existence
exists_file('script.sh').
exists_directory('output').

% List directory
directory_files('src', Files).
```

---

## Common Directives

### Module Loading

```prolog
:- use_module(library(lists)).
:- use_module(unifyweaver(core/template_system)).
```

### Dynamic Declaration

```prolog
:- dynamic my_predicate/2.
:- dynamic cached/3.
```

### Discontiguous Predicates

Allow predicate clauses to be non-contiguous in file:

```prolog
:- discontiguous template/2.

template(header, '...').
% ... other code ...
template(footer, '...').  % OK with discontiguous
```

### Multifile Predicates

Allow predicate clauses across multiple files:

```prolog
:- multifile user:term_expansion/2.

user:term_expansion(In, Out) :- ...
```

### Initialization

Run code when file loads:

```prolog
:- initialization(setup_defaults).

setup_defaults :-
    assertz(config(debug, false)),
    assertz(config(verbose, true)).
```

### UnifyWeaver Constraint Pragma

```prolog
:- constraint(ancestor/2, [unique, unordered]).
:- constraint(temporal/2, [unique, ordered]).
```

This directive is processed by `term_expansion/2` in `constraint_analyzer.pl`.

---

## Source Files

- SWI-Prolog documentation: https://www.swi-prolog.org/pldoc/
- `src/unifyweaver/core/constraint_analyzer.pl` - Constraint pragma handling

## See Also

- Chapter 2: Prolog Fundamentals (tutorial)
- Chapter 3: UnifyWeaver Architecture (how Prolog is compiled)
- SWI-Prolog Reference Manual
