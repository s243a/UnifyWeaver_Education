<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Prolog Fundamentals - Questions

**Q&A companion to [02_prolog_fundamentals_impl.md](./02_prolog_fundamentals_impl.md)**

This document contains questions about Prolog fundamentals as used in UnifyWeaver.

---

<a id="b01c02-q-what-is-atom"></a>
## Q: What is a Prolog atom?

An atom is a named constant starting with lowercase or enclosed in single quotes:

```prolog
alice           % Simple atom
'Main.c'        % Quoted (special characters)
'has spaces'    % Quoted (spaces)
```

Check with: `atom(X)` returns true if X is an atom.

**Reference**: [Atoms](./02_prolog_fundamentals_impl.md#atoms)

---

<a id="b01c02-q-what-is-variable"></a>
## Q: What is a Prolog variable?

Variables are placeholders starting with uppercase or underscore:

```prolog
X           % Named variable
Parent      % Named variable
_           % Anonymous variable
_Ignored    % Named but conventionally ignored
```

The anonymous variable `_` matches anything without binding.

**Reference**: [Variables](./02_prolog_fundamentals_impl.md#variables)

---

<a id="b01c02-q-anonymous-variable"></a>
## Q: What is the anonymous variable in Prolog?

The underscore `_` matches any term without creating a binding:

```prolog
?- parent(alice, _).  % We don't care what the child is

?- foo(_, _).  % Two independent anonymous variables
```

Multiple `_` in the same term are independent (don't unify with each other).

**Reference**: [Variables](./02_prolog_fundamentals_impl.md#variables)

---

<a id="b01c02-q-complex-term"></a>
## Q: What is a complex term (structure)?

A functor with arguments: `functor(arg1, arg2, ...)`

```prolog
parent(alice, bob)        % functor: parent, arity: 2
file('main.c', 1024)      % functor: file, arity: 2
```

The **arity** is the number of arguments. Written as `functor/arity`.

**Reference**: [Complex Terms](./02_prolog_fundamentals_impl.md#complex-terms-structures)

---

<a id="b01c02-q-functor-arity"></a>
## Q: What does functor/arity notation mean?

It identifies a predicate by name and argument count:

| Notation | Meaning |
|----------|---------|
| `parent/2` | `parent` with 2 arguments |
| `edge/2` | `edge` with 2 arguments |
| `compile/3` | `compile` with 3 arguments |

**Important**: `foo/1` and `foo/2` are completely different predicates!

**Reference**: [Predicate Notation](./02_prolog_fundamentals_impl.md#predicate-notation)

---

<a id="b01c02-q-fact-vs-rule"></a>
## Q: What's the difference between a fact and a rule?

**Fact**: A term asserted as true (no body).
```prolog
parent(alice, bob).
```

**Rule**: Head is true if body is true.
```prolog
grandparent(X, Z) :- parent(X, Y), parent(Y, Z).
```

The `:-` operator means "if" or "is true when".

**Reference**: [Facts vs Rules](./02_prolog_fundamentals_impl.md#facts-vs-rules)

---

<a id="b01c02-q-unification-rules"></a>
## Q: What are the rules for Prolog unification?

1. **Atoms**: Unify only with themselves
2. **Variables**: Unify with any term (become bound)
3. **Numbers**: Unify only with equal numbers
4. **Structures**: Same functor, same arity, arguments unify

```prolog
?- foo(X, b) = foo(a, Y).
X = a, Y = b.
```

**Reference**: [Unification Algorithm](./02_prolog_fundamentals_impl.md#unification-algorithm)

---

<a id="b01c02-q-occurs-check"></a>
## Q: What is the occurs check in Prolog?

Standard Prolog skips the "occurs check" for performance, which can create cyclic terms:

```prolog
?- X = f(X).
X = f(f(f(...))).  % Infinite!
```

Use `unify_with_occurs_check/2` for safe unification:

```prolog
?- unify_with_occurs_check(X, f(X)).
false.  % Correctly fails
```

**Reference**: [Occurs Check](./02_prolog_fundamentals_impl.md#occurs-check)

---

<a id="b01c02-q-module-declaration"></a>
## Q: How do I declare a Prolog module?

```prolog
:- module(mymodule, [
    public_pred/2,      % Exported
    another_pred/1
]).

helper(X) :- ...        % Private (not exported)
public_pred(X, Y) :- ... % Public
```

**Reference**: [Module Declaration](./02_prolog_fundamentals_impl.md#module-declaration)

---

<a id="b01c02-q-use-module"></a>
## Q: How do I load a Prolog module?

```prolog
% Standard library
:- use_module(library(lists)).

% UnifyWeaver module (with alias)
:- use_module(unifyweaver(core/compiler_driver)).

% Specific predicates only
:- use_module(library(lists), [member/2, append/3]).
```

**Reference**: [Loading Modules](./02_prolog_fundamentals_impl.md#loading-modules)

---

<a id="b01c02-q-module-qualified"></a>
## Q: How do I call a predicate from a specific module?

Use the module prefix:

```prolog
?- lists:member(X, [1,2,3]).

?- mymodule:helper(X).  % Call private predicate
```

**Reference**: [Module-Qualified Calls](./02_prolog_fundamentals_impl.md#module-qualified-calls)

---

<a id="b01c02-q-dynamic-predicate"></a>
## Q: What is a dynamic predicate?

A predicate that can be modified at runtime:

```prolog
:- dynamic my_fact/2.

?- assertz(my_fact(a, b)).   % Add fact
?- retract(my_fact(a, b)).   % Remove fact
?- retractall(my_fact(_, _)). % Remove all
```

**Reference**: [Dynamic Predicates](./02_prolog_fundamentals_impl.md#dynamic-predicates)

---

<a id="b01c02-q-assertz-vs-asserta"></a>
## Q: What's the difference between assertz and asserta?

```prolog
assertz(fact).  % Add at END of database
asserta(fact).  % Add at BEGINNING of database
```

Order matters for clause selection during backtracking.

**Reference**: [Assert and Retract](./02_prolog_fundamentals_impl.md#assert-and-retract)

---

<a id="b01c02-q-findall"></a>
## Q: How do I collect all solutions in Prolog?

Use `findall/3`:

```prolog
?- findall(X, parent(alice, X), Children).
Children = [bob, carol, dave].

?- findall(X-Y, edge(X, Y), Edges).
Edges = [a-b, b-c, c-d].
```

**Reference**: [Findall Pattern](./02_prolog_fundamentals_impl.md#findall-pattern)

---

<a id="b01c02-q-file-write"></a>
## Q: How do I write to a file in Prolog?

```prolog
open('output.txt', write, Stream),
write(Stream, 'Hello World'),
nl(Stream),
close(Stream).
```

**Reference**: [Basic Read/Write](./02_prolog_fundamentals_impl.md#basic-readwrite)

---

<a id="b01c02-q-read-file"></a>
## Q: How do I read a file to a string in Prolog?

```prolog
read_file_to_string('file.txt', Content, []).
```

**Reference**: [Read Entire File](./02_prolog_fundamentals_impl.md#read-entire-file)

---

<a id="b01c02-q-safe-file-io"></a>
## Q: How do I safely handle files (ensure closing)?

Use `setup_call_cleanup/3`:

```prolog
setup_call_cleanup(
    open('file.txt', write, S),
    write(S, Content),
    close(S)
).
```

This ensures the file is closed even if an error occurs.

**Reference**: [Safe File Handling](./02_prolog_fundamentals_impl.md#safe-file-handling)

---

<a id="b01c02-q-format-string"></a>
## Q: What are the common format specifiers in Prolog?

| Specifier | Meaning |
|-----------|---------|
| `~w` | Write term |
| `~d` | Decimal integer |
| `~a` | Atom |
| `~n` | Newline |
| `~~` | Literal tilde |

```prolog
format('Name: ~w, Age: ~d~n', [alice, 30]).
```

**Reference**: [Format Strings](./02_prolog_fundamentals_impl.md#format-strings)

---

<a id="b01c02-q-discontiguous"></a>
## Q: What does the discontiguous directive do?

Allows predicate clauses to be non-adjacent in the file:

```prolog
:- discontiguous template/2.

template(header, '...').
% ... other code ...
template(footer, '...').  % OK with discontiguous
```

Without it, SWI-Prolog warns about non-contiguous clauses.

**Reference**: [Discontiguous Predicates](./02_prolog_fundamentals_impl.md#discontiguous-predicates)

---

<a id="b01c02-q-multifile"></a>
## Q: What does the multifile directive do?

Allows predicate clauses to span multiple files:

```prolog
:- multifile user:term_expansion/2.
```

Used for hook predicates that multiple modules contribute to.

**Reference**: [Multifile Predicates](./02_prolog_fundamentals_impl.md#multifile-predicates)

---

<a id="b01c02-q-initialization"></a>
## Q: How do I run code when a file loads?

Use the initialization directive:

```prolog
:- initialization(setup_defaults).

setup_defaults :-
    assertz(config(debug, false)).
```

**Reference**: [Initialization](./02_prolog_fundamentals_impl.md#initialization)

---

<a id="b01c02-q-constraint-pragma"></a>
## Q: How do I declare constraints in UnifyWeaver?

```prolog
:- constraint(ancestor/2, [unique, unordered]).
:- constraint(temporal/2, [unique, ordered]).
```

This guides how UnifyWeaver handles deduplication in generated code.

**Reference**: [UnifyWeaver Constraint Pragma](./02_prolog_fundamentals_impl.md#unifyweaver-constraint-pragma)

---

## Question Index

| ID | Topic |
|----|-------|
| [b01c02-q-what-is-atom](#b01c02-q-what-is-atom) | Atoms |
| [b01c02-q-what-is-variable](#b01c02-q-what-is-variable) | Variables |
| [b01c02-q-anonymous-variable](#b01c02-q-anonymous-variable) | Anonymous variable |
| [b01c02-q-complex-term](#b01c02-q-complex-term) | Complex terms |
| [b01c02-q-functor-arity](#b01c02-q-functor-arity) | Functor/arity notation |
| [b01c02-q-fact-vs-rule](#b01c02-q-fact-vs-rule) | Facts vs rules |
| [b01c02-q-unification-rules](#b01c02-q-unification-rules) | Unification rules |
| [b01c02-q-occurs-check](#b01c02-q-occurs-check) | Occurs check |
| [b01c02-q-module-declaration](#b01c02-q-module-declaration) | Module declaration |
| [b01c02-q-use-module](#b01c02-q-use-module) | Loading modules |
| [b01c02-q-module-qualified](#b01c02-q-module-qualified) | Module-qualified calls |
| [b01c02-q-dynamic-predicate](#b01c02-q-dynamic-predicate) | Dynamic predicates |
| [b01c02-q-assertz-vs-asserta](#b01c02-q-assertz-vs-asserta) | assertz vs asserta |
| [b01c02-q-findall](#b01c02-q-findall) | Findall pattern |
| [b01c02-q-file-write](#b01c02-q-file-write) | File writing |
| [b01c02-q-read-file](#b01c02-q-read-file) | File reading |
| [b01c02-q-safe-file-io](#b01c02-q-safe-file-io) | Safe file handling |
| [b01c02-q-format-string](#b01c02-q-format-string) | Format specifiers |
| [b01c02-q-discontiguous](#b01c02-q-discontiguous) | Discontiguous directive |
| [b01c02-q-multifile](#b01c02-q-multifile) | Multifile directive |
| [b01c02-q-initialization](#b01c02-q-initialization) | Initialization |
| [b01c02-q-constraint-pragma](#b01c02-q-constraint-pragma) | Constraint pragma |
