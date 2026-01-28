<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Firewall Policies - Implementation Details

This document provides function-level documentation for the declarative security policy system.

**Source**: `src/unifyweaver/security/firewall.pl`

---

## Overview: Policy Predicates

| Predicate | Purpose |
|-----------|---------|
| `allow/1` | Explicitly permit an operation |
| `deny/1` | Explicitly block an operation |
| `require/1` | Make an operation mandatory |
| `implies/2` | Conditional policy rules |

---

## allow/1

Explicitly permits an operation type.

### Signature

```prolog
:- allow(+Operation)
```

### Operation Types

| Operation | Description |
|-----------|-------------|
| `target(T)` | Allow compilation to target T |
| `prolog_target(D)` | Allow Prolog dialect D |
| `import_module(M)` | Allow module import |
| `compile(native)` | Allow native compilation |

### Example

```prolog
:- allow(target(bash)).
:- allow(target(go)).
:- allow(prolog_target(swi)).
:- allow(import_module(library(lists))).
```

---

## deny/1

Explicitly blocks an operation type.

### Signature

```prolog
:- deny(+Operation)
```

### Example

```prolog
:- deny(target(python)).
:- deny(compile(native)).
:- deny(import_module(library(http/_))).
:- deny(import_module(library(socket))).
```

### Wildcard Denial

```prolog
% Allow specific, deny all others
:- allow(target(bash)).
:- allow(target(go)).
:- deny(target(_)).  % Deny all unlisted targets
```

---

## require/1

Makes an operation mandatory.

### Signature

```prolog
:- require(+Condition)
```

### Example

```prolog
:- require(validation(passed)).
:- require(prolog_target(gnu)).
:- require(code_review(approved)).
```

---

## implies/2

Creates conditional policy rules.

### Signature

```prolog
:- implies(+Condition, +Requirement)
```

### Example

```prolog
% If using GNU Prolog, disable compilation
:- implies(prolog_target(gnu), compile(false)).

% If compiling native, require code review
:- implies(compile(native), code_review(required)).

% Production requires validation
:- implies(environment(production), validation(strict)).
```

---

## Firewall Modes

### firewall_mode/1

Sets the enforcement behavior.

```prolog
:- firewall_mode(+Mode)
```

| Mode | Behavior |
|------|----------|
| `guidance` | Warn but allow (default) |
| `enforce` | Block violations |
| `audit` | Allow but log everything |

### Guidance Mode

```prolog
:- firewall_mode(guidance).
:- guidance(target(python), 'Python not recommended for production').
```

Output:
```
[Firewall] GUIDANCE: target(python)
  Reason: Python not recommended for production
  Action: Proceeding with caution
```

### Enforce Mode

```prolog
:- firewall_mode(enforce).
:- deny(target(python)).
```

Output:
```
[Firewall] DENIED: target(python)
  Reason: Blocked by security policy
  ERROR: Operation cannot proceed
```

### Audit Mode

```prolog
:- firewall_mode(audit).
:- audit_all.
```

Output:
```
[Firewall] AUDIT: target(python) - ALLOWED
[Firewall] AUDIT: compile(native) - ALLOWED
```

---

## Policy Composition

### AND Composition

```prolog
:- allow(prolog_target(gnu)) :-
    environment(production),
    code_review(passed).
```

### OR Composition

```prolog
:- allow(compile(native)) :- environment(production).
:- allow(compile(native)) :- explicit_permission(admin).
```

### NOT Composition

```prolog
:- deny(network_access) :- \+ environment(development).
```

---

## load_firewall_policy/1

Loads policies from a file.

### Signature

```prolog
load_firewall_policy(+FilePath)
```

### Example

```prolog
:- load_firewall_policy('.firewall').
:- load_firewall_policy('/etc/unifyweaver/production.firewall').
```

---

## firewall_allows/1

Checks if an operation is permitted.

### Signature

```prolog
firewall_allows(+Operation)
```

### Example

```prolog
?- firewall_allows(target(python)).
false.

?- firewall_allows(target(go)).
true.
```

---

## firewall_explain_denial/2

Explains why an operation was denied.

### Signature

```prolog
firewall_explain_denial(+Operation, -Reason)
```

### Example

```prolog
?- firewall_explain_denial(target(python), Reason).
Reason = 'Explicitly denied by policy rule 4'.
```

---

## Integration Points

### 1. Target Selection

```prolog
generate_code(Predicates, Target, Code) :-
    (firewall_allows(target(Target))
    ->  true
    ;   throw(error(firewall_denied(target(Target)),
                   'Target blocked by policy'))
    ),
    % ... continue generation
```

### 2. Dialect Selection

```prolog
generate_prolog_script(Predicates, Options, Code) :-
    option(dialect(Dialect), Options, swi),
    (firewall_allows(prolog_target(Dialect))
    ->  true
    ;   throw(error(firewall_denied(prolog_target(Dialect)),
                   'Dialect blocked by policy'))
    ).
```

### 3. Module Imports

```prolog
add_import(Module, Imports, FinalImports) :-
    (firewall_allows(import_module(Module))
    ->  FinalImports = [Module|Imports]
    ;   format('[Firewall] Module ~w blocked~n', [Module]),
        FinalImports = Imports
    ).
```

---

## Debugging Tools

### set_firewall_flag/2

Enables trace mode:

```prolog
:- set_firewall_flag(trace, true).
```

Output:
```
[Firewall] Checking: target(python)
[Firewall] Rule 1: allow(target(bash)) - No match
[Firewall] Rule 2: allow(target(go)) - No match
[Firewall] Rule 3: deny(target(python)) - MATCH
[Firewall] RESULT: DENIED
```

### list_firewall_rules/0

Lists all active policies:

```prolog
?- list_firewall_rules.
1. allow(target(bash))
2. allow(target(go))
3. deny(target(python))
4. deny(compile(native))
```

### firewall_dry_run/2

Tests policies without executing:

```prolog
?- firewall_dry_run([
    check(target(python)),
    check(target(go)),
    check(compile(native))
], Results).
Results = [
    denied(target(python)),
    allowed(target(go)),
    denied(compile(native))
].
```

---

## Common Policy Patterns

### Target Whitelist

```prolog
:- allow(target(bash)).
:- allow(target(go)).
:- allow(target(rust)).
:- deny(target(_)).
```

### Environment-Based

```prolog
:- allow(target(_)) :- environment(development).
:- allow(target(go)) :- environment(production).
:- deny(target(python)) :- environment(production).
```

### Module Whitelist

```prolog
:- allow(import_module(library(lists))).
:- allow(import_module(library(filesex))).
:- deny(import_module(library(http/_))).
:- deny(import_module(library(socket))).
:- deny(import_module(_)).
```

---

## Related Documentation

- [Book 8 Chapter 1: Introduction](../01_introduction.md)
- [Book 8 Chapter 3: Lifecycle Hooks](../03_lifecycle_hooks.md)
- [Firewall Source](../../../../src/unifyweaver/security/firewall.pl)
