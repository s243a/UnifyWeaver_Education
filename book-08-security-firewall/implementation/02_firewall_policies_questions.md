<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Firewall Policies - Questions

Q&A companion for [02_firewall_policies_impl.md](./02_firewall_policies_impl.md).

---

## Question Index

1. [What does allow/1 do?](#b08c02-q-allow)
2. [What does deny/1 do?](#b08c02-q-deny)
3. [What does require/1 do?](#b08c02-q-require)
4. [What does implies/2 do?](#b08c02-q-implies)
5. [What are the firewall modes?](#b08c02-q-modes)
6. [How do I load a firewall policy?](#b08c02-q-load)
7. [How do I check if an operation is allowed?](#b08c02-q-check)
8. [How do I debug firewall policies?](#b08c02-q-debug)
9. [How do I create a target whitelist?](#b08c02-q-whitelist)
10. [How do environment-based policies work?](#b08c02-q-environment)
11. [Where are firewall checks integrated?](#b08c02-q-integration)
12. [How do I compose AND/OR policies?](#b08c02-q-composition)

---

## Questions and Answers

### <a id="b08c02-q-allow"></a>Q1: What does allow/1 do?

**Answer**: Explicitly permits an operation type:

```prolog
:- allow(target(bash)).
:- allow(target(go)).
:- allow(import_module(library(lists))).
```

Supports targets, dialects, modules, and compilation.

**See**: [allow/1](./02_firewall_policies_impl.md#allow1)

---

### <a id="b08c02-q-deny"></a>Q2: What does deny/1 do?

**Answer**: Explicitly blocks an operation type:

```prolog
:- deny(target(python)).
:- deny(compile(native)).
:- deny(import_module(library(http/_))).
```

Use `deny(target(_))` to deny all unlisted targets.

**See**: [deny/1](./02_firewall_policies_impl.md#deny1)

---

### <a id="b08c02-q-require"></a>Q3: What does require/1 do?

**Answer**: Makes a condition mandatory:

```prolog
:- require(validation(passed)).
:- require(code_review(approved)).
```

Operations fail if required conditions aren't met.

**See**: [require/1](./02_firewall_policies_impl.md#require1)

---

### <a id="b08c02-q-implies"></a>Q4: What does implies/2 do?

**Answer**: Creates conditional policy rules:

```prolog
:- implies(compile(native), code_review(required)).
:- implies(environment(production), validation(strict)).
```

If condition holds, requirement must also hold.

**See**: [implies/2](./02_firewall_policies_impl.md#implies2)

---

### <a id="b08c02-q-modes"></a>Q5: What are the firewall modes?

**Answer**:

| Mode | Behavior |
|------|----------|
| `guidance` | Warn but allow (default) |
| `enforce` | Block violations |
| `audit` | Allow but log everything |

```prolog
:- firewall_mode(enforce).
```

**See**: [Firewall Modes](./02_firewall_policies_impl.md#firewall-modes)

---

### <a id="b08c02-q-load"></a>Q6: How do I load a firewall policy?

**Answer**: Use `load_firewall_policy/1`:

```prolog
:- load_firewall_policy('.firewall').
:- load_firewall_policy('/etc/unifyweaver/production.firewall').
```

**See**: [load_firewall_policy/1](./02_firewall_policies_impl.md#load_firewall_policy1)

---

### <a id="b08c02-q-check"></a>Q7: How do I check if an operation is allowed?

**Answer**: Use `firewall_allows/1`:

```prolog
?- firewall_allows(target(python)).
false.

?- firewall_allows(target(go)).
true.
```

**See**: [firewall_allows/1](./02_firewall_policies_impl.md#firewall_allows1)

---

### <a id="b08c02-q-debug"></a>Q8: How do I debug firewall policies?

**Answer**: Three debugging tools:

```prolog
% Enable trace
:- set_firewall_flag(trace, true).

% List all rules
?- list_firewall_rules.

% Dry run
?- firewall_dry_run([check(target(python))], Results).
```

**See**: [Debugging Tools](./02_firewall_policies_impl.md#debugging-tools)

---

### <a id="b08c02-q-whitelist"></a>Q9: How do I create a target whitelist?

**Answer**: Allow specific, deny all others:

```prolog
:- allow(target(bash)).
:- allow(target(go)).
:- allow(target(rust)).
:- deny(target(_)).  % Deny all unlisted
```

**See**: [Target Whitelist](./02_firewall_policies_impl.md#target-whitelist)

---

### <a id="b08c02-q-environment"></a>Q10: How do environment-based policies work?

**Answer**: Use conditional rules:

```prolog
% Development: permissive
:- allow(target(_)) :- environment(development).

% Production: strict
:- allow(target(go)) :- environment(production).
:- deny(target(python)) :- environment(production).
```

**See**: [Environment-Based](./02_firewall_policies_impl.md#environment-based)

---

### <a id="b08c02-q-integration"></a>Q11: Where are firewall checks integrated?

**Answer**: Multiple points in the pipeline:

| Point | Check |
|-------|-------|
| Target selection | `firewall_allows(target(T))` |
| Dialect selection | `firewall_allows(prolog_target(D))` |
| Module imports | `firewall_allows(import_module(M))` |
| Compilation | `firewall_allows(compile(native))` |

**See**: [Integration Points](./02_firewall_policies_impl.md#integration-points)

---

### <a id="b08c02-q-composition"></a>Q12: How do I compose AND/OR policies?

**Answer**:

**AND** (both required):
```prolog
:- allow(prolog_target(gnu)) :-
    environment(production),
    code_review(passed).
```

**OR** (either sufficient):
```prolog
:- allow(compile(native)) :- environment(production).
:- allow(compile(native)) :- explicit_permission(admin).
```

**See**: [Policy Composition](./02_firewall_policies_impl.md#policy-composition)

---

## Summary

Firewall policies provide:
- Declarative security rules with `allow/1`, `deny/1`, `require/1`
- Conditional policies with `implies/2`
- Three enforcement modes: guidance, enforce, audit
- Policy composition with AND, OR, NOT
- Integration at target, dialect, module, and compilation points
- Debugging via trace, listing, and dry run
