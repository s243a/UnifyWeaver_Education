<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book 8: Security & Firewall - Implementation Documentation

Technical deep-dive documentation for declarative security policies.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 2: Firewall Policies | [02_firewall_policies_impl.md](./02_firewall_policies_impl.md) | [02_firewall_policies_questions.md](./02_firewall_policies_questions.md) |

## Question Count

- Chapter 2: 12 questions

## Topics Covered

### Chapter 2: Firewall Policies
- `allow/1`, `deny/1`, `require/1` policy predicates
- `implies/2` conditional policies
- Firewall modes: guidance, enforce, audit
- `load_firewall_policy/1` loading
- `firewall_allows/1` runtime checks
- `firewall_explain_denial/2` debugging
- Policy composition (AND, OR, NOT)
- Target/module whitelisting patterns
- Environment-based policies
- Integration points (target, dialect, module, compile)
- Debugging with trace, listing, dry run

## Source Files

- `src/unifyweaver/security/firewall.pl`

## Related Books

- [Book 11: Prolog Target](../book-11-prolog-target/) - Dialect security
- [Book 1: Foundations](../book-01-foundations/) - Core architecture
