<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Book 12: PowerShell Target - Implementation Documentation

Technical deep-dive documentation for PowerShell code generation.

## Available Documentation

| Chapter | Implementation | Questions |
|---------|----------------|-----------|
| Chapter 2: Facts and Rules | [02_facts_rules_impl.md](./02_facts_rules_impl.md) | [02_facts_rules_questions.md](./02_facts_rules_questions.md) |

## Question Count

- Chapter 2: 12 questions

## Topics Covered

### Chapter 2: Facts and Rules
- `compile_to_powershell/3` API
- Unary fact compilation to string arrays
- Binary fact compilation to PSCustomObject arrays
- Why PSCustomObject (typed properties, pipeline, serialization)
- `compile_facts_to_powershell/3` class-based export
- Rule compilation with nested loop joins
- Negation via hashtable exclusion
- Pure PowerShell vs BaaS compilation modes
- Pipeline integration with ValueFromPipeline
- Query modes for binary facts
- Join performance (O(n*m))
- Index pre-computation optimization

## Source Files

- `src/unifyweaver/targets/powershell_target.pl`

## Related Books

- [Book 3: C# Target](../book-03-csharp-target/) - .NET integration
- [Book 2: Bash Target](../book-02-bash-target/) - BaaS mode backend
