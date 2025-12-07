<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Book 1: Foundations

**Architecture, Prolog Basics, and the Preference System**

*Part of the [UnifyWeaver Education Series](../README.md)*

This book covers the fundamental concepts every UnifyWeaver user needs to understand before working with any target language. Start here if you're new to UnifyWeaver.

## Prerequisites

- Basic command-line knowledge
- Programming experience (any language)
- SWI-Prolog 8.0+ installed

## What You'll Learn

By completing this book, you will understand:

- Core Prolog concepts (facts, rules, queries, unification)
- UnifyWeaver's compiler architecture
- The preference system for customizing behavior
- How targets are selected and configured
- The plugin system
- Basic compilation workflow

## Chapter Overview (Planned)

### Part 1: Prolog Fundamentals

**Chapter 1: Introduction to UnifyWeaver**
- What is UnifyWeaver?
- Why declarative-to-imperative compilation?
- Overview of available targets
- Installation and setup

**Chapter 2: Prolog Basics**
- Facts, rules, and queries
- Unification and pattern matching
- Lists and recursion basics
- The cut operator

**Chapter 3: Advanced Prolog**
- Meta-predicates
- DCGs (Definite Clause Grammars)
- Constraint Logic Programming
- Module system

### Part 2: UnifyWeaver Architecture

**Chapter 4: Compiler Pipeline**
- Source analysis
- Intermediate representation
- Target selection
- Code generation

**Chapter 5: The Preference System**
- Default behaviors
- User preferences
- Target-specific options
- Configuration files

**Chapter 6: Plugin Architecture**
- Data source plugins
- Target plugins
- Custom extensions
- Plugin lifecycle

### Part 3: Getting Started

**Chapter 7: Your First Compilation**
- Writing a simple predicate
- Choosing a target
- Running the compiler
- Understanding the output

**Chapter 8: Testing and Debugging**
- Test runner basics
- Debugging compiled code
- Common issues and solutions

## Content Status

This book is planned but chapters are not yet written. Content is currently embedded in [Book 2: Bash Target](../book-2-bash-target/README.md) chapters 1-4.

**To get started now**, read these chapters from Book 2:
- `01_introduction.md` - Introduction to UnifyWeaver
- `02_prolog_fundamentals.md` - Prolog basics
- `03_unifyweaver_architecture.md` - Architecture overview
- `04_your_first_program.md` - First compilation

## What's Next?

After completing Book 1, continue to:
- [Book 2: Bash Target](../book-2-bash-target/README.md) - Stream compilation basics
- [Book 3: C# Target](../book-3-csharp-target/README.md) - Fixed-point approaches

## License

This educational content is licensed under CC BY 4.0.
Code examples are dual-licensed under MIT OR Apache-2.0.
