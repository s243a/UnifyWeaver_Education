<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Implementation Documentation

**Deep-dive technical documentation for RAG systems**

## Purpose

Each book in the UnifyWeaver Education Series contains an `implementation/` subfolder with detailed technical documentation. These docs provide:

1. **Function-level documentation** - Every public function with parameters, return values, and examples
2. **Algorithm explanations** - Step-by-step breakdowns of key algorithms
3. **Mathematical derivations** - Formal proofs and derivations where applicable
4. **Code patterns** - Common usage patterns and idioms
5. **Edge cases** - Known limitations and how to handle them

## Structure

```
education/
├── book-01-foundations/
│   └── implementation/
│       ├── 03_architecture_impl.md       # Implementation details
│       ├── 03_architecture_questions.md  # Q&A companion
│       └── README.md
├── book-06-go-target/
│   └── implementation/
│       ├── 02_basic_compilation_impl.md
│       ├── 02_basic_compilation_questions.md
│       └── README.md
└── ...
```

## Document Types

### Implementation Docs (`*_impl.md`)
- Function-level API documentation
- Algorithm explanations with code
- Mathematical derivations
- Edge cases and limitations

### Questions Docs (`*_questions.md`)
- Q&A pairs linking to impl sections
- Unique anchor IDs (e.g., `b01c03-q-...`) for concatenation
- Suitable for NotebookLM and training data
- Question index for navigation

## For RAG Systems

These documents are designed for retrieval-augmented generation:

- **Atomic sections** - Each function/concept is self-contained
- **Consistent format** - All docs follow the same structure
- **Rich metadata** - Headers, tables, and code blocks for parsing
- **Cross-references** - Links to related concepts and source files

### Recommended Chunking

For optimal RAG performance, chunk at:
1. H2 headers (`## Function Name`)
2. Code blocks (preserve complete examples)
3. Tables (keep rows together)

### Search Keywords

Each implementation doc includes:
- Function names and signatures
- Algorithm names (e.g., "Givens rotation", "softmax")
- Common error patterns
- Performance characteristics

## Contributing

When adding implementation docs:

1. Follow the existing format
2. Include working code examples
3. Document edge cases and limitations
4. Add source file references
5. Use clear, concise language

## Books with Implementation Docs

| Book | Status | Chapters |
|------|--------|----------|
| Book 1: Foundations | In Progress | Ch 3 (12 questions) |
| Book 2: Bash Target | In Progress | Ch 1-2 (19 questions) |
| Book 3: C# Target | In Progress | Ch 3 (12 questions) |
| Book 4: Workflows | In Progress | Ch 3 (11 questions) |
| Book 5: Python Target | In Progress | Ch 3 (14 questions) |
| Book 6: Go Target | In Progress | Ch 2 (9 questions) |
| Book 7: Cross-Target Glue | In Progress | Ch 4 (11 questions) |
| Book 8: Security & Firewall | In Progress | Ch 2 (12 questions) |
| Book 9: Rust Target | In Progress | Ch 2 (10 questions) |
| Book 10: SQL Target | In Progress | Ch 3 (11 questions) |
| Book 11: Prolog Target | In Progress | Ch 2 (10 questions) |
| Book 12: PowerShell Target | In Progress | Ch 2 (12 questions) |
| Book 13: Semantic Search | In Progress | Ch 2 (12 questions) |
| Book 14: AI Training | In Progress | Ch 6 (12 questions) |
| Book 15: Perl Target | In Progress | Ch 2 (10 questions) |
| Book 16: Ruby Target | In Progress | Ch 2 (10 questions) |
| Book AWK Target | In Progress | Overview (10 questions) |

### Other Books (other-books/)

| Book | Status | Chapters |
|------|--------|----------|
| C Target | In Progress | Ch 2 (10 questions) |
| C++ Target | In Progress | Ch 2 (10 questions) |
| Clojure Target | In Progress | Ch 2 (10 questions) |
| F# Target | In Progress | Ch 3 (10 questions) |
| GUI Generation | In Progress | Ch 2, 9 (20 questions) |
| Haskell Target | In Progress | Ch 2 (10 questions) |
| Java Target | In Progress | Ch 2 (10 questions) |
| Jython Target | In Progress | Ch 2 (10 questions) |
| Kotlin Target | In Progress | Ch 2 (10 questions) |
| LLVM Target | In Progress | Ch 2 (10 questions) |
| Scala Target | In Progress | Ch 2 (10 questions) |
| TypeScript Target | In Progress | Ch 2 (10 questions) |
| VB.NET Target | In Progress | Ch 3 (10 questions) |
| WASM Target | In Progress | Ch 2 (10 questions) |

## Total Questions: 347
