<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Book: Go Target & Cross-Platform Compilation

**Learning Path for UnifyWeaver's Go Target**

This book covers how to use UnifyWeaver to compile Prolog predicates into standalone Go executables. The Go target enables high-performance, cross-platform record processing with no runtime dependencies.

## Prerequisites

-   Basic understanding of Prolog (covered in Book 1)
-   Go 1.16+ installed (for compiling generated code)
-   UnifyWeaver with Go target support (v0.5+)

## Learning Path

**1. Introduction** (`01_introduction.md`)
-   Why use the Go target?
-   Architecture overview
-   Comparison with other targets

**2. Basic Compilation** (`02_basic_compilation.md`)
-   Compiling facts to lookup tables
-   Compiling rules for stream processing
-   Running generated binaries

**3. Advanced Features** (`03_advanced_features.md`)
-   Regex matching with `match/2`
-   Capture groups and extraction
-   Constraints and aggregations

**4. JSON Processing** (`04_json_processing.md`)
-   JSON input/output
-   Schema validation
-   Nested field extraction

## License

This educational content is licensed under CC BY 4.0.
Code examples are dual-licensed under MIT OR Apache-2.0.
