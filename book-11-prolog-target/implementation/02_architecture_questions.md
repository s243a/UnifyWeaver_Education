<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Architecture - Questions

Q&A companion for [02_architecture_impl.md](./02_architecture_impl.md).

---

## Question Index

1. [What does generate_prolog_script/3 do?](#b11c02-q-generate-script)
2. [What are the pipeline steps?](#b11c02-q-pipeline-steps)
3. [What does analyze_dependencies/2 find?](#b11c02-q-dependencies)
4. [What dialects are supported?](#b11c02-q-dialects)
5. [How are shebangs generated?](#b11c02-q-shebang)
6. [How is initialization code different per dialect?](#b11c02-q-initialization)
7. [What does write_prolog_script/3 do?](#b11c02-q-write)
8. [How does safe compilation work?](#b11c02-q-compile-safe)
9. [How do I validate dialect compatibility?](#b11c02-q-validate)
10. [What are the design principles?](#b11c02-q-design)

---

## Questions and Answers

### <a id="b11c02-q-generate-script"></a>Q1: What does generate_prolog_script/3 do?

**Answer**: Main entry point for Prolog script generation:

```prolog
generate_prolog_script(
    [factorial/2],
    [dialect(gnu), compile(true), entry_point(test_factorial)],
    ScriptCode
).
```

Options: `dialect/1`, `compile/1`, `entry_point/1`, `output_file/1`.

**See**: [generate_prolog_script/3](./02_architecture_impl.md#generate_prolog_script3)

---

### <a id="b11c02-q-pipeline-steps"></a>Q2: What are the pipeline steps?

**Answer**: Eight steps:

| Step | Function |
|------|----------|
| 0 | Dialect validation |
| 1 | Dependency analysis |
| 2 | Shebang generation |
| 3 | Header generation |
| 4 | Import generation |
| 5 | User code extraction |
| 6 | Entry point generation |
| 7 | Initialization code |

Step 8 assembles all pieces.

**See**: [Generation Pipeline Steps](./02_architecture_impl.md#generation-pipeline-steps)

---

### <a id="b11c02-q-dependencies"></a>Q3: What does analyze_dependencies/2 find?

**Answer**: Scans predicates for required modules:

```prolog
Dependencies = [
    module(unifyweaver(core/partitioner)),
    ensure_loaded(unifyweaver(core/partitioners/fixed_size)),
    plugin_registration(partitioner, fixed_size, ...)
]
```

**See**: [analyze_dependencies/2](./02_architecture_impl.md#analyze_dependencies2)

---

### <a id="b11c02-q-dialects"></a>Q4: What dialects are supported?

**Answer**: Two dialects:

```prolog
supported_dialect(swi).  % SWI-Prolog
supported_dialect(gnu).  % GNU Prolog
```

Use `dialect_capabilities/2` to query features.

**See**: [Dialect Predicates](./02_architecture_impl.md#dialect-predicates)

---

### <a id="b11c02-q-shebang"></a>Q5: How are shebangs generated?

**Answer**: Dialect-specific shebang lines:

| Dialect | Shebang |
|---------|---------|
| `swi` | `#!/usr/bin/env swipl` |
| `gnu` | `#!/usr/bin/env gprolog --consult-file` |

**See**: [dialect_shebang/2](./02_architecture_impl.md#dialect_shebang2)

---

### <a id="b11c02-q-initialization"></a>Q6: How is initialization code different per dialect?

**Answer**:

| Dialect | Compiled | Code |
|---------|----------|------|
| `gnu` | Yes | `:- initialization(goal).` |
| `gnu` | No | `:- goal.` |
| `swi` | - | `:- initialization(goal, main).` |

**See**: [Step 7: Initialization Code](./02_architecture_impl.md#step-7-initialization-code)

---

### <a id="b11c02-q-write"></a>Q7: What does write_prolog_script/3 do?

**Answer**: Three actions:

1. Write file with UTF-8 encoding
2. Set executable permission (`chmod +x`)
3. Compile if `compile(true)` in options

**See**: [write_prolog_script/3](./02_architecture_impl.md#write_prolog_script3)

---

### <a id="b11c02-q-compile-safe"></a>Q8: How does safe compilation work?

**Answer**: Catches failures and continues:

```prolog
compile_script_safe(Dialect, ScriptPath, Options) :-
    catch(
        compile_script(Dialect, ScriptPath),
        error(compilation_failed(_, _), _),
        handle_compilation_failure(...)
    ).
```

Logs warnings, continues with interpreted script.

**See**: [compile_script_safe/3](./02_architecture_impl.md#compile_script_safe3)

---

### <a id="b11c02-q-validate"></a>Q9: How do I validate dialect compatibility?

**Answer**: Use `validate_for_dialect/3`:

```prolog
validate_for_dialect(gnu, [my_pred/2], Issues).
```

Returns list of compatibility issues.

**See**: [validate_for_dialect/3](./02_architecture_impl.md#validate_for_dialect3)

---

### <a id="b11c02-q-design"></a>Q10: What are the design principles?

**Answer**:

| Principle | Implementation |
|-----------|----------------|
| Separation | Dialect logic isolated |
| Extensibility | New dialects only modify one module |
| Robustness | Safe wrappers for failures |
| Testability | Each step is separate predicate |

**See**: [Design Principles](./02_architecture_impl.md#design-principles)

---

## Summary

Prolog target architecture provides:
- 8-step generation pipeline
- Dialect abstraction (SWI, GNU)
- Dependency analysis
- Safe compilation with fallback
- Clear separation of concerns
