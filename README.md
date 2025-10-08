# UnifyWeaver Educational Materials

**A comprehensive tutorial series for learning UnifyWeaver**

This repository contains educational materials for [UnifyWeaver](https://github.com/s243a/UnifyWeaver), a Prolog-to-Bash compiler that generates efficient streaming bash scripts from declarative Prolog logic.

## Status

🚧 **Work in Progress** - Currently being developed

These materials are preliminary and may contain errors or incomplete sections. Contributions and corrections are welcome!

## Chapters

1. **[Introduction](01_introduction.md)** - What is UnifyWeaver and why use it?
2. **[Prolog Fundamentals](02_prolog_fundamentals.md)** - Basic Prolog concepts
3. **[UnifyWeaver Architecture](03_unifyweaver_architecture.md)** - How the compiler works
4. **[Your First Program](04_your_first_program.md)** - Hands-on tutorial with family tree
5. **[Stream Compilation](05_stream_compilation.md)** - Non-recursive predicates
6. **[Advanced Constraints](06_advanced_constraints.md)** - Constraint system
7. **[Variable Scope and Process Substitution](07_variable_scope_and_process_substitution.md)** - Bash variable scoping
8. **[Template System](08_template_system.md)** - Code generation templates
9. **[Advanced Recursion Patterns](09_advanced_recursion.md)** - Tail, linear, and mutual recursion

### Planned Chapters

10. **Prolog Introspection and Theory** - How UnifyWeaver analyzes Prolog code
11. **Test Runner Inference** - Automatic test generation

## Prerequisites

- Basic understanding of logic programming or functional programming
- Familiarity with Unix/Linux command line
- SWI-Prolog installed (for running examples)
- Bash shell (for running generated scripts)

## How to Use This Tutorial

### Sequential Learning

Work through chapters in order. Each chapter builds on previous concepts.

### Hands-On Approach

Each chapter includes:
- Conceptual explanations
- Code examples
- Actual generated code (not pseudo-code!)
- Exercises to try yourself

### Running Examples

```bash
# Clone UnifyWeaver
git clone https://github.com/s243a/UnifyWeaver.git
cd UnifyWeaver

# Start SWI-Prolog
swipl

# Load a compiler module
?- use_module('src/unifyweaver/core/stream_compiler').

# Try the examples from the chapters!
```

## License

This educational content is dual-licensed:

- **[MIT License](LICENSE-MIT.md)** - For code examples
- **[Creative Commons Attribution 4.0 International (CC-BY-4.0)](LICENSE-CC-BY-4.0.md)** - For written content

You are free to:
- ✅ Use these materials for learning
- ✅ Share and redistribute
- ✅ Adapt and build upon
- ✅ Use commercially

With attribution to: **John William Creighton (@s243a)**

See individual license files for full terms.

## Contributing

Contributions are welcome! If you find errors, unclear explanations, or want to add content:

1. Fork this repository
2. Make your changes
3. Submit a pull request

Please maintain the dual-licensing structure for contributions.

## Related Projects

- **[UnifyWeaver](https://github.com/s243a/UnifyWeaver)** - The main compiler
- **UnifyWeaver Documentation** - Technical reference (in main repo `docs/` folder)

## Acknowledgments

Created with assistance from:
- Claude (Anthropic) - AI pair programming assistant
- Gemini (Google) - Initial tutorial drafts

## Questions or Feedback?

- Open an issue on GitHub
- Discussions welcome!

---

**Note:** These materials document UnifyWeaver's current state. As the project evolves, some content may become outdated. Check the main UnifyWeaver repository for the latest code.

*Last updated: 2025-10-06*
