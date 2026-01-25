<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 1: Introduction - Questions

**Q&A companion to [01_introduction_impl.md](./01_introduction_impl.md)**

This document contains questions about UnifyWeaver introduction and setup.

---

<a id="b01c01-q-what-is-unifyweaver"></a>
## Q: What is UnifyWeaver?

UnifyWeaver is a Prolog-based code generator that compiles declarative specifications into multiple target languages. It translates Prolog facts, rules, and queries into optimized code for various runtimes.

**Key insight**: Define relationships once in Prolog, generate optimized code for any target.

**Reference**: [Chapter 1: Introduction](../01_introduction.md#what-is-unifyweaver)

---

<a id="b01c01-q-install-swipl"></a>
## Q: How do I install SWI-Prolog?

| Platform | Command |
|----------|---------|
| Ubuntu/Debian | `sudo apt-get install swi-prolog` |
| Arch Linux | `sudo pacman -S swi-prolog` |
| macOS | `brew install swi-prolog` |
| Windows | `winget install SWI-Prolog.SWI-Prolog` |
| Termux | `pkg install swi-prolog` |

Verify with: `swipl --version`

**Reference**: [Installation](./01_introduction_impl.md#installation)

---

<a id="b01c01-q-clone-repo"></a>
## Q: How do I set up the UnifyWeaver repository?

```bash
git clone https://github.com/s243a/UnifyWeaver.git
cd UnifyWeaver
git clone https://github.com/s243a/UnifyWeaver_Education.git education
```

**Reference**: [Repository Setup](./01_introduction_impl.md#repository-setup)

---

<a id="b01c01-q-start-unifyweaver"></a>
## Q: How do I start UnifyWeaver?

```bash
swipl -f init.pl
```

The `init.pl` file sets up module search paths so UnifyWeaver modules can be loaded.

**Reference**: [Starting UnifyWeaver](./01_introduction_impl.md#starting-unifyweaver)

---

<a id="b01c01-q-load-modules"></a>
## Q: How do I load UnifyWeaver modules?

Use the `unifyweaver(...)` alias after loading `init.pl`:

```prolog
?- use_module(unifyweaver(core/compiler_driver)).
?- use_module(unifyweaver(targets/bash_target)).
```

**Reference**: [Loading Modules](./01_introduction_impl.md#loading-modules)

---

<a id="b01c01-q-module-alias"></a>
## Q: How does the module alias system work?

The `unifyweaver(...)` alias resolves paths:

```
unifyweaver(core/recursive_compiler)
→ 'src/unifyweaver/core/recursive_compiler.pl'
```

This is configured in `init.pl` using `file_search_path/2`.

**Reference**: [Module Path Resolution](./01_introduction_impl.md#module-path-resolution)

---

<a id="b01c01-q-project-structure"></a>
## Q: What is the UnifyWeaver project structure?

| Directory | Purpose |
|-----------|---------|
| `src/unifyweaver/core/` | Core compilation logic |
| `src/unifyweaver/targets/` | Language-specific generators |
| `src/unifyweaver/incremental/` | Caching system |
| `templates/` | External template files |
| `education/` | Tutorial and documentation |

**Reference**: [Project Structure](./01_introduction_impl.md#project-structure)

---

<a id="b01c01-q-available-targets"></a>
## Q: What compilation targets does UnifyWeaver support?

**Shell/Scripting**: Bash, PowerShell, AWK

**Systems**: Go, Rust, LLVM, WASM

**.NET**: C# (Stream & Query), F#, VB.NET

**Dynamic**: Python, TypeScript

**Data**: SQL, Prolog

**Reference**: [Target Categories](./01_introduction_impl.md#target-categories)

---

<a id="b01c01-q-compilation-approaches"></a>
## Q: What compilation approaches does UnifyWeaver use?

| Approach | Targets | Description |
|----------|---------|-------------|
| Stream-based | Bash, Go, Rust | Unix-style pipelines |
| Generator | Python | Lazy evaluation |
| Query Runtime | C# Query | Semi-naive evaluation |
| Declarative SQL | SQL | Database execution |

**Reference**: [Compilation Approaches](./01_introduction_impl.md#compilation-approaches)

---

<a id="b01c01-q-stream-based"></a>
## Q: How does stream-based compilation work?

Data flows through Unix-style pipelines:

```
Input Stream → Filter → Transform → Filter → Output Stream
```

**Targets**: Bash, Go, Rust, AWK

**Characteristics**: Memory efficient, composable, natural for shell.

**Reference**: [Stream-Based Pipelines](./01_introduction_impl.md#stream-based-pipelines)

---

<a id="b01c01-q-generator-mode"></a>
## Q: How does generator/lazy evaluation work?

Uses Python generators for on-demand computation:

```python
def ancestors(person):
    for parent in parents(person):
        yield parent
        yield from ancestors(parent)
```

**Characteristics**: Memory efficient, Pythonic, good for large datasets.

**Reference**: [Generator/Lazy Evaluation](./01_introduction_impl.md#generatorlazy-evaluation)

---

<a id="b01c01-q-declarative-vs-imperative"></a>
## Q: What's the difference between declarative and imperative programming?

**Declarative** (Prolog): Describes **what** you want.
```prolog
ancestor(X, Y) :- parent(X, Y).
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y).
```

**Imperative** (Bash): Describes **how** to compute it.
```bash
while [[ ${#queue[@]} -gt 0 ]]; do
    current="${queue[0]}"
    # ... process current, add to queue ...
done
```

UnifyWeaver bridges these paradigms.

**Reference**: [Declarative vs Imperative](./01_introduction_impl.md#declarative-vs-imperative)

---

<a id="b01c01-q-problem-solved"></a>
## Q: What problem does UnifyWeaver solve?

- **Repetitive boilerplate**: Same logic in multiple scripts
- **Inconsistent data handling**: Different scripts, different bugs
- **Hidden dependencies**: Hard to see relationships
- **Maintenance nightmares**: Changes require hunting

**Solution**: Single source of truth in Prolog, generate disposable artifacts.

**Reference**: [Chapter 1: Introduction](../01_introduction.md#the-problem-it-solves)

---

## Question Index

| ID | Topic |
|----|-------|
| [b01c01-q-what-is-unifyweaver](#b01c01-q-what-is-unifyweaver) | What is UnifyWeaver |
| [b01c01-q-install-swipl](#b01c01-q-install-swipl) | SWI-Prolog installation |
| [b01c01-q-clone-repo](#b01c01-q-clone-repo) | Repository setup |
| [b01c01-q-start-unifyweaver](#b01c01-q-start-unifyweaver) | Starting UnifyWeaver |
| [b01c01-q-load-modules](#b01c01-q-load-modules) | Loading modules |
| [b01c01-q-module-alias](#b01c01-q-module-alias) | Module alias system |
| [b01c01-q-project-structure](#b01c01-q-project-structure) | Project structure |
| [b01c01-q-available-targets](#b01c01-q-available-targets) | Available targets |
| [b01c01-q-compilation-approaches](#b01c01-q-compilation-approaches) | Compilation approaches |
| [b01c01-q-stream-based](#b01c01-q-stream-based) | Stream-based compilation |
| [b01c01-q-generator-mode](#b01c01-q-generator-mode) | Generator mode |
| [b01c01-q-declarative-vs-imperative](#b01c01-q-declarative-vs-imperative) | Declarative vs imperative |
| [b01c01-q-problem-solved](#b01c01-q-problem-solved) | Problem solved |
