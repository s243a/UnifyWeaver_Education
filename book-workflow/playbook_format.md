# Playbook Format Specification

This document details the standardized format for creating playbooks, which serve as strategic guides for LLM agents within UnifyWeaver workflows. Playbooks are Markdown-based and incorporate specific callouts and structures to facilitate clear communication and automated parsing by the agent.

## Core Structure

A playbook typically consists of:

1.  **Frontmatter (Optional):** YAML frontmatter for metadata (e.g., `id`, `name`, `version`, `workflow_id`).
2.  **Goal/Objective:** A clear, concise statement of the task the playbook aims to achieve.
3.  **Context:** Any necessary background information or assumptions.
4.  **Strategies:** A description of different approaches the agent can take to achieve the goal, often with associated economic data (cost, speed, quality) and heuristic guidance.
5.  **Tool Usage:** Instructions on how to use specific tools, referencing their skill definitions.
6.  **Output Specifications:** Using the `output` callout, define the expected outputs from the agent's execution.

## Callout Types

Playbooks leverage GitHub-style callouts (admonitions) for structured information.

### 1. `[!output]` Callout (New)

This callout specifies the expected output from the agent's execution. It includes metadata to provide context about the output.

**Structure:**

```markdown
> [!output]
> language: <language_name> (e.g., prolog, bash, markdown, json, text)
> purpose: <brief_description_of_the_output>
> # Optional additional metadata
> format: <e.g., executable, data, documentation, configuration>
> expected_size: <e.g., small, medium, large>
>
> ```<language_name>
> # The generated output will go here.
> # This block serves as a template or placeholder for the agent's output.
> ```
```

**Guidelines:**
*   The `language` field is mandatory and specifies the programming language or format of the output.
*   The `purpose` field is mandatory and provides a human-readable description of what the output represents.
*   The content within the fenced code block (` ```<language_name> ... ``` `) serves as a template or placeholder for the agent's generated output.
*   **Constraint:** The content within `[!output]` callouts (including metadata and code block) should be kept concise, ideally representing less than 10% of the total playbook content. This ensures playbooks remain focused on strategy and guidance rather than becoming large output containers.

### 2. `[!example-record]` Callout (Existing, for reference)

Used to embed or reference examples, as seen in `log_examples.md` and `recursion_examples.md`.

**Structure:**

```markdown
> [!example-record]
> id: <unique_id>
> name: <descriptive_name>
>
> ```<language_name>
> # Example content
> ```
```

## Referencing Example Libraries

Playbooks can and should reference examples from the `examples_library` or other designated example directories. This promotes reusability and provides concrete instances for the agent.

**Mechanism:**
Use standard Markdown links to point to relevant example files or sections within them.

```markdown
For an example of an ancestor relationship in Prolog, refer to [Prolog Recursion Examples](../../examples_library/recursion_examples.md).
```

## Prolog Generation Guidance

When a playbook requires the agent to generate Prolog code for UnifyWeaver, the playbook should provide:

*   Clear specifications for the expected predicates, their arity, and their semantics.
*   Examples of desired Prolog structures.
*   Instructions on how to invoke the `compiler_driver.pl` with the generated Prolog.
*   Guidance on handling the transpiled output.
