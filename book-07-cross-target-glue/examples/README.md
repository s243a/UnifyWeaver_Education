# Cross-Target Glue Examples

This book's examples require multiple working compilation targets and a complex runtime environment to orchestrate them. Due to the interdependency between targets, standalone examples are not practical here.

## Where to Find Examples

Cross-target glue examples are available in the main UnifyWeaver project:

- **Main Playbook**: `playbooks/cross_target_glue_playbook.md`
- **Examples Library**: `playbooks/examples_library/cross_target_glue_examples.md`

These examples demonstrate:
- Shell integration (AWK + Python + Bash pipelines)
- .NET integration (C# + PowerShell bridges)
- Multi-target orchestration

## Prerequisites

To run cross-target glue examples, ensure you have the following targets working:
- Bash target (Book 02)
- C# target (Book 03)
- Python target (Book 05)
- AWK target (Book AWK)

Refer to individual target books to verify each compilation target is functional before attempting cross-target integration.
