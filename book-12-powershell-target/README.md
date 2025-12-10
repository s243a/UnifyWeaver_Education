<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Book 12: PowerShell Target

**Windows Automation and .NET Scripting**

*Part of the [UnifyWeaver Education Series](../README.md)*

This book covers compiling Prolog predicates to PowerShell scripts. PowerShell's deep Windows integration and .NET access make this target ideal for Windows automation, system administration, and enterprise environments.

## Prerequisites

**Required:**
- [Book 1: Foundations](../book-01-foundations/README.md)

**Recommended:**
- [Book 3: C# Target](../book-03-csharp-target/README.md) - shares .NET concepts
- [Book 9: Rust Target](../book-09-rust-target/README.md) - first book in the Specialized Targets section

**Technical:**
- PowerShell 7+ (cross-platform) or Windows PowerShell 5.1
- Basic PowerShell knowledge (helpful)

## What You'll Learn

By completing this book, you will be able to:

- Compile Prolog predicates to PowerShell cmdlets
- Generate standalone PowerShell scripts
- Access .NET types from generated code
- Build Windows automation pipelines
- Integrate with Active Directory and Windows services

## Implementation Status

This table shows the current implementation status of features described in each chapter.

| Chapter | Feature | Status | Notes |
|---------|---------|--------|-------|
| [Ch 1](01_introduction.md) | Basic compilation | **Implemented** | `compile_to_powershell/3` works |
| [Ch 1](01_introduction.md) | Module loading | **Implemented** | `use_module(unifyweaver(core/powershell_compiler))` |
| [Ch 2](02_facts_rules.md) | Facts to arrays | **Implemented** | Facts compile to `PSCustomObject` arrays |
| [Ch 2](02_facts_rules.md) | Rules to joins | **Implemented** | `include_dependencies(true)` auto-includes dependent facts |
| [Ch 3](03_cmdlet_generation.md) | CmdletBinding | **Implemented** | `cmdlet_binding(true)` adds `[CmdletBinding()]` |
| [Ch 3](03_cmdlet_generation.md) | Parameter validation | **Implemented** | `arg_options` supports `Mandatory`, `ValidateSet` |
| [Ch 3](03_cmdlet_generation.md) | Begin/Process/End | **Implemented** | Auto-generated for advanced functions |
| [Ch 4](04_dotnet_integration.md) | dotnet_source | **Implemented** | Inline C# compilation works |
| [Ch 4](04_dotnet_integration.md) | DLL caching | **Implemented** | `pre_compile(true)` generates caching |
| [Ch 4](04_dotnet_integration.md) | NuGet references | **Implemented** | `references(['LiteDB'])` works |
| [Ch 5](05_windows_automation.md) | Windows automation | Design only | Examples show patterns, not auto-generated |

**Legend:** **Implemented** = tested and working, Partial = works with limitations, Not yet = documented but not implemented, Design only = conceptual/aspirational

## Chapters

### Part 1: Basic Compilation

**[Chapter 1: Introduction](01_introduction.md)** - *Implemented*
- Why use the PowerShell target?
- PowerShell vs Bash for automation
- Compilation modes (BaaS, Pure, Inline .NET)
- Your first PowerShell compilation

**[Chapter 2: Facts and Rules](02_facts_rules.md)** - *Mostly Implemented*
- Compiling facts to PowerShell arrays
- PSCustomObject for binary facts
- Translating rules to functions with joins
- Pipeline integration

**[Chapter 3: Cmdlet Generation](03_cmdlet_generation.md)** - *Design Document*
- Creating advanced functions with CmdletBinding
- Parameter attributes and validation
- Begin/Process/End blocks
- Verbose and Debug output

### Part 2: .NET Integration

**[Chapter 4: .NET Integration](04_dotnet_integration.md)** - *Fully Implemented*
- Inline C# with Add-Type
- The dotnet_source plugin
- DLL caching for 138x speedup
- NuGet package integration

**[Chapter 5: Windows Automation](05_windows_automation.md)** - *Design Document*
- File system operations
- Windows services management
- Registry access
- Event logs and WMI/CIM queries

### Part 3: Advanced Topics (Planned)

**Chapter 6: In-Process Hosting** *(coming soon)*
- Sharing runtime with C# target
- Cross-target glue via .NET
- Performance considerations

**Chapter 7: Active Directory** *(coming soon)*
- LDAP queries from Prolog
- User and group management
- Permission handling

**Chapter 8: Enterprise Patterns** *(coming soon)*
- Remote execution (PSRemoting)
- Scheduled tasks
- Credential management

## Quick Example

```prolog
% Define a Windows service checker
service_running(Name) :-
    get_service(Name, Status),
    Status == 'Running'.

% Compile to PowerShell
?- compile_to_powershell(service_running/1, [], Code).

% Generated PowerShell:
% function Test-ServiceRunning {
%     param([string]$Name)
%     $service = Get-Service -Name $Name -ErrorAction SilentlyContinue
%     return $service.Status -eq 'Running'
% }
```

## Related Resources

- [PowerShell Inline .NET Playbook](../../playbooks/powershell_inline_dotnet_playbook.md) - Detailed inline C# guide
- [PowerShell .NET Examples](../../playbooks/examples_library/powershell_dotnet_examples.md) - Working examples
- [PowerShell Target API](../../docs/POWERSHELL_TARGET.md) - Full API reference

## What's Next?

After completing Book 12, continue to:
- [Book 7: Cross-Target Glue](../book-07-cross-target-glue/README.md) - .NET bridge integration
- [Book 8: Security & Firewall](../book-08-security-firewall/README.md) - Enterprise security
- [Book 13: Semantic Search](../book-13-semantic-search/README.md) - AI capabilities

## License

This educational content is licensed under CC BY 4.0.
Code examples are dual-licensed under MIT OR Apache-2.0.
