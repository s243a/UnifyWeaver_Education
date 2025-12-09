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

## Chapters

### Part 1: Basic Compilation

**[Chapter 1: Introduction](01_introduction.md)**
- Why use the PowerShell target?
- PowerShell vs Bash for automation
- Compilation modes (BaaS, Pure, Inline .NET)
- Your first PowerShell compilation

**[Chapter 2: Facts and Rules](02_facts_rules.md)**
- Compiling facts to PowerShell arrays
- PSCustomObject for binary facts
- Translating rules to functions with joins
- Pipeline integration

**[Chapter 3: Cmdlet Generation](03_cmdlet_generation.md)**
- Creating advanced functions with CmdletBinding
- Parameter attributes and validation
- Begin/Process/End blocks
- Verbose and Debug output

### Part 2: .NET Integration

**[Chapter 4: .NET Integration](04_dotnet_integration.md)**
- Inline C# with Add-Type
- The dotnet_source plugin
- DLL caching for 138x speedup
- NuGet package integration

**[Chapter 5: Windows Automation](05_windows_automation.md)**
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

## Content Status

**Completed:**
- Chapter 1: Introduction
- Chapter 2: Facts and Rules
- Chapter 3: Cmdlet Generation
- Chapter 4: .NET Integration
- Chapter 5: Windows Automation

**Planned:**
- Chapter 6-8: Advanced enterprise topics

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
