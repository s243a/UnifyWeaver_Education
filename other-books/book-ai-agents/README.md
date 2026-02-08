<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Book: AI Agents

**Building Terminal-Friendly AI Assistants with the Agent Loop**

*Part of the [UnifyWeaver Education Series](../../README.md)*

This book covers building and using AI coding agents, with a focus on the UnifyWeaver Agent Loop - a terminal-friendly alternative to TUI-based assistants that works reliably in Termux and constrained environments.

## Prerequisites

- Basic Python knowledge
- Familiarity with command-line interfaces
- Understanding of HTTP APIs (helpful but not required)

## What You'll Learn

By completing this book, you will understand:

- What AI agents are and how they work
- The agent loop architecture and design patterns
- How to parse and execute tool calls safely
- Integrating multiple AI backends (Claude, OpenAI, Ollama, etc.)
- Building custom agents for your specific needs

## Chapter Overview

### Part 1: Foundations

**[Chapter 1: Introduction to AI Agents](01_introduction.md)**
- What is an AI agent?
- Agents vs. simple chatbots
- The tool-use paradigm
- Why terminal-friendly agents matter

**[Chapter 2: Agent Loop Architecture](02_agent_loop_architecture.md)**
- The core loop: prompt → response → tool → continue
- Context management strategies
- Session persistence
- Backend abstraction patterns

### Part 2: Implementation

**[Chapter 3: Tool Handling](03_tool_handling.md)**
- Parsing tool calls from LLM output
- Safe tool execution with confirmations
- Built-in tools: bash, read, write, edit
- Error handling and recovery

**[Chapter 4: Backend Integration](04_backend_integration.md)**
- The backend interface
- CLI backends (coro, claude-code, gemini, ollama-cli)
- API backends (Claude, OpenAI, Ollama)
- Streaming support

### Part 3: Advanced Topics

**[Chapter 5: Building Custom Agents](05_building_custom_agents.md)**
- Configuration files and agent variants
- API key resolution: uwsal.json, env vars, and the priority chain
- Skills and system prompts
- Prompt templates
- Creating domain-specific agents

### Part 4: Production

**[Chapter 6: Cost Tracking and Pricing](06_cost_tracking.md)**
- Per-model pricing (input/output per 1M tokens)
- CostTracker architecture and usage recording
- OpenRouter pricing auto-fetch with local caching
- Cost implications of tool-calling iterations

**[Chapter 7: Display and Terminal UX](07_display_system.md)**
- Animated spinners with tput-based terminal control
- Status callbacks and permanent line commits
- Elapsed time display and terminal width truncation
- Append-only fallback for limited terminals
- Stream-JSON for live tool progress

**[Chapter 8: Lessons from Production](08_lessons_from_production.md)**
- The truthiness bug (`context or ContextManager()`)
- CLI backends and their internal context
- Trimming minimums and off-by-one errors
- Testing with extreme values
- Debug output patterns and observability

## Implementation

The `implementation/` folder contains working code referenced throughout:

| File | Description |
|------|-------------|
| (See tools/agent-loop/generated/) | Full implementation |

## Relationship to UnifyWeaver

The Agent Loop tool demonstrates UnifyWeaver's Prolog-based code generation:

1. **Prolog specs** define backend configurations declaratively
2. **Generator** produces Python code from specs
3. **Result** is a working multi-backend agent system

This pattern - declarative specification to imperative implementation - is the core of UnifyWeaver's approach.

## What's Next?

After completing this book:
- Use the agent loop for your coding tasks
- Extend it with custom backends
- Explore the Prolog generation layer

## License

This educational content is licensed under CC BY 4.0.
Code examples are dual-licensed under MIT OR Apache-2.0.
