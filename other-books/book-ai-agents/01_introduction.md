<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 1: Introduction to AI Agents

## What is an AI Agent?

An **AI agent** is a system that uses a large language model (LLM) to accomplish tasks by:

1. **Understanding** natural language instructions
2. **Planning** a sequence of actions
3. **Executing** those actions using tools
4. **Adapting** based on results

The key distinction from a simple chatbot is **tool use** - agents can take actions in the real world, not just generate text.

## Agents vs. Chatbots

| Chatbot | Agent |
|---------|-------|
| Answers questions | Completes tasks |
| Generates text | Executes code |
| Single response | Multi-step loops |
| Stateless | Context-aware |

A chatbot might tell you *how* to create a file. An agent *creates the file*.

## The Tool-Use Paradigm

Modern LLMs can be instructed to output structured "tool calls" when they need to take actions:

```
User: Create a hello.py file

LLM Output:
I'll create that file for you.

<tool_call>
{"name": "write", "path": "hello.py", "content": "print('Hello!')"}
</tool_call>
```

The **agent loop** parses these tool calls, executes them, and returns results to the LLM for further processing.

## Why Terminal-Friendly Agents Matter

Popular coding agents like Claude Code, Cursor, and Aider provide excellent TUI (Terminal User Interface) experiences - spinners, progress bars, syntax highlighting.

However, these break in constrained environments:

| Environment | Problem |
|-------------|---------|
| **Termux** | Many escape codes unsupported |
| **SSH sessions** | Terminal capabilities vary |
| **tmux/screen** | Nested terminal issues |
| **xterm.js** | Browser-based terminal limitations |
| **CI/CD** | Non-interactive contexts |

The UnifyWeaver Agent Loop solves this by:

1. **Using single-task mode** - No fancy TUI, just text in/out
2. **Managing context ourselves** - Track conversation history
3. **Clean output** - Strip escape codes, format plainly

## Components of an Agent System

```
┌─────────────────────────────────────────────┐
│              User Interface                  │
│         (Terminal, CLI, REPL)               │
├─────────────────────────────────────────────┤
│              Agent Loop                      │
│  ┌─────────────────────────────────────┐    │
│  │ 1. Receive user input               │    │
│  │ 2. Format with context              │    │
│  │ 3. Send to LLM backend              │    │
│  │ 4. Parse response for tool calls    │    │
│  │ 5. Execute tools (with confirmation)│    │
│  │ 6. Add results to context           │    │
│  │ 7. Repeat until done                │    │
│  └─────────────────────────────────────┘    │
├─────────────────────────────────────────────┤
│              LLM Backends                    │
│  Claude │ OpenAI │ Ollama │ Gemini │ etc.  │
├─────────────────────────────────────────────┤
│              Tool Handlers                   │
│  Bash │ Read │ Write │ Edit │ Custom       │
└─────────────────────────────────────────────┘
```

## The Basic Agent Loop

Here's the fundamental pattern in pseudocode:

```python
context = []

while True:
    # Get user input
    user_message = input("You: ")
    context.append({"role": "user", "content": user_message})

    # Loop until no more tool calls
    while True:
        # Send to LLM
        response = llm.send(context)
        context.append({"role": "assistant", "content": response})

        # Check for tool calls
        tool_calls = parse_tool_calls(response)
        if not tool_calls:
            break  # Done with this turn

        # Execute each tool
        for tool in tool_calls:
            result = execute_tool(tool)
            context.append({"role": "tool", "content": result})

    # Show response to user
    print(f"Assistant: {response}")
```

This simple loop is the foundation of all agent systems.

## Key Challenges

Building a robust agent requires solving:

1. **Context Management** - Conversations grow large; need truncation or summarization
2. **Tool Safety** - Agents can run arbitrary code; need confirmations
3. **Error Recovery** - Tools fail; agents need to adapt
4. **Backend Differences** - Each LLM has different APIs and tool formats
5. **Session Persistence** - Users want to resume conversations

The following chapters address each of these.

## Summary

- AI agents use LLMs to complete tasks, not just answer questions
- The key capability is **tool use** - executing actions in the real world
- Terminal-friendly agents work where fancy TUIs break
- The basic pattern is a loop: user → LLM → tools → results → LLM → ...

## Next Steps

In [Chapter 2](02_agent_loop_architecture.md), we'll examine the agent loop architecture in detail, including context management and session handling.
