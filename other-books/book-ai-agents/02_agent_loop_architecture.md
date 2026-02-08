<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 2: Agent Loop Architecture

## Overview

The agent loop is the core of any AI coding assistant. This chapter explores the architecture patterns that make it robust, efficient, and extensible.

## The Complete Loop

```
┌────────────────────────────────────────────────────────────┐
│                    UnifyWeaver Agent Loop                   │
├────────────────────────────────────────────────────────────┤
│                                                            │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐    │
│  │   Input     │    │   Agent     │    │   Output    │    │
│  │   Handler   │───▶│   Router    │───▶│   Handler   │    │
│  └─────────────┘    └─────────────┘    └─────────────┘    │
│         │                 │                   │            │
│         ▼                 ▼                   ▼            │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐    │
│  │   Context   │    │  Tool Call  │    │   Display   │    │
│  │   Manager   │◀──▶│   Handler   │    │   Renderer  │    │
│  └─────────────┘    └─────────────┘    └─────────────┘    │
│                                                            │
└────────────────────────────────────────────────────────────┘
```

### Input Handler

Responsible for:
- Reading user input (single line or multi-line)
- Processing slash commands (`/save`, `/load`, etc.)
- Expanding prompt templates
- Handling aliases

### Agent Router

Determines:
- Which backend to use
- What model to call
- Whether to stream responses
- How many iterations to allow

### Context Manager

Manages:
- Conversation history
- System prompts and skills
- Message truncation for token limits
- Format conversion (plain, markdown, JSON, XML)

### Tool Call Handler

Handles:
- Parsing tool calls from LLM output
- User confirmations for destructive operations
- Tool execution and result capture
- Error handling

### Output Handler

Provides:
- Clean text display
- Token usage reporting
- Cost tracking
- Export functionality

## Context Management Strategies

### Mode: Continue

Keep full conversation history:

```python
# Every message stays in context
context = [
    {"role": "user", "content": "message 1"},
    {"role": "assistant", "content": "response 1"},
    {"role": "user", "content": "message 2"},
    {"role": "assistant", "content": "response 2"},
    # ...continues growing
]
```

**Pros**: Full context, best coherence
**Cons**: Eventually hits token limits

### Mode: Fresh

Start each turn with no history:

```python
# Only current message
context = [
    {"role": "user", "content": "current message"}
]
```

**Pros**: No token limits
**Cons**: No continuity between turns

### Mode: Sliding Window

Keep only recent messages:

```python
# Last N messages
context = messages[-5:]  # Last 5 messages
```

**Pros**: Balance of context and limits
**Cons**: May lose important early context

## Context Limits

Beyond message count, the `ContextManager` supports multiple limit dimensions:

| Limit | Flag | Default | Description |
|-------|------|---------|-------------|
| `max_tokens` | `--max-tokens` | 100,000 | Estimated token budget (chars / 4 heuristic) |
| `max_messages` | (sliding mode) | 50 | Maximum number of messages |
| `max_chars` | `--max-chars` | 0 (unlimited) | Total character budget |
| `max_words` | `--max-words` | 0 (unlimited) | Total word budget |

Limits are enforced at two points:

**1. On insertion** (`add_message`): After adding each message, trim the oldest messages until all limits are satisfied:

```python
def _trim_if_needed(self) -> None:
    # Trim by token count (keep at least 1 message)
    while self._token_count > self.max_tokens and len(self.messages) > 1:
        self._remove_oldest()

    # Trim by character count
    if self.max_chars > 0:
        while self._char_count > self.max_chars and len(self.messages) > 1:
            self._remove_oldest()

    # Trim by word count
    if self.max_words > 0:
        while self._word_count > self.max_words and len(self.messages) > 1:
            self._remove_oldest()
```

The `> 1` minimum is important. An earlier version used `> 2`, which prevented trimming when a single long response exceeded the limit.

**2. On retrieval** (`get_context`): Build the context from newest to oldest, stopping when the budget is exhausted:

```python
def get_context(self) -> list[dict]:
    # ...
    result = []
    total_chars = 0
    for msg in reversed(messages):
        msg_chars = len(msg.content)
        if self.max_chars > 0 and total_chars + msg_chars > self.max_chars and result:
            break
        result.append({"role": msg.role, "content": msg.content})
        total_chars += msg_chars
    result.reverse()
    return result
```

This dual enforcement ensures that context limits are respected even when a single message is larger than the budget.

### The Truthiness Bug

An early version of the agent loop had a subtle bug that silently defeated all context limits:

```python
# BROKEN: ContextManager.__len__ returns 0 when empty
# bool(empty_context_manager) is False, so 'or' always replaces it
self.context = context or ContextManager()
```

When `ContextManager` had no messages, Python's `__len__` returned 0, making `bool()` return `False`. The `or` operator then replaced the properly configured context (with `max_chars=50`) with a bare default (with `max_chars=0`).

The fix:

```python
# FIXED: explicit None check preserves empty but configured contexts
self.context = context if context is not None else ContextManager()
```

This is a general Python gotcha: any object implementing `__len__` is falsy when empty. Always use `is not None` when the object might be legitimately empty but should not be replaced.

### Context Deduplication

When sending tool results back to an API backend, the current message might already be the last entry in the context (because it was just added). Without deduplication, the model sees the same message twice:

```python
# Add current message only if not already the last context message
if not context or context[-1].get('content') != message:
    messages.append({"role": "user", "content": message})
```

## Context Formats

Different backends expect different formats. The context manager handles conversion:

### Plain Text

```
User: Create a hello world script
Assistant: I'll create that for you.
[Tool: write hello.py]
User: Now run it
```

### Markdown

```markdown
## User
Create a hello world script

## Assistant
I'll create that for you.

**Tool Call**: write hello.py

## User
Now run it
```

### JSON (Messages API)

```json
[
  {"role": "user", "content": "Create a hello world script"},
  {"role": "assistant", "content": "I'll create that for you."},
  {"role": "tool_result", "content": "File written: hello.py"},
  {"role": "user", "content": "Now run it"}
]
```

### XML

```xml
<conversation>
  <message role="user">Create a hello world script</message>
  <message role="assistant">I'll create that for you.</message>
  <tool_result>File written: hello.py</tool_result>
  <message role="user">Now run it</message>
</conversation>
```

## Session Persistence

Sessions allow saving and resuming conversations.

### Save Format

```json
{
  "id": "abc123",
  "created": "2025-02-01T10:00:00Z",
  "updated": "2025-02-01T10:30:00Z",
  "backend": "claude",
  "model": "claude-sonnet-4-20250514",
  "context_format": "json",
  "messages": [
    {"role": "user", "content": "..."},
    {"role": "assistant", "content": "..."}
  ],
  "metadata": {
    "name": "my-project",
    "tokens_used": 5432
  }
}
```

### Session Operations

```python
# Save current session
session_id = sessions.save(context, name="my-project")

# List available sessions
sessions_list = sessions.list()

# Load a session
context = sessions.load(session_id)

# Search across sessions
results = sessions.search("authentication")
```

## The Iteration Loop

When the LLM returns tool calls, we execute them and continue:

```python
def agent_turn(user_message: str, context: list) -> str:
    """Process one complete user turn."""

    context.append({"role": "user", "content": user_message})
    iterations = 0
    max_iterations = config.max_iterations or float('inf')

    while True:
        # Call LLM
        response = backend.send(context)
        context.append({"role": "assistant", "content": response.content})

        # Parse tool calls
        tool_calls = parse_tool_calls(response.content)

        if not tool_calls:
            # No tools, we're done
            return response.content

        # Execute tools
        for tool in tool_calls:
            if requires_confirmation(tool) and not auto_tools:
                if not user_confirms(tool):
                    context.append({
                        "role": "tool_result",
                        "content": "Tool execution cancelled by user"
                    })
                    continue

            result = execute_tool(tool)
            context.append({"role": "tool_result", "content": result})

        # Check iteration limit
        iterations += 1
        if iterations >= max_iterations:
            print(f"Pausing after {iterations} iterations")
            if not user_wants_continue():
                break
            iterations = 0

    return context[-1]["content"]
```

## Backend Abstraction

The backend interface abstracts different LLM providers:

```python
class AgentBackend(ABC):
    """Abstract base for all backends."""

    @abstractmethod
    def send_message(self, message: str, context: list) -> AgentResponse:
        """Send message with context, return response."""
        pass

    @abstractmethod
    def parse_tool_calls(self, response: str) -> list[ToolCall]:
        """Extract tool calls from response text."""
        pass

    def supports_streaming(self) -> bool:
        """Whether streaming is supported."""
        return False
```

This allows the same agent loop to work with:
- CLI tools (coro, claude-code, gemini, ollama)
- REST APIs (Claude, OpenAI, Ollama)
- Local models

## Configuration and Variants

Agent variants pre-configure common setups:

```yaml
agents:
  default:
    backend: coro
    auto_tools: false
    max_iterations: 10

  yolo:
    description: "Fast, auto-approve everything"
    backend: coro
    auto_tools: true
    max_iterations: 0

  claude-opus:
    description: "Claude Opus for complex tasks"
    backend: claude
    model: claude-opus-4-20250514
    stream: true

  local:
    description: "Local Ollama for privacy"
    backend: ollama-api
    model: codellama
    host: localhost
```

## Summary

- The agent loop coordinates input, context, backends, tools, and output
- Context modes (continue, fresh, sliding) balance coherence vs. token limits
- Multiple context formats support different backends
- Sessions enable save/resume workflows
- Backend abstraction allows one loop, many providers
- Configuration variants simplify common setups

## Next Steps

[Chapter 3](03_tool_handling.md) dives deep into tool call parsing and safe execution.
