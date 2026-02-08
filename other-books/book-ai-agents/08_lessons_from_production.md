<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 8: Lessons from Production

This chapter documents real bugs encountered, debugging techniques used, and architectural lessons learned while building and testing the agent loop. These are not theoretical concerns — each one was discovered through actual use.

## Lesson 1: The Truthiness Bug

### The Bug

All context limits (`--max-chars`, `--max-words`, `--max-tokens`) silently did nothing for every backend. Setting `--max-chars 50` had zero effect.

### Root Cause

In the `AgentLoop.__init__` constructor:

```python
self.context = context or ContextManager()
```

The `ContextManager` class implements `__len__` (returning the number of messages). When a freshly created `ContextManager` has no messages, `len()` returns 0. In Python, `bool(obj)` calls `__len__` for objects that implement it, and `bool(0)` is `False`.

So this happened:

1. CLI parses `--max-chars 50` and creates `ContextManager(max_chars=50)`
2. This context manager has 0 messages, so `bool(context_manager)` is `False`
3. `context or ContextManager()` evaluates the `or`, replacing our configured context with a bare default
4. The bare default has `max_chars=0` (unlimited)

Two `ContextManager` instances were created: the first with the user's limits, the second discarding them.

### The Fix

```python
self.context = context if context is not None else ContextManager()
```

Use `is not None` instead of truthiness when the object might be legitimately empty.

### The Lesson

This is a general Python gotcha. Any class implementing `__len__`, `__bool__`, or `__eq__` can produce surprising behavior with `or`. Common victims:

```python
# All of these are falsy when empty:
[]          # list
{}          # dict
set()       # set
""          # string
0           # int
# And any custom class with __len__ returning 0
```

Always use `is not None` when the distinction between "empty" and "absent" matters.

### How It Was Found

Debug output was added to trace `ContextManager` creation:

```python
# Temporary monkey-patch to find where contexts were created
_orig_init = ContextManager.__init__
def _traced_init(self, *args, **kwargs):
    _orig_init(self, *args, **kwargs)
    print(f"[DEBUG] ContextManager created: max_chars={self.max_chars}", flush=True)
_traced_init.__wrapped__ = True
ContextManager.__init__ = _traced_init
```

This revealed two `ContextManager` creations: first with `max_chars=50`, then immediately with `max_chars=0`. The second one was the replacement by `or`.

## Lesson 2: CLI Backends Have Their Own Context

### The Bug

Setting `--max-chars 50` with the coro backend had no effect. The model still remembered information from earlier in the conversation.

### Root Cause

Coro-code (and similar CLI agents) manage their own context internally:

- **CORO.md**: A workspace file the agent maintains with project context
- **Cached file summaries**: The agent caches summaries of files it has read
- **Auto-context**: The agent automatically includes relevant files in its prompt

Our `--max-chars` only controls the conversation prefix we send as the prompt argument. The coro CLI then wraps this in its own context, adding workspace information, file caches, and system prompts. Even if we send zero conversation history, the backend still has context.

### The Workaround

Two approaches were implemented:

1. **OpenRouter backend**: Call the API directly, bypassing coro's agent layer. We construct the entire `messages` array, so `--max-chars` controls everything the model sees.

2. **Coro `max_token` passthrough**: Coro supports a `max_token` config option. When `--max-tokens` is passed to the agent loop, a temporary `coro.json` is created with this limit:

```python
def _create_limited_config(self) -> str | None:
    base = self._read_coro_config()
    base['max_token'] = self.max_context_tokens
    tf = tempfile.NamedTemporaryFile(mode='w', suffix='.json', prefix='coro_', delete=False)
    json.dump(base, tf, indent=2)
    tf.close()
    return tf.name
```

### The Lesson

When wrapping a CLI tool, you only control what you pass in. The tool may have its own state, caches, and context management. Document these limitations and provide alternatives (like a direct API backend) when full control is needed.

## Lesson 3: Trimming Minimums Matter

### The Bug

Context trimming with `--max-chars 50` still kept all messages because no trimming occurred.

### Root Cause

The trim loop had a minimum of 2 messages:

```python
# Original: wouldn't trim below 2 messages
while self._char_count > self.max_chars and len(self.messages) > 2:
    self._remove_oldest()
```

With only 2 messages in context (one user, one assistant), the `> 2` check prevented any trimming, even if those 2 messages exceeded the character limit.

### The Fix

```python
# Fixed: trim down to 1 message
while self._char_count > self.max_chars and len(self.messages) > 1:
    self._remove_oldest()
```

Additionally, retrieval-time enforcement was added as a second line of defense:

```python
def get_context(self) -> list[dict]:
    result = []
    total_chars = 0
    for msg in reversed(messages):  # Newest first
        if self.max_chars > 0 and total_chars + len(msg.content) > self.max_chars and result:
            break
        result.append(msg)
        total_chars += len(msg.content)
    result.reverse()
    return result
```

### The Lesson

Off-by-one errors in trim minimums can silently defeat the entire context management system. Aggressive limits (like `--max-chars 50` for testing) are valuable precisely because they expose these bugs — normal limits (100K tokens) would never trigger the issue.

## Lesson 4: Test with Extreme Values

### The Technique

To verify context trimming works, use deliberately extreme limits:

```bash
# Tell the model a secret, then set a tiny limit
python3 agent_loop.py -b openrouter --max-chars 50
> The secret code is banana-purple-7
> What is the secret code?
```

If the model can recall "banana-purple-7" with `--max-chars 50`, trimming is broken. If it says "I don't know," trimming is working.

This is more effective than using `--max-chars 10000` which might never trigger trimming in a short conversation. The extreme value forces the system into edge cases immediately.

### Watching Token Growth

Debug output showing token counts per iteration reveals whether context is growing or being trimmed:

```
# Growing (broken): tokens increase with each message
input: 247, output: 18    # First call
input: 377, output: 25    # Second call (grew by 130)
input: 581, output: 112   # Third call (grew by 204)

# Trimmed (working): tokens stay bounded
input: 247, output: 18    # First call
input: 195, output: 25    # Second call (trimmed)
input: 201, output: 30    # Third call (stable)
```

## Lesson 5: Deduplication at System Boundaries

### The Bug

When sending tool results back to an API backend, the same message appeared twice in the context — once from `context.add_message()` and once as the `message` parameter to `send_message()`.

### The Fix

A simple dedup check before adding the current message:

```python
# In send_message()
if not context or context[-1].get('content') != message:
    messages.append({"role": "user", "content": message})
```

### The Lesson

When data flows through multiple layers (context manager, agent loop, backend), it is easy to accidentally include the same information twice. Each layer should be defensive about deduplication, especially at the point where context is assembled for the API call.

## Lesson 6: The `stripped` Variable Ordering Bug

### The Bug

The coro backend's `_clean_output()` method crashed with `NameError: name 'stripped' is not defined`.

### Root Cause

```python
def _clean_output(self, output):
    plain = ansi_escape.sub('', output)
    # BUG: 'stripped' used here, but defined 3 lines later
    if len(stripped) < 20 and re.search(r'[\x1b\[\]]', stripped):
        return ''
    stripped = plain.strip()  # <-- defined too late
    return stripped
```

### The Fix

Move the assignment above the usage:

```python
def _clean_output(self, output):
    plain = ansi_escape.sub('', output)
    stripped = plain.strip()  # <-- define first
    if len(stripped) < 20 and re.search(r'[\x1b\[\]]', stripped):
        return ''
    return stripped
```

### The Lesson

Python's variable scoping can hide bugs. The function-level scope means `stripped` is technically in scope for the entire function body, but `NameError` occurs at runtime when the assignment hasn't been reached yet. Linters and type checkers catch this — use them.

## Lesson 7: Debug Output Is Not Optional

Several bugs in this project were only found because debug output was added:

| Bug | Debug Output That Found It |
|-----|---------------------------|
| Truthiness bug | `[DEBUG] ContextManager created: max_chars=0` |
| Context not trimming | `[DEBUG] get_context: 5 messages, total_chars=2341, max_chars=50` |
| Token growth | `[Tokens: input: 581, output: 112]` |
| Coro internal context | Token counts growing despite our trimming |

A pattern that works well: make debug output conditional on a `--debug` flag, but always have it available. When a bug occurs in production, you can reproduce with `--debug` enabled.

```python
if self.debug:
    print(f"[DEBUG] context: {len(messages)} msgs, "
          f"chars={self._char_count}, max={self.max_chars}", flush=True)
```

The `flush=True` matters. Without it, debug output can be buffered and appear at the wrong time relative to the bug, making diagnosis harder.

## Lesson 8: Design for Observability

The agent loop makes several internal decisions that are invisible by default:

- Which messages were trimmed from context
- Whether a tool call was auto-approved or user-confirmed
- How many tokens the context contains before and after trimming
- What the actual API request body looks like

Making these observable (through debug flags, token counts in output, and the `/cost` command) turns the agent from a black box into a debuggable system. Every time a bug was hard to find, it was because the relevant information was not being logged.

## Summary

| Lesson | Category | Takeaway |
|--------|----------|----------|
| Truthiness bug | Language gotcha | Use `is not None`, not `or`, for objects with `__len__` |
| CLI context | Architecture | CLI backends have their own state; API backends give full control |
| Trim minimums | Off-by-one | Use `> 1` not `> 2`; test with extreme values |
| Extreme values | Testing | `--max-chars 50` catches bugs that `--max-chars 10000` never will |
| Deduplication | Boundaries | Dedup at the point where context is assembled |
| Variable ordering | Language gotcha | Define variables before use; use linters |
| Debug output | Observability | Always available, conditional on a flag, with `flush=True` |
| Observability | Architecture | If you can't see it, you can't debug it |

These lessons apply to any system that orchestrates external services, manages state across multiple layers, and must work in constrained environments. The agent loop is a particularly fertile ground for these bugs because it sits at the intersection of API protocols, context management, subprocess execution, and terminal rendering.
