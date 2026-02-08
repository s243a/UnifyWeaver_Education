<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 7: Display and Terminal UX

## The Problem

API calls to language models can take 5-30 seconds. Without feedback, the user sees a frozen terminal and wonders if the program has hung. Additionally, tool-calling agents perform multiple operations in sequence (API call, tool execution, another API call), and the user needs to understand what is happening at each step.

The display system provides:
- Animated spinners during API calls
- Live status updates showing tool call progress
- Elapsed time indicators
- Graceful degradation for limited terminals

## Terminal Control with tput

The display system uses `tput` commands rather than hardcoded ANSI escape sequences. This is more portable across terminal types:

```python
class TerminalControl:
    """Terminal control using tput commands."""

    @staticmethod
    def cr() -> None:
        """Carriage return - move cursor to start of line."""
        tput_write('cr')

    @staticmethod
    def el() -> None:
        """Clear to end of line."""
        tput_write('el')

    @staticmethod
    def clear_line() -> None:
        """Move to start of line and clear it."""
        tput_write('cr')
        tput_write('el')

    @staticmethod
    def cols() -> int:
        """Get terminal width."""
        # Uses 'tput cols', defaults to 80 if unavailable
```

The `tput_write` function caches the output of `tput` commands at import time. If `tput` is unavailable (as in some CI environments), the system falls back gracefully.

Why tput instead of raw ANSI? Terminal emulators vary. Termux, xterm, tmux, and screen all handle escape codes slightly differently. `tput` queries the terminal's own capability database (`terminfo`), producing the correct sequences for the actual terminal in use.

## The Spinner

The spinner runs in a background thread while the main thread waits for the API response:

```python
class Spinner:
    FRAMES = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏']
    INTERVAL = 0.1  # seconds between frames

    def __init__(self, message: str = "Working"):
        self.message = message
        self._running = False
        self._thread = None
        self._lock = threading.Lock()
        self._start_time = 0.0

    def start(self) -> None:
        self._start_time = time.time()
        self._running = True
        self._thread = threading.Thread(target=self._animate, daemon=True)
        self._thread.start()

    def _animate(self) -> None:
        self._tc.hide_cursor()
        try:
            while self._running:
                with self._lock:
                    elapsed = time.time() - self._start_time
                    elapsed_str = f" ({elapsed:.0f}s)" if elapsed >= 2 else ""
                    frame = self.FRAMES[self._frame % len(self.FRAMES)]
                    display = self._truncate(self.message + elapsed_str, self._cols)
                    self._tc.clear_line()
                    sys.stdout.write(f"{frame} {display}")
                    sys.stdout.flush()
                    self._frame += 1
                time.sleep(self.INTERVAL)
        finally:
            self._tc.show_cursor()
```

Key implementation details:

| Detail | Reason |
|--------|--------|
| `daemon=True` thread | Thread dies automatically if main process exits |
| `threading.Lock` | Prevents interleaved output between spinner and status updates |
| Hidden cursor | Eliminates flickering as the spinner overwrites itself |
| Elapsed time after 2s | Short calls don't need a timer; long calls do |
| Terminal width truncation | Prevents line wrapping that breaks the spinner |

## Status Updates and Permanent Lines

The spinner supports live updates via the `on_status` callback. When a backend reports progress (like "Waiting for response..." or "reading file.py"), the spinner transitions the old message to a permanent line and shows the new one:

```python
def update(self, message: str) -> None:
    """Update spinner message, committing the old one as a permanent line."""
    with self._lock:
        if message != self.message:
            # Commit current message as permanent line
            self._tc.clear_line()
            sys.stdout.write(f"  {self.message}\n")
            sys.stdout.flush()
            self.message = message
```

This produces output like:

```
[Calling backend...]
  Waiting for response...
  [1] $ ls
  agent_loop.py
  backends
  ...

[Iteration 1: continuing with tool results...]
  Waiting for response...
  [1] $ ls -la
```

Each indented line was a spinner message that got committed as permanent output when the next status arrived. The user sees a chronological log of what the agent did.

## The on_status Callback Pattern

Backends report progress through an `on_status` callback passed via `**kwargs`:

```python
# In the agent loop
def on_status(status: str):
    if spinner:
        spinner.update(status)
    else:
        print(f"  {status}")

response = self.backend.send_message(
    user_input,
    self.context.get_context(),
    on_status=on_status
)
```

Inside the backend, status is reported at key points:

```python
# In OpenRouterBackend.send_message()
if on_status:
    on_status("Waiting for response...")

# ... after parsing each tool call
if on_status:
    desc = self._describe_tool_call(func.get('name'), arguments)
    on_status(f"[{len(tool_calls)}] {desc}")
```

The `_describe_tool_call` method produces readable summaries:

```python
def _describe_tool_call(self, tool_name: str, params: dict) -> str:
    if tool_name == 'read':
        return f"reading {os.path.basename(params.get('path', '?'))}"
    elif tool_name == 'bash':
        cmd = params.get('command', '?')
        return f"$ {cmd[:69]}..." if len(cmd) > 72 else f"$ {cmd}"
    return tool_name
```

## Append-Only Fallback

Some terminals cannot rewrite lines (e.g., piped output, logging systems, very basic serial consoles). The system detects this and falls back:

```python
class DisplayMode:
    @classmethod
    def supports_ncurses(cls) -> bool:
        """Check if terminal supports tput control."""
        return bool(_SEQUENCES.get('el'))  # Can we clear a line?
```

If tput is unavailable, an `AppendOnlySpinner` is used instead:

```python
class AppendOnlySpinner:
    def start(self) -> None:
        sys.stdout.write(f"[{self.message}]")
        sys.stdout.flush()
        # Background thread prints dots periodically

    def update(self, message: str) -> None:
        pass  # No-op in append mode

    def stop(self, final_message=None) -> None:
        sys.stdout.write(" done\n")
        sys.stdout.flush()
```

This produces simpler output:

```
[Calling backend...].... done
```

Not as informative, but functional everywhere.

## Stream-JSON: Live Tool Progress

Some CLI backends (claude-code, gemini) support structured JSON streaming. Instead of waiting for the full response, the backend reads NDJSON events line by line:

```python
# Claude-code with --output-format stream-json
process = subprocess.Popen(
    ["claude", "--output-format", "stream-json", "--verbose", prompt],
    stdout=subprocess.PIPE,
    text=True
)

for line in process.stdout:
    event = json.loads(line)
    if event.get('type') == 'tool_use':
        on_status(f"Using tool: {event['name']}")
    elif event.get('type') == 'text':
        content += event.get('text', '')
```

This gives the spinner real-time updates as the backend's own tool calls execute, bridging the gap between CLI backends (which handle tools internally) and API backends (which return tool calls for us to execute).

| Backend | Stream-JSON | Notes |
|---------|------------|-------|
| claude-code | Yes | `--output-format stream-json --verbose` |
| gemini | Yes | `-o stream-json` |
| coro | No | No structured output available |
| API backends | N/A | Direct HTTP, not CLI streaming |

## Summary

- The spinner runs in a background thread with a threading lock for safe updates
- `tput` commands are preferred over raw ANSI for portability
- Status callbacks (`on_status`) let backends report progress without coupling to the display
- Old status messages become permanent lines, creating a visible activity log
- `AppendOnlySpinner` provides graceful degradation for limited terminals
- Stream-JSON enables live tool progress for supported CLI backends

## Next Steps

[Chapter 8](08_lessons_from_production.md) covers real bugs, debugging techniques, and lessons learned from building and testing the agent loop.
