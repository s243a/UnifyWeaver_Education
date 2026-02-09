<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: Tool Handling

## The Tool-Use Pattern

When an LLM needs to take an action, it emits a **tool call** - a structured request that the agent loop executes.

```
LLM Output:
I'll read that file for you.

<tool_call>
{"name": "read", "path": "/path/to/file.py"}
</tool_call>
```

The agent loop must:
1. **Parse** tool calls from the response text
2. **Validate** the request
3. **Confirm** with the user (for destructive operations)
4. **Execute** the tool
5. **Return** results to the LLM

## Built-in Tools

The agent loop provides four core tools:

### Bash

Execute shell commands:

```json
{
  "name": "bash",
  "command": "ls -la /home/user"
}
```

**Safety**: Always requires confirmation unless `--auto-tools` is set.

### Read

Read file contents:

```json
{
  "name": "read",
  "path": "/path/to/file.py",
  "offset": 0,
  "limit": 100
}
```

**Safety**: Generally safe, no confirmation needed.

### Write

Create or overwrite a file:

```json
{
  "name": "write",
  "path": "/path/to/newfile.py",
  "content": "print('Hello, World!')"
}
```

**Safety**: Requires confirmation (creates/overwrites files).

### Edit

Modify an existing file using search/replace:

```json
{
  "name": "edit",
  "path": "/path/to/file.py",
  "old_string": "old code",
  "new_string": "new code"
}
```

**Safety**: Requires confirmation (modifies files).

## Parsing Tool Calls

Different LLMs use different formats. The parser handles multiple patterns:

### XML-style Tags

```python
def parse_xml_tools(response: str) -> list[ToolCall]:
    """Parse <tool_call>...</tool_call> blocks."""
    pattern = r'<tool_call>\s*(\{.*?\})\s*</tool_call>'
    matches = re.findall(pattern, response, re.DOTALL)
    return [ToolCall(**json.loads(m)) for m in matches]
```

### Claude's Native Format

```python
def parse_claude_tools(response: str) -> list[ToolCall]:
    """Parse Claude's tool_use blocks."""
    # Claude uses structured tool_use in API responses
    # CLI backends often emit markdown-style blocks
    pattern = r'```tool\s*\n(\{.*?\})\n```'
    ...
```

### Function Call Format

```python
def parse_function_calls(response: str) -> list[ToolCall]:
    """Parse function_name(args) style."""
    pattern = r'(\w+)\((.*?)\)'
    ...
```

## Safe Execution

### Confirmation Prompts

For destructive operations, show what will happen:

```python
def requires_confirmation(tool: ToolCall) -> bool:
    """Check if this tool needs user approval."""
    if tool.name == "bash":
        return True  # Always confirm shell commands
    if tool.name in ("write", "edit"):
        return True  # Always confirm file modifications
    return False

def confirm_tool(tool: ToolCall) -> bool:
    """Ask user to confirm tool execution."""
    print(f"\n⚠️  Tool: {tool.name}")

    if tool.name == "bash":
        print(f"   Command: {tool.command}")
    elif tool.name == "write":
        print(f"   File: {tool.path}")
        print(f"   Content preview: {tool.content[:100]}...")
    elif tool.name == "edit":
        print(f"   File: {tool.path}")
        print(f"   Replace: {tool.old_string[:50]}...")
        print(f"   With: {tool.new_string[:50]}...")

    response = input("\nExecute? [y/N]: ").strip().lower()
    return response in ('y', 'yes')
```

### Command Blocklist

The agent loop maintains a built-in blocklist of dangerous command patterns. These are checked *before* the confirmation prompt — even with `--auto-tools`, blocked commands are rejected:

```python
_BLOCKED_COMMAND_PATTERNS = [
    (r'\brm\s+-[rf]*\s+/', "rm with absolute path and force/recursive flags"),
    (r'\bmkfs\b', "filesystem format"),
    (r'\bdd\s+.*of=/dev/', "write to block device"),
    (r'>\s*/dev/sd', "redirect to block device"),
    (r'\bcurl\b.*\|\s*(?:ba)?sh', "pipe remote script to shell"),
    (r'\bwget\b.*\|\s*(?:ba)?sh', "pipe remote script to shell"),
    (r'\bchmod\s+777\b', "world-writable permissions"),
    (r':\(\)\s*\{\s*:\|:\s*&\s*\}\s*;', "fork bomb"),
    (r'\b>\s*/etc/', "overwrite system config"),
]

def is_command_blocked(command, extra_blocked=None, extra_allowed=None):
    """Return reason if command is blocked, None if allowed."""
    # Allowlist checked first — explicit allows override blocks
    if extra_allowed:
        for pattern in extra_allowed:
            if re.search(pattern, command, re.IGNORECASE):
                return None
    # User-provided extra blocks
    if extra_blocked:
        for pattern in extra_blocked:
            if re.search(pattern, command, re.IGNORECASE):
                return f"Blocked by config: matches '{pattern}'"
    # Built-in blocks
    for pattern, description in _BLOCKED_COMMAND_PATTERNS:
        if re.search(pattern, command, re.IGNORECASE):
            return f"Blocked: {description}"
    return None
```

The blocklist is customizable via `uwsal.json` — see [Chapter 5](05_building_custom_agents.md) for configuration details.

### Path Validation

All file operations (read, write, edit) pass through `validate_path()` before execution. This resolves symlinks and `..` components with `os.path.realpath()`, then checks against blocklists:

```python
_BLOCKED_PATHS = {'/etc/shadow', '/etc/gshadow', '/etc/sudoers'}
_BLOCKED_PREFIXES = ('/proc/', '/sys/', '/dev/')
_BLOCKED_HOME_PATTERNS = ('.ssh/', '.gnupg/', '.aws/', '.config/gcloud/', '.env', '.netrc', '.npmrc')

def validate_path(raw_path, working_dir, extra_blocked=None, extra_allowed=None):
    """Resolve and validate a file path. Returns (resolved, error_or_None)."""
    resolved = os.path.realpath(
        raw_path if raw_path.startswith('/') else os.path.join(working_dir, raw_path)
    )
    # Check both raw and resolved paths (important on Termux where
    # /etc -> /system/etc, so realpath differs from the literal)
    # Allow list checked first, then block list, then built-in defaults
    ...
```

The dual-path check matters on Termux and other environments with symlinked system directories. Without it, an LLM could request `/etc/shadow` and bypass the blocklist because `realpath` resolves it to `/system/etc/shadow`.

Like the command blocklist, path blocklists are customizable via `uwsal.json`:

```json
{
  "security": {
    "blocked_paths": ["/data/production/"],
    "allowed_paths": ["/etc/hosts"]
  }
}
```

### Security Profiles

The path validation and command blocklist are controlled by security profiles:

| Profile | Path validation | Command blocklist | Notes |
|---------|----------------|-------------------|-------|
| `open` | Off | Off | No checks — for trusted environments |
| `cautious` | On | On | **Default** — blocks dangerous paths and commands |
| `sandboxed` | On | On | Adds proot filesystem isolation (future) |
| `paranoid` | On | On | Adds audit logging (future) |

```bash
# Default: cautious (path + command validation)
python3 agent_loop.py -b openrouter "Read /etc/shadow"
# → [Security] Blocked: /etc/shadow is a sensitive system file

# Disable all checks
python3 agent_loop.py -b openrouter --no-security "Read /etc/shadow"

# Explicit profile
python3 agent_loop.py -b openrouter --security-profile paranoid "..."
```

The `--no-security` flag is the explicit escape hatch for when validation interferes with a specific workflow. It's equivalent to `--security-profile open`.

## Tool Execution

```python
def execute_tool(tool: ToolCall) -> ToolResult:
    """Execute a tool and return the result."""
    try:
        if tool.name == "bash":
            return execute_bash(tool)
        elif tool.name == "read":
            return execute_read(tool)
        elif tool.name == "write":
            return execute_write(tool)
        elif tool.name == "edit":
            return execute_edit(tool)
        else:
            return ToolResult(
                success=False,
                error=f"Unknown tool: {tool.name}"
            )
    except Exception as e:
        return ToolResult(
            success=False,
            error=f"Tool execution failed: {str(e)}"
        )
```

### Bash Execution

```python
def execute_bash(tool: ToolCall) -> ToolResult:
    """Execute a shell command."""
    try:
        result = subprocess.run(
            tool.command,
            shell=True,
            capture_output=True,
            text=True,
            timeout=120  # 2 minute timeout
        )

        output = result.stdout
        if result.stderr:
            output += f"\nSTDERR:\n{result.stderr}"

        return ToolResult(
            success=result.returncode == 0,
            output=output,
            exit_code=result.returncode
        )
    except subprocess.TimeoutExpired:
        return ToolResult(
            success=False,
            error="Command timed out after 120 seconds"
        )
```

### File Operations

```python
def execute_read(tool: ToolCall) -> ToolResult:
    """Read a file's contents."""
    try:
        with open(tool.path, 'r') as f:
            if hasattr(tool, 'offset') and tool.offset:
                lines = f.readlines()
                start = tool.offset
                end = start + (tool.limit or 100)
                content = ''.join(lines[start:end])
            else:
                content = f.read()

        return ToolResult(success=True, output=content)
    except FileNotFoundError:
        return ToolResult(success=False, error=f"File not found: {tool.path}")


def execute_write(tool: ToolCall) -> ToolResult:
    """Write content to a file."""
    try:
        # Create parent directories if needed
        Path(tool.path).parent.mkdir(parents=True, exist_ok=True)

        with open(tool.path, 'w') as f:
            f.write(tool.content)

        return ToolResult(
            success=True,
            output=f"Wrote {len(tool.content)} bytes to {tool.path}"
        )
    except Exception as e:
        return ToolResult(success=False, error=str(e))


def execute_edit(tool: ToolCall) -> ToolResult:
    """Edit a file using search/replace."""
    try:
        with open(tool.path, 'r') as f:
            content = f.read()

        if tool.old_string not in content:
            return ToolResult(
                success=False,
                error=f"String not found in {tool.path}"
            )

        new_content = content.replace(tool.old_string, tool.new_string, 1)

        with open(tool.path, 'w') as f:
            f.write(new_content)

        return ToolResult(
            success=True,
            output=f"Edited {tool.path}"
        )
    except FileNotFoundError:
        return ToolResult(success=False, error=f"File not found: {tool.path}")
```

## Result Formatting

Tool results are added to context for the LLM:

```python
def format_tool_result(tool: ToolCall, result: ToolResult) -> str:
    """Format tool result for inclusion in context."""
    if result.success:
        return f"""Tool executed: {tool.name}
Result:
{result.output}"""
    else:
        return f"""Tool failed: {tool.name}
Error: {result.error}"""
```

## Error Recovery

When tools fail, the LLM should be informed so it can adapt:

```python
# Tool fails
result = execute_tool(tool)
if not result.success:
    # Add error to context
    context.append({
        "role": "tool_result",
        "content": f"Error: {result.error}"
    })
    # LLM will see the error and try a different approach
```

Example conversation:

```
User: Read the config file
LLM: <tool_call>{"name": "read", "path": "/etc/config.json"}</tool_call>
Tool Result: Error: File not found: /etc/config.json
LLM: That file doesn't exist. Let me check what config files are available.
<tool_call>{"name": "bash", "command": "ls /etc/*.json"}</tool_call>
```

## Summary

- Tools let agents take real actions (run commands, edit files)
- Always parse tool calls from potentially messy LLM output
- Require confirmation for destructive operations
- Path validation with `realpath()` defeats traversal attacks and blocks credential files
- Command blocklist catches dangerous patterns before execution
- Security profiles (open/cautious/sandboxed/paranoid) make checks configurable
- Blocklists are customizable via `uwsal.json` with allow overriding block
- Handle errors gracefully — inform the LLM so it can adapt

## Next Steps

[Chapter 4](04_backend_integration.md) covers integrating multiple LLM backends.
