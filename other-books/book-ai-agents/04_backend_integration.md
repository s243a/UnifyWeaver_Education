<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 4: Backend Integration

## The Backend Interface

All LLM backends implement a common interface, allowing the agent loop to work with any provider:

```python
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Optional

@dataclass
class AgentResponse:
    """Response from an LLM backend."""
    content: str
    tool_calls: list = None
    input_tokens: int = 0
    output_tokens: int = 0
    model: str = ""
    finish_reason: str = ""

class AgentBackend(ABC):
    """Abstract base for all LLM backends."""

    @abstractmethod
    def send_message(self, message: str, context: list) -> AgentResponse:
        """Send a message with context, return response."""
        pass

    @abstractmethod
    def parse_tool_calls(self, response: str) -> list:
        """Extract tool calls from response text."""
        pass

    def supports_streaming(self) -> bool:
        """Whether this backend supports streaming output."""
        return False

    def stream_message(self, message: str, context: list):
        """Stream response chunks. Override if supported."""
        raise NotImplementedError
```

## CLI Backends

CLI backends call external command-line tools.

### Coro Backend

```python
class CoroBackend(AgentBackend):
    """Coro-code CLI backend."""

    def __init__(self, command: str = "coro", model: str = None):
        self.command = command
        self.model = model

    def send_message(self, message: str, context: list) -> AgentResponse:
        # Format context into prompt
        prompt = self._format_prompt(message, context)

        # Build command
        cmd = [self.command, "--verbose"]
        if self.model:
            cmd.extend(["--model", self.model])
        cmd.append(prompt)

        # Execute
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=300
        )

        # Parse output
        content = self._clean_output(result.stdout)
        tokens = self._parse_tokens(result.stdout)

        return AgentResponse(
            content=content,
            input_tokens=tokens.get("input", 0),
            output_tokens=tokens.get("output", 0)
        )

    def _format_prompt(self, message: str, context: list) -> str:
        """Format message with conversation history."""
        if not context:
            return message

        # Include recent context
        history = []
        for msg in context[-10:]:  # Last 10 messages
            role = msg.get("role", "user")
            content = msg.get("content", "")
            history.append(f"{role.title()}: {content}")

        return f"""Previous conversation:
{chr(10).join(history)}

Current request: {message}"""

    def _clean_output(self, output: str) -> str:
        """Remove escape codes and formatting."""
        # Strip ANSI escape codes
        ansi_escape = re.compile(r'\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])')
        return ansi_escape.sub('', output)
```

### Claude Code Backend

```python
class ClaudeCodeBackend(AgentBackend):
    """Claude Code CLI backend."""

    def __init__(self, command: str = "claude"):
        self.command = command

    def send_message(self, message: str, context: list) -> AgentResponse:
        prompt = self._format_prompt(message, context)

        # Claude Code uses --print for non-interactive output
        result = subprocess.run(
            [self.command, "--print", prompt],
            capture_output=True,
            text=True,
            timeout=300
        )

        return AgentResponse(content=result.stdout)
```

### Ollama CLI Backend

```python
class OllamaCliBackend(AgentBackend):
    """Ollama CLI backend using 'ollama run'."""

    def __init__(self, model: str = "codellama"):
        self.model = model

    def send_message(self, message: str, context: list) -> AgentResponse:
        prompt = self._format_prompt(message, context)

        # Ollama run reads from stdin
        result = subprocess.run(
            ["ollama", "run", self.model],
            input=prompt,
            capture_output=True,
            text=True,
            timeout=300
        )

        return AgentResponse(content=result.stdout)
```

## API Backends

API backends call REST endpoints directly.

### Claude API Backend

```python
class ClaudeApiBackend(AgentBackend):
    """Anthropic Claude API backend."""

    def __init__(
        self,
        api_key: str = None,
        model: str = "claude-sonnet-4-20250514"
    ):
        self.api_key = api_key or os.environ.get("ANTHROPIC_API_KEY")
        self.model = model
        self.client = anthropic.Anthropic(api_key=self.api_key)

    def send_message(self, message: str, context: list) -> AgentResponse:
        # Convert context to Claude message format
        messages = self._convert_context(context)
        messages.append({"role": "user", "content": message})

        response = self.client.messages.create(
            model=self.model,
            max_tokens=4096,
            messages=messages
        )

        return AgentResponse(
            content=response.content[0].text,
            input_tokens=response.usage.input_tokens,
            output_tokens=response.usage.output_tokens,
            model=self.model,
            finish_reason=response.stop_reason
        )

    def supports_streaming(self) -> bool:
        return True

    def stream_message(self, message: str, context: list):
        """Stream response chunks."""
        messages = self._convert_context(context)
        messages.append({"role": "user", "content": message})

        with self.client.messages.stream(
            model=self.model,
            max_tokens=4096,
            messages=messages
        ) as stream:
            for text in stream.text_stream:
                yield text
```

### OpenAI Backend

```python
class OpenAiBackend(AgentBackend):
    """OpenAI API backend."""

    def __init__(
        self,
        api_key: str = None,
        model: str = "gpt-4o"
    ):
        self.api_key = api_key or os.environ.get("OPENAI_API_KEY")
        self.model = model
        self.client = openai.OpenAI(api_key=self.api_key)

    def send_message(self, message: str, context: list) -> AgentResponse:
        messages = self._convert_context(context)
        messages.append({"role": "user", "content": message})

        response = self.client.chat.completions.create(
            model=self.model,
            messages=messages
        )

        choice = response.choices[0]
        return AgentResponse(
            content=choice.message.content,
            input_tokens=response.usage.prompt_tokens,
            output_tokens=response.usage.completion_tokens,
            model=self.model,
            finish_reason=choice.finish_reason
        )

    def supports_streaming(self) -> bool:
        return True

    def stream_message(self, message: str, context: list):
        messages = self._convert_context(context)
        messages.append({"role": "user", "content": message})

        stream = self.client.chat.completions.create(
            model=self.model,
            messages=messages,
            stream=True
        )

        for chunk in stream:
            if chunk.choices[0].delta.content:
                yield chunk.choices[0].delta.content
```

### Ollama API Backend

```python
class OllamaApiBackend(AgentBackend):
    """Ollama REST API backend."""

    def __init__(
        self,
        host: str = "localhost",
        port: int = 11434,
        model: str = "codellama"
    ):
        self.base_url = f"http://{host}:{port}"
        self.model = model

    def send_message(self, message: str, context: list) -> AgentResponse:
        prompt = self._format_prompt(message, context)

        response = requests.post(
            f"{self.base_url}/api/generate",
            json={
                "model": self.model,
                "prompt": prompt,
                "stream": False
            }
        )

        data = response.json()
        return AgentResponse(
            content=data.get("response", ""),
            model=self.model
        )
```

### Gemini CLI Backend

```python
class GeminiBackend(AgentBackend):
    """Google Gemini CLI backend."""

    def __init__(self, command: str = "gemini", model: str = None):
        self.command = command
        self.model = model

    def send_message(self, message: str, context: list) -> AgentResponse:
        prompt = self._format_prompt(message, context)

        cmd = [self.command]
        if self.model:
            cmd.extend(["--model", self.model])
        cmd.append(prompt)

        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=300
        )

        content = self._clean_output(result.stdout)
        return AgentResponse(content=content)
```

Gemini also supports stream-json output (`-o stream-json`) for live tool call progress, similar to claude-code's `--output-format stream-json`.

## API Backends (No External Dependencies)

### OpenRouter Backend

OpenRouter provides an OpenAI-compatible API that aggregates hundreds of models. The backend uses Python's `urllib` directly, requiring no pip dependencies:

```python
class OpenRouterBackend(AgentBackend):
    """OpenRouter API backend (OpenAI-compatible, no pip deps)."""

    def __init__(
        self,
        api_key: str = None,
        model: str = None,
        base_url: str = "https://openrouter.ai/api/v1",
        max_tokens: int = 4096,
        system_prompt: str = None,
        tools: list = None,
    ):
        # API key and model are resolved externally by the backend factory
        # using resolve_api_key() and read_config_cascade() — see Chapter 5
        self.api_key = api_key
        self.model = model or 'moonshotai/kimi-k2.5'
        self.base_url = base_url
        self.max_tokens = max_tokens
        self.system_prompt = system_prompt or "You are a helpful AI coding assistant."
        self.tool_schemas = tools  # None = no tools

    def send_message(self, message: str, context: list, **kwargs) -> AgentResponse:
        on_status = kwargs.get('on_status')

        # Build messages array
        messages = [{"role": "system", "content": self.system_prompt}]
        for msg in context:
            role = msg.get('role')
            if role in ('user', 'assistant'):
                messages.append({"role": role, "content": msg['content']})
        if not context or context[-1].get('content') != message:
            messages.append({"role": "user", "content": message})

        # Build request body
        body = {
            "model": self.model,
            "messages": messages,
            "max_tokens": self.max_tokens,
        }
        if self.tool_schemas:
            body["tools"] = self.tool_schemas
            body["tool_choice"] = "auto"

        # POST using urllib (no pip deps)
        url = f"{self.base_url.rstrip('/')}/chat/completions"
        req = Request(
            url,
            data=json.dumps(body).encode('utf-8'),
            headers={
                'Content-Type': 'application/json',
                'Authorization': f'Bearer {self.api_key}',
            },
            method='POST'
        )

        with urlopen(req, timeout=300) as resp:
            data = json.loads(resp.read().decode('utf-8'))

        # Parse response
        choice = data['choices'][0]
        msg = choice.get('message', {})
        content = msg.get('content') or ''

        # Parse tool calls (OpenAI format)
        tool_calls = []
        for tc in msg.get('tool_calls', []):
            if tc.get('type') == 'function':
                func = tc['function']
                tool_calls.append(ToolCall(
                    name=func['name'],
                    arguments=json.loads(func.get('arguments', '{}')),
                    id=tc.get('id', '')
                ))

        # Token usage
        usage = data.get('usage', {})
        return AgentResponse(
            content=content,
            tool_calls=tool_calls,
            input_tokens=usage.get('prompt_tokens', 0),
            output_tokens=usage.get('completion_tokens', 0),
        )
```

Key design decisions:

| Decision | Rationale |
|----------|-----------|
| `urllib` instead of `openai` SDK | No pip dependencies; works in Termux out of the box |
| Auto-read `~/coro.json` | Shares API key and model config with the coro CLI |
| Optional `tools` parameter | `None` means no tool schemas sent; model can only reply with text |
| `tool_choice: "auto"` | Model decides when to call tools vs. respond with text |

The tool schemas use the OpenAI function calling format:

```python
DEFAULT_TOOL_SCHEMAS = [
    {
        "type": "function",
        "function": {
            "name": "bash",
            "description": "Execute a bash command and return its output.",
            "parameters": {
                "type": "object",
                "properties": {
                    "command": {"type": "string", "description": "The bash command to execute"}
                },
                "required": ["command"]
            }
        }
    },
    # ... read, write, edit follow the same pattern
]
```

Usage:

```bash
# Basic query (no tools)
python3 agent_loop.py -b openrouter "What is 2+2?"

# With tool calling enabled
python3 agent_loop.py -b openrouter --auto-tools "List files in the current directory"

# Context limits work because we control the full message array
python3 agent_loop.py -b openrouter --max-chars 50
```

## Backend Factory

Create backends from configuration. For API backends, API keys are resolved through the unified config cascade (see [Chapter 5](05_building_custom_agents.md#api-key-resolution)):

```python
from config import resolve_api_key, read_config_cascade

def create_backend(agent_config, no_fallback=False) -> AgentBackend:
    """Create a backend from agent configuration."""
    backend_type = agent_config.backend

    if backend_type == "coro":
        return CoroBackend(command=agent_config.command or "coro")
    elif backend_type == "claude":
        api_key = resolve_api_key('claude', agent_config.api_key, no_fallback)
        return ClaudeApiBackend(api_key=api_key, model=agent_config.model)
    elif backend_type == "openai":
        api_key = resolve_api_key('openai', agent_config.api_key, no_fallback)
        return OpenAiBackend(api_key=api_key, model=agent_config.model)
    elif backend_type == "openrouter":
        api_key = resolve_api_key('openrouter', agent_config.api_key, no_fallback)
        cascade = read_config_cascade(no_fallback)
        return OpenRouterBackend(
            api_key=api_key,
            model=agent_config.model or cascade.get('model'),
            base_url=cascade.get('base_url', 'https://openrouter.ai/api/v1'),
            tools=DEFAULT_TOOL_SCHEMAS if agent_config.tools else None,
        )
    elif backend_type == "gemini":
        return GeminiBackend(command=agent_config.command or "gemini")
    elif backend_type == "ollama-api":
        return OllamaApiBackend(
            host=agent_config.host or "localhost",
            port=agent_config.port or 11434,
            model=agent_config.model or "codellama",
        )
    raise ValueError(f"Unknown backend: {backend_type}")
```

## Streaming Support

Streaming provides better UX for long responses:

```python
def run_with_streaming(backend: AgentBackend, message: str, context: list):
    """Run with streaming if supported."""
    if backend.supports_streaming():
        full_response = ""
        for chunk in backend.stream_message(message, context):
            print(chunk, end="", flush=True)
            full_response += chunk
        print()  # Newline at end
        return AgentResponse(content=full_response)
    else:
        # Fall back to non-streaming
        response = backend.send_message(message, context)
        print(response.content)
        return response
```

## Backend Summary

| Backend | Type | Dependencies | Tool Calling | Context Control |
|---------|------|-------------|--------------|-----------------|
| `coro` | CLI | coro CLI | Backend handles | Partial (backend has own context) |
| `claude-code` | CLI | claude CLI | Backend handles | Partial |
| `gemini` | CLI | gemini CLI | Backend handles | Partial |
| `ollama-cli` | CLI | ollama CLI | None | Full |
| `openrouter` | API | None (urllib) | Agent loop handles | Full |
| `claude` | API | `anthropic` pip | Agent loop handles | Full |
| `openai` | API | `openai` pip | Agent loop handles | Full |
| `ollama-api` | API | None (requests) | Agent loop handles | Full |

Key points:
- All backends implement a common interface (`AgentBackend`)
- CLI backends wrap external tools; API backends call REST endpoints directly
- OpenRouter uses only `urllib` (no pip deps), making it ideal for constrained environments
- CLI backends manage their own context internally; API backends give full control to the agent loop
- A factory pattern creates backends from YAML/dict configuration
- Streaming improves UX for supported backends

## Next Steps

[Chapter 5](05_building_custom_agents.md) shows how to build custom agents for specific domains.
[Chapter 6](06_cost_tracking.md) covers API cost tracking and pricing.
