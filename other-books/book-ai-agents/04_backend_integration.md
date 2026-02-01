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

## Backend Factory

Create backends from configuration:

```python
def create_backend(config: dict) -> AgentBackend:
    """Create a backend from configuration."""
    backend_type = config.get("backend", "coro")

    if backend_type == "coro":
        return CoroBackend(
            command=config.get("command", "coro"),
            model=config.get("model")
        )
    elif backend_type == "claude":
        return ClaudeApiBackend(
            api_key=config.get("api_key"),
            model=config.get("model", "claude-sonnet-4-20250514")
        )
    elif backend_type == "openai":
        return OpenAiBackend(
            api_key=config.get("api_key"),
            model=config.get("model", "gpt-4o")
        )
    elif backend_type == "ollama-api":
        return OllamaApiBackend(
            host=config.get("host", "localhost"),
            port=config.get("port", 11434),
            model=config.get("model", "codellama")
        )
    # ... other backends

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

## Summary

- All backends implement a common interface
- CLI backends wrap external tools (coro, claude, ollama)
- API backends call REST endpoints directly
- A factory creates backends from configuration
- Streaming improves UX for supported backends

## Next Steps

[Chapter 5](05_building_custom_agents.md) shows how to build custom agents for specific domains.
