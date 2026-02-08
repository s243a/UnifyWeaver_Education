<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 5: Building Custom Agents

## Overview

Custom agents combine:
- A **backend** (which LLM to use)
- A **system prompt** (how the agent should behave)
- **Configuration** (auto-tools, iterations, model settings)
- Optional **skills** (domain-specific capabilities)

This chapter shows how to build agents for specific tasks.

## Agent Configuration

### YAML Format

Create `agents.yaml` to define your agents:

```yaml
agents:
  # Fast coding agent
  coder:
    description: "Quick coding tasks"
    backend: coro
    auto_tools: true
    max_iterations: 0  # Unlimited

  # Careful reviewer
  reviewer:
    description: "Code review with explanations"
    backend: claude
    model: claude-sonnet-4-20250514
    auto_tools: false
    system_prompt: |
      You are a careful code reviewer. For each piece of code:
      1. Check for bugs and edge cases
      2. Evaluate readability and maintainability
      3. Suggest specific improvements
      Be constructive but thorough.

  # Local private agent
  private:
    description: "Local Ollama for sensitive code"
    backend: ollama-api
    model: codellama:13b
    host: localhost
    port: 11434

  # Documentation writer
  docs:
    description: "Generate documentation"
    backend: claude
    model: claude-sonnet-4-20250514
    system_prompt: |
      You are a technical writer. Generate clear, concise documentation.
      Use proper markdown formatting. Include examples where helpful.
      Focus on what users need to know, not implementation details.
```

### JSON Format

Equivalent in JSON:

```json
{
  "agents": {
    "coder": {
      "description": "Quick coding tasks",
      "backend": "coro",
      "auto_tools": true,
      "max_iterations": 0
    },
    "reviewer": {
      "description": "Code review with explanations",
      "backend": "claude",
      "model": "claude-sonnet-4-20250514",
      "auto_tools": false,
      "system_prompt": "You are a careful code reviewer..."
    }
  }
}
```

## API Key Resolution

Agent variants in `agents.yaml` define *which* backend to use, but API backends also need credentials. Rather than hardcoding keys in every config, the agent loop resolves API keys through a priority chain.

### The Priority Chain

```
1. --api-key CLI argument           (highest priority)
2. Backend-specific env var         (OPENROUTER_API_KEY, ANTHROPIC_API_KEY, etc.)
3. uwsal.json keys.<backend>        (per-provider key)
3b. uwsal.json api_key              (top-level fallback)
4. coro.json api_key                (legacy fallback, skipped with --no-fallback)
5. Standard file locations          (~/.anthropic/api_key, ~/.openai/api_key)
```

This means you can set up credentials once and every backend finds them automatically.

### uwsal.json — Primary Config File

`uwsal.json` (Unify Weaver, Simple Agent Loop) is the primary config file. It's searched in the current directory first, then in `~/`:

```json
{
  "api_key": "sk-or-...",
  "model": "moonshotai/kimi-k2.5",
  "base_url": "https://openrouter.ai/api/v1",
  "keys": {
    "openrouter": "sk-or-...",
    "anthropic": "sk-ant-...",
    "openai": "sk-..."
  }
}
```

- **Top-level `api_key`** — used by any backend that doesn't have a provider-specific key
- **`keys` object** — per-provider keys, looked up by backend type
- **`model`** and **`base_url`** — defaults for API backends

### Config File Search Order

```
1. CWD/uwsal.json
2. ~/uwsal.json
3. CWD/coro.json     (skipped with --no-fallback)
4. ~/coro.json       (skipped with --no-fallback)
```

`coro.json` is the legacy config from the coro CLI tool. It's still supported as a fallback so existing setups keep working without changes.

### Implementation

The resolution logic lives in `config.py`:

```python
def read_config_cascade(no_fallback: bool = False) -> dict:
    """Read config from uwsal.json, falling back to coro.json."""
    candidates = ['uwsal.json', os.path.expanduser('~/uwsal.json')]
    if not no_fallback:
        candidates += ['coro.json', os.path.expanduser('~/coro.json')]
    for path in candidates:
        try:
            with open(path) as f:
                return json.load(f)
        except (FileNotFoundError, json.JSONDecodeError):
            continue
    return {}

def resolve_api_key(backend_type: str, cli_key: str | None = None,
                    no_fallback: bool = False) -> str | None:
    """Resolve API key with full priority chain."""
    if cli_key:
        return cli_key

    env_vars = {
        'openrouter': 'OPENROUTER_API_KEY',
        'claude': 'ANTHROPIC_API_KEY',
        'openai': 'OPENAI_API_KEY',
        'gemini': 'GEMINI_API_KEY',
    }
    env_var = env_vars.get(backend_type)
    if env_var:
        val = os.environ.get(env_var)
        if val:
            return val

    config = read_config_cascade(no_fallback)
    provider_key = config.get('keys', {}).get(backend_type)
    if provider_key:
        return provider_key
    if config.get('api_key'):
        return config['api_key']

    # Standard file locations
    file_locations = {'claude': '~/.anthropic/api_key', 'openai': '~/.openai/api_key'}
    loc = file_locations.get(backend_type)
    if loc:
        try:
            with open(os.path.expanduser(loc)) as f:
                return f.read().strip()
        except FileNotFoundError:
            pass

    return None
```

### The `--no-fallback` Flag

By default, the agent loop checks `coro.json` as a fallback for both API keys and model/base_url settings. The `--no-fallback` flag skips `coro.json` entirely — only `uwsal.json` is consulted. This is useful when you want to ensure a clean separation from legacy coro configs:

```bash
# Uses uwsal.json only — fails if no uwsal.json exists
python3 agent_loop.py -b openrouter --no-fallback "What is 2+2?"

# Default: checks uwsal.json first, falls back to coro.json
python3 agent_loop.py -b openrouter "What is 2+2?"
```

### Design Note: Why Backends Don't Resolve Keys

An earlier version had each backend class discovering its own API key (e.g., `OpenRouterBackend._read_coro_config()`). This scattered the resolution logic across multiple files and made it impossible to enforce `--no-fallback` consistently.

The current design resolves everything in the backend factory *before* constructing the backend. The backend constructor just takes what it's given:

```python
# Old: backend discovers its own key (scattered logic)
class OpenRouterBackend:
    def __init__(self, api_key=None, ...):
        coro_config = self._read_coro_config()
        self.api_key = api_key or os.environ.get('OPENROUTER_API_KEY') or coro_config.get('api_key')

# New: factory resolves key, backend just stores it (centralized logic)
class OpenRouterBackend:
    def __init__(self, api_key=None, ...):
        self.api_key = api_key
```

## System Prompts

The system prompt defines the agent's behavior and expertise.

### Basic Structure

```yaml
system_prompt: |
  # Role
  You are a [role description].

  # Capabilities
  You can:
  - [capability 1]
  - [capability 2]

  # Guidelines
  - [guideline 1]
  - [guideline 2]

  # Output Format
  [How to format responses]
```

### Example: Security Auditor

```yaml
security-auditor:
  backend: claude
  model: claude-opus-4-20250514
  system_prompt: |
    You are a security-focused code auditor.

    For each code file or snippet, analyze:
    1. **Input Validation**: Check for injection vulnerabilities
    2. **Authentication**: Verify proper auth checks
    3. **Data Handling**: Look for sensitive data exposure
    4. **Dependencies**: Flag known vulnerable packages

    Output format:
    - List findings by severity (Critical, High, Medium, Low)
    - Include file:line references
    - Provide fix recommendations

    Be thorough but avoid false positives.
```

### Example: Test Generator

```yaml
test-writer:
  backend: claude
  model: claude-sonnet-4-20250514
  auto_tools: true
  system_prompt: |
    You are a test-driven development expert.

    When given code, generate comprehensive tests:
    1. Unit tests for each public function
    2. Edge case tests (empty input, nulls, boundaries)
    3. Integration tests if multiple components interact

    Use pytest for Python, Jest for JavaScript.
    Follow AAA pattern: Arrange, Act, Assert.
    Aim for >80% code coverage.
```

## Skills

Skills are reusable capability modules that can be attached to any agent.

### Creating a Skill

Create a file like `skills/git-expert.md`:

```markdown
# Git Expert Skill

You have deep expertise with Git and GitHub workflows.

## Capabilities

- Explain git operations clearly
- Help resolve merge conflicts
- Suggest branching strategies
- Write good commit messages

## Commands You Know

- git status, diff, log, show
- git add, commit, push, pull
- git branch, checkout, merge, rebase
- git stash, cherry-pick, bisect
- gh pr, gh issue (GitHub CLI)

## Best Practices

- Always check status before committing
- Write descriptive commit messages (what and why)
- Use feature branches
- Rebase for clean history, merge for collaboration
```

### Attaching Skills

```yaml
agents:
  git-helper:
    backend: claude
    skills:
      - skills/git-expert.md
      - skills/bash-expert.md
```

The skills content is prepended to the system prompt.

## Prompt Templates

Templates allow parameterized prompts:

### Defining Templates

```yaml
templates:
  review:
    description: "Code review template"
    template: |
      Please review this {language} code for {focus}:

      ```{language}
      {code}
      ```

  explain:
    description: "Explain code"
    template: |
      Explain this code at a {level} level:

      {code}
```

### Using Templates

```bash
# Interactive
/template use review language=python focus=security code="$(cat app.py)"

# Command line
python3 agent_loop.py -a reviewer --template review \
  language=python focus="error handling" code="$(cat main.py)"
```

## Command Aliases

Create shortcuts for common operations:

```yaml
aliases:
  r: review
  t: test
  d: docs
  yolo: backend coro; auto-tools; iterations 0
```

Usage:
```bash
> /yolo
# Equivalent to: /backend coro then /auto-tools then /iterations 0
```

## Domain-Specific Agents

### DevOps Agent

```yaml
devops:
  backend: claude
  model: claude-sonnet-4-20250514
  auto_tools: true
  system_prompt: |
    You are a DevOps expert specializing in:
    - Docker and container orchestration
    - CI/CD pipelines (GitHub Actions, GitLab CI)
    - Infrastructure as Code (Terraform, Ansible)
    - Kubernetes deployments
    - Monitoring and logging

    Always consider:
    - Security best practices
    - Cost optimization
    - Scalability
    - Disaster recovery

    Prefer declarative over imperative approaches.
```

### Data Science Agent

```yaml
data-scientist:
  backend: claude
  model: claude-sonnet-4-20250514
  system_prompt: |
    You are a data scientist expert in:
    - Python data stack (pandas, numpy, scikit-learn)
    - Data visualization (matplotlib, seaborn, plotly)
    - Machine learning workflows
    - Statistical analysis

    Best practices:
    - Always explore data before modeling
    - Check for missing values and outliers
    - Document assumptions
    - Validate with appropriate metrics
```

### API Developer Agent

```yaml
api-dev:
  backend: claude
  model: claude-sonnet-4-20250514
  system_prompt: |
    You are an API development expert.

    Design principles:
    - RESTful conventions
    - Proper HTTP status codes
    - Input validation
    - Error handling with clear messages
    - Rate limiting considerations
    - API versioning

    Security:
    - Always validate and sanitize input
    - Use authentication (JWT, OAuth)
    - Implement authorization checks
    - Log security-relevant events
```

## Combining Everything

A complete agent setup:

```yaml
# agents.yaml
agents:
  full-stack:
    description: "Full-stack development assistant"
    backend: claude
    model: claude-sonnet-4-20250514
    auto_tools: false
    max_iterations: 10
    stream: true
    skills:
      - skills/git-expert.md
      - skills/testing-expert.md
    system_prompt: |
      You are a full-stack developer assistant.

      Frontend: React, TypeScript, Tailwind
      Backend: Python, FastAPI, PostgreSQL
      DevOps: Docker, GitHub Actions

      Priorities:
      1. Working code first
      2. Tests for critical paths
      3. Clear documentation
      4. Performance optimization

templates:
  feature:
    template: |
      Implement this feature: {description}

      Requirements:
      {requirements}

      Acceptance criteria:
      {criteria}

aliases:
  f: template use feature
```

Usage:
```bash
python3 agent_loop.py -a full-stack

> /f description="user login" requirements="email/password auth" criteria="secure, tested"
```

## Summary

- Agent variants bundle backend + config + system prompt
- System prompts define behavior and expertise
- Skills are reusable capability modules
- Templates enable parameterized prompts
- Aliases create shortcuts for workflows
- Combine everything for powerful domain-specific agents

## Next Steps

[Chapter 6](06_cost_tracking.md) covers how API costs are tracked, including automatic pricing lookup from OpenRouter.
