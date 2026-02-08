<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 6: Cost Tracking and Pricing

## Why Track Costs?

Every API call to a language model has a price. A single prompt-response exchange might cost fractions of a cent, but an agent making dozens of tool-calling iterations can accumulate significant costs. Without tracking:

- You have no visibility into how much a session costs
- You cannot compare the cost-effectiveness of different models
- Runaway tool-calling loops can drain API credits silently

The agent loop's cost tracker provides real-time cost estimates after each API call.

## The Pricing Model

Language model APIs charge per token, typically quoted as a price per 1 million tokens, with different rates for input (prompt) and output (completion):

| Model | Input (per 1M tokens) | Output (per 1M tokens) |
|-------|----------------------|----------------------|
| claude-opus-4-20250514 | $15.00 | $75.00 |
| claude-sonnet-4-20250514 | $3.00 | $15.00 |
| gpt-4o | $2.50 | $10.00 |
| gpt-4o-mini | $0.15 | $0.60 |
| moonshotai/kimi-k2.5 | ~$0.60 | ~$2.00 |
| codellama (local) | $0.00 | $0.00 |

The cost of a single API call is:

```
cost = (input_tokens / 1,000,000) * input_price + (output_tokens / 1,000,000) * output_price
```

## CostTracker Architecture

The cost tracker consists of two data structures and a recording method:

```python
@dataclass
class UsageRecord:
    """Record of a single API call."""
    timestamp: str
    model: str
    input_tokens: int
    output_tokens: int
    input_cost: float
    output_cost: float
    total_cost: float

@dataclass
class CostTracker:
    """Track API costs for a session."""
    pricing: dict          # model_name -> {"input": float, "output": float}
    records: list          # list of UsageRecord
    total_input_tokens: int = 0
    total_output_tokens: int = 0
    total_cost: float = 0.0
```

Recording usage:

```python
def record_usage(self, model: str, input_tokens: int, output_tokens: int) -> UsageRecord:
    pricing = self.pricing.get(model, {"input": 0.0, "output": 0.0})

    input_cost = (input_tokens / 1_000_000) * pricing["input"]
    output_cost = (output_tokens / 1_000_000) * pricing["output"]
    total_cost = input_cost + output_cost

    record = UsageRecord(
        timestamp=datetime.now().isoformat(),
        model=model,
        input_tokens=input_tokens,
        output_tokens=output_tokens,
        input_cost=input_cost,
        output_cost=output_cost,
        total_cost=total_cost,
    )

    self.records.append(record)
    self.total_input_tokens += input_tokens
    self.total_output_tokens += output_tokens
    self.total_cost += total_cost
    return record
```

## Token Estimation

Not all backends report exact token counts. CLI backends like coro may embed token info in their output; others provide nothing. The context manager uses a simple heuristic:

```python
def _estimate_tokens(text: str) -> int:
    """Estimate tokens from text length. Roughly 4 chars per token for English."""
    return max(1, len(text) // 4)
```

This is deliberately approximate. For cost tracking, exact counts from the API's `usage` field are preferred when available. The heuristic is used for context management decisions (should we trim?) where precision matters less.

## OpenRouter Pricing Auto-Fetch

The hardcoded pricing table cannot cover all models. OpenRouter aggregates hundreds of models, each with different pricing. The solution: query OpenRouter's API for current pricing and cache it locally.

### The API Endpoint

OpenRouter exposes model metadata at `https://openrouter.ai/api/v1/models`. The response includes per-token pricing:

```json
{
  "data": [
    {
      "id": "moonshotai/kimi-k2.5",
      "pricing": {
        "prompt": "0.0000006",
        "completion": "0.000002"
      }
    }
  ]
}
```

Note: these are **per-token** prices (not per-1M-tokens), so they need conversion.

### Fetching and Caching

```python
def fetch_openrouter_pricing(model_id: str) -> dict | None:
    """Fetch pricing for a model from OpenRouter's API."""
    # Check cache first (1-day TTL)
    cache = _load_openrouter_cache()
    if cache and model_id in cache.get('models', {}):
        return cache['models'][model_id]

    # Fetch from API
    url = "https://openrouter.ai/api/v1/models"
    req = Request(url, headers={'Accept': 'application/json'})

    with urlopen(req, timeout=30) as resp:
        data = json.loads(resp.read().decode('utf-8'))

    # Parse all models and convert to per-1M-tokens
    models = {}
    for model in data.get('data', []):
        pricing = model.get('pricing', {})
        prompt_per_token = float(pricing.get('prompt', '0'))
        completion_per_token = float(pricing.get('completion', '0'))
        models[model['id']] = {
            'input': prompt_per_token * 1_000_000,
            'output': completion_per_token * 1_000_000,
        }

    # Save cache
    _save_openrouter_cache({'models': models, 'timestamp': time.time()})

    return models.get(model_id)
```

The cache is stored at `~/.agent-loop/cache/openrouter_pricing.json` with a 1-day TTL:

```python
def _load_openrouter_cache() -> dict | None:
    cache_path = os.path.expanduser('~/.agent-loop/cache/openrouter_pricing.json')
    try:
        with open(cache_path) as f:
            cache = json.load(f)
        # Check TTL (1 day = 86400 seconds)
        if time.time() - cache.get('timestamp', 0) > 86400:
            return None
        return cache
    except (FileNotFoundError, json.JSONDecodeError):
        return None
```

### Auto-Fetch on First Use

The `ensure_pricing()` method is called before `record_usage()` to transparently fetch pricing for unknown models:

```python
def ensure_pricing(self, model: str) -> bool:
    """Ensure pricing exists for a model. Fetch from OpenRouter if needed."""
    if model in self.pricing:
        return True
    pricing = fetch_openrouter_pricing(model)
    if pricing:
        self.pricing[model] = pricing
        return True
    return False  # Unknown model, costs will show as $0.0000
```

## Integration with the Agent Loop

After each backend response, the agent loop records usage:

```python
# In _process_message()
if self.cost_tracker and response.tokens:
    model = getattr(self.backend, 'model', 'unknown')
    self.cost_tracker.ensure_pricing(model)
    input_tokens = response.tokens.get('input', 0)
    output_tokens = response.tokens.get('output', 0)
    self.cost_tracker.record_usage(model, input_tokens, output_tokens)
```

Users see costs after each response:

```
[Tokens: input: 247, output: 18, total: 265 | Est. cost: $0.0002]
```

The `/cost` interactive command shows a session summary:

```
Cost Summary
------------
Model: moonshotai/kimi-k2.5
Total input tokens:  1,234
Total output tokens:    567
Total cost:          $0.0019

Records:
  1. 2025-02-07T19:25:00 - 247 in / 18 out = $0.0002
  2. 2025-02-07T19:25:15 - 411 in / 89 out = $0.0004
  ...
```

## Cost Implications of Tool Calling

Tool-calling agents are more expensive than simple Q&A because each iteration sends the full context (including previous tool results) as input tokens. Consider this sequence:

| Iteration | Input Tokens | Output Tokens | Why |
|-----------|-------------|---------------|-----|
| Initial prompt | 247 | 18 | Model returns tool call |
| Iteration 1 | 377 | 25 | Context now includes tool result |
| Iteration 2 | 581 | 112 | Context grows with each iteration |

The input token count grows with each iteration because the conversation history accumulates tool results. This is why context limits (`--max-chars`, `--max-tokens`) matter for cost control, not just for memory management.

## Summary

- Cost tracking converts token counts to dollar amounts using per-model pricing tables
- OpenRouter pricing is auto-fetched from the API and cached locally for 1 day
- The `ensure_pricing()` method transparently fills gaps in the hardcoded pricing table
- Tool-calling iterations accumulate input tokens, making context limits a cost control mechanism
- The `/cost` command provides session-level cost visibility

## Next Steps

[Chapter 7](07_display_system.md) covers the terminal display system that provides visual feedback during API calls.
