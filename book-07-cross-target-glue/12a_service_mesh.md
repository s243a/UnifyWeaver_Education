<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 12a: Service Mesh (Phase 4)

## Overview

Phase 4 adds **service mesh** capabilities to UnifyWeaver's client-server architecture:

- **Load Balancing** - Distribute requests across multiple backends
- **Circuit Breakers** - Prevent cascade failures
- **Retry with Backoff** - Handle transient failures gracefully
- **Backend Pools** - Manage groups of service instances

These patterns are essential for building resilient distributed systems.

## Load Balancing

### Load Balance Strategies

UnifyWeaver supports multiple load balancing strategies:

| Strategy | Description | Use Case |
|----------|-------------|----------|
| `round_robin` | Rotate through backends sequentially | Equal capacity backends |
| `random` | Select backends randomly | Simple, stateless |
| `least_connections` | Route to backend with fewest active connections | Variable request duration |
| `weighted` | Distribute based on assigned weights | Mixed capacity backends |
| `ip_hash` | Hash client IP to select backend | Session affinity |

### Service Definition with Load Balancing

```prolog
service(api_gateway, [
    load_balance(round_robin),
    backends([
        backend('http://api1:8080', [weight(1)]),
        backend('http://api2:8080', [weight(1)]),
        backend('http://api3:8080', [weight(2)])  % Gets 2x traffic
    ])
], [
    receive(Request),
    forward_to_backend(Request, Response),
    respond(Response)
]).
```

### Weighted Load Balancing

```prolog
service(weighted_api, [
    load_balance(weighted),
    backends([
        backend('http://powerful:8080', [weight(5)]),
        backend('http://standard:8080', [weight(2)]),
        backend('http://small:8080', [weight(1)])
    ])
], Handler).
```

### Generated Python Load Balancer

```python
import random
import threading
from typing import List, Dict, Any
from dataclasses import dataclass

@dataclass
class Backend:
    url: str
    weight: int = 1
    active_connections: int = 0
    healthy: bool = True

class LoadBalancer:
    """Load balancer with multiple strategies."""

    def __init__(self, strategy: str, backends: List[Backend]):
        self.strategy = strategy
        self.backends = backends
        self.current_index = 0
        self.lock = threading.Lock()

    def select_backend(self, client_ip: str = None) -> Backend:
        """Select a backend based on the configured strategy."""
        healthy_backends = [b for b in self.backends if b.healthy]
        if not healthy_backends:
            raise Exception("No healthy backends available")

        if self.strategy == "round_robin":
            return self._round_robin(healthy_backends)
        elif self.strategy == "random":
            return self._random(healthy_backends)
        elif self.strategy == "least_connections":
            return self._least_connections(healthy_backends)
        elif self.strategy == "weighted":
            return self._weighted(healthy_backends)
        elif self.strategy == "ip_hash":
            return self._ip_hash(healthy_backends, client_ip)
        else:
            return self._round_robin(healthy_backends)

    def _round_robin(self, backends: List[Backend]) -> Backend:
        with self.lock:
            backend = backends[self.current_index % len(backends)]
            self.current_index += 1
            return backend

    def _random(self, backends: List[Backend]) -> Backend:
        return random.choice(backends)

    def _least_connections(self, backends: List[Backend]) -> Backend:
        return min(backends, key=lambda b: b.active_connections)

    def _weighted(self, backends: List[Backend]) -> Backend:
        total_weight = sum(b.weight for b in backends)
        r = random.randint(1, total_weight)
        cumulative = 0
        for backend in backends:
            cumulative += backend.weight
            if r <= cumulative:
                return backend
        return backends[-1]

    def _ip_hash(self, backends: List[Backend], client_ip: str) -> Backend:
        if not client_ip:
            return self._round_robin(backends)
        hash_value = hash(client_ip)
        return backends[hash_value % len(backends)]
```

### Generated Go Load Balancer

```go
package main

import (
    "hash/fnv"
    "math/rand"
    "sync"
    "sync/atomic"
)

type Backend struct {
    URL               string
    Weight            int
    ActiveConnections int32
    Healthy           bool
}

type LoadBalancer struct {
    Strategy     string
    Backends     []*Backend
    currentIndex uint32
    mu           sync.RWMutex
}

func NewLoadBalancer(strategy string, backends []*Backend) *LoadBalancer {
    return &LoadBalancer{
        Strategy: strategy,
        Backends: backends,
    }
}

func (lb *LoadBalancer) SelectBackend(clientIP string) *Backend {
    lb.mu.RLock()
    healthyBackends := make([]*Backend, 0)
    for _, b := range lb.Backends {
        if b.Healthy {
            healthyBackends = append(healthyBackends, b)
        }
    }
    lb.mu.RUnlock()

    if len(healthyBackends) == 0 {
        return nil
    }

    switch lb.Strategy {
    case "round_robin":
        return lb.roundRobin(healthyBackends)
    case "random":
        return lb.randomSelect(healthyBackends)
    case "least_connections":
        return lb.leastConnections(healthyBackends)
    case "weighted":
        return lb.weighted(healthyBackends)
    case "ip_hash":
        return lb.ipHash(healthyBackends, clientIP)
    default:
        return lb.roundRobin(healthyBackends)
    }
}

func (lb *LoadBalancer) roundRobin(backends []*Backend) *Backend {
    idx := atomic.AddUint32(&lb.currentIndex, 1)
    return backends[idx%uint32(len(backends))]
}

func (lb *LoadBalancer) randomSelect(backends []*Backend) *Backend {
    return backends[rand.Intn(len(backends))]
}

func (lb *LoadBalancer) leastConnections(backends []*Backend) *Backend {
    min := backends[0]
    for _, b := range backends[1:] {
        if atomic.LoadInt32(&b.ActiveConnections) < atomic.LoadInt32(&min.ActiveConnections) {
            min = b
        }
    }
    return min
}

func (lb *LoadBalancer) weighted(backends []*Backend) *Backend {
    totalWeight := 0
    for _, b := range backends {
        totalWeight += b.Weight
    }
    r := rand.Intn(totalWeight) + 1
    cumulative := 0
    for _, b := range backends {
        cumulative += b.Weight
        if r <= cumulative {
            return b
        }
    }
    return backends[len(backends)-1]
}

func (lb *LoadBalancer) ipHash(backends []*Backend, clientIP string) *Backend {
    if clientIP == "" {
        return lb.roundRobin(backends)
    }
    h := fnv.New32a()
    h.Write([]byte(clientIP))
    return backends[h.Sum32()%uint32(len(backends))]
}
```

## Circuit Breaker

The circuit breaker pattern prevents cascade failures by stopping requests to failing services.

### Circuit Breaker States

```
     ┌─────────┐
     │  CLOSED │ ◄── Normal operation
     └────┬────┘
          │ failures >= threshold
          ▼
     ┌─────────┐
     │   OPEN  │ ◄── Rejecting all requests
     └────┬────┘
          │ timeout expires
          ▼
     ┌─────────┐
     │HALF-OPEN│ ◄── Testing with limited requests
     └────┬────┘
          │ success: → CLOSED
          │ failure: → OPEN
```

### Circuit Breaker Configuration

```prolog
service(external_api, [
    circuit_breaker(
        threshold(5),           % Open after 5 failures
        timeout(60000),         % Stay open for 60 seconds
        half_open_requests(2),  % Allow 2 test requests
        success_threshold(2)    % 2 successes to close
    )
], [
    receive(Request),
    call_external(Request, Response),
    respond(Response)
]).
```

### Circuit Breaker Options

| Option | Description | Default |
|--------|-------------|---------|
| `threshold(N)` | Failures before opening | 5 |
| `timeout(Ms)` | Time to stay open | 60000 |
| `half_open_requests(N)` | Test requests in half-open | 1 |
| `success_threshold(N)` | Successes to close from half-open | 1 |

### Generated Python Circuit Breaker

```python
import time
import threading
from enum import Enum
from typing import Callable, Any

class CircuitState(Enum):
    CLOSED = "closed"
    OPEN = "open"
    HALF_OPEN = "half_open"

class CircuitBreaker:
    """Circuit breaker for fault tolerance."""

    def __init__(
        self,
        threshold: int = 5,
        timeout: int = 60000,
        half_open_requests: int = 1,
        success_threshold: int = 1
    ):
        self.threshold = threshold
        self.timeout_ms = timeout
        self.half_open_requests = half_open_requests
        self.success_threshold = success_threshold

        self.state = CircuitState.CLOSED
        self.failure_count = 0
        self.success_count = 0
        self.last_failure_time = 0
        self.half_open_count = 0
        self.lock = threading.Lock()

    def call(self, func: Callable, *args, **kwargs) -> Any:
        """Execute function through circuit breaker."""
        with self.lock:
            if self.state == CircuitState.OPEN:
                if self._should_attempt_reset():
                    self.state = CircuitState.HALF_OPEN
                    self.half_open_count = 0
                    self.success_count = 0
                else:
                    raise CircuitBreakerOpen("Circuit breaker is open")

            if self.state == CircuitState.HALF_OPEN:
                if self.half_open_count >= self.half_open_requests:
                    raise CircuitBreakerOpen("Half-open limit reached")
                self.half_open_count += 1

        try:
            result = func(*args, **kwargs)
            self._on_success()
            return result
        except Exception as e:
            self._on_failure()
            raise

    def _should_attempt_reset(self) -> bool:
        elapsed = (time.time() * 1000) - self.last_failure_time
        return elapsed >= self.timeout_ms

    def _on_success(self):
        with self.lock:
            if self.state == CircuitState.HALF_OPEN:
                self.success_count += 1
                if self.success_count >= self.success_threshold:
                    self.state = CircuitState.CLOSED
                    self.failure_count = 0
            else:
                self.failure_count = 0

    def _on_failure(self):
        with self.lock:
            self.failure_count += 1
            self.last_failure_time = time.time() * 1000

            if self.state == CircuitState.HALF_OPEN:
                self.state = CircuitState.OPEN
            elif self.failure_count >= self.threshold:
                self.state = CircuitState.OPEN

class CircuitBreakerOpen(Exception):
    pass
```

### Generated Go Circuit Breaker

```go
package main

import (
    "errors"
    "sync"
    "sync/atomic"
    "time"
)

type CircuitState int32

const (
    StateClosed CircuitState = iota
    StateOpen
    StateHalfOpen
)

var ErrCircuitOpen = errors.New("circuit breaker is open")

type CircuitBreaker struct {
    Threshold        int32
    TimeoutMs        int64
    HalfOpenRequests int32
    SuccessThreshold int32

    state           int32
    failureCount    int32
    successCount    int32
    lastFailureTime int64
    halfOpenCount   int32
    mu              sync.Mutex
}

func NewCircuitBreaker(threshold int, timeout int, halfOpen int, successThreshold int) *CircuitBreaker {
    return &CircuitBreaker{
        Threshold:        int32(threshold),
        TimeoutMs:        int64(timeout),
        HalfOpenRequests: int32(halfOpen),
        SuccessThreshold: int32(successThreshold),
        state:            int32(StateClosed),
    }
}

func (cb *CircuitBreaker) Call(fn func() (interface{}, error)) (interface{}, error) {
    if !cb.allowRequest() {
        return nil, ErrCircuitOpen
    }

    result, err := fn()
    if err != nil {
        cb.onFailure()
        return nil, err
    }

    cb.onSuccess()
    return result, nil
}

func (cb *CircuitBreaker) allowRequest() bool {
    state := CircuitState(atomic.LoadInt32(&cb.state))

    switch state {
    case StateClosed:
        return true
    case StateOpen:
        if cb.shouldAttemptReset() {
            atomic.StoreInt32(&cb.state, int32(StateHalfOpen))
            atomic.StoreInt32(&cb.halfOpenCount, 0)
            atomic.StoreInt32(&cb.successCount, 0)
            return true
        }
        return false
    case StateHalfOpen:
        count := atomic.AddInt32(&cb.halfOpenCount, 1)
        return count <= cb.HalfOpenRequests
    }
    return false
}

func (cb *CircuitBreaker) shouldAttemptReset() bool {
    elapsed := time.Now().UnixMilli() - atomic.LoadInt64(&cb.lastFailureTime)
    return elapsed >= cb.TimeoutMs
}

func (cb *CircuitBreaker) onSuccess() {
    state := CircuitState(atomic.LoadInt32(&cb.state))
    if state == StateHalfOpen {
        count := atomic.AddInt32(&cb.successCount, 1)
        if count >= cb.SuccessThreshold {
            atomic.StoreInt32(&cb.state, int32(StateClosed))
            atomic.StoreInt32(&cb.failureCount, 0)
        }
    } else {
        atomic.StoreInt32(&cb.failureCount, 0)
    }
}

func (cb *CircuitBreaker) onFailure() {
    atomic.StoreInt64(&cb.lastFailureTime, time.Now().UnixMilli())
    count := atomic.AddInt32(&cb.failureCount, 1)

    state := CircuitState(atomic.LoadInt32(&cb.state))
    if state == StateHalfOpen {
        atomic.StoreInt32(&cb.state, int32(StateOpen))
    } else if count >= cb.Threshold {
        atomic.StoreInt32(&cb.state, int32(StateOpen))
    }
}
```

## Retry with Backoff

Handle transient failures with configurable retry strategies.

### Retry Strategies

| Strategy | Description | Formula |
|----------|-------------|---------|
| `fixed` | Constant delay | `delay` |
| `linear` | Linearly increasing | `delay * attempt` |
| `exponential` | Exponentially increasing | `delay * 2^attempt` |

### Retry Configuration

```prolog
service(flaky_api, [
    retry(3, exponential),  % 3 retries with exponential backoff
    retry_options([
        delay(1000),        % Initial delay: 1 second
        max_delay(30000),   % Max delay: 30 seconds
        jitter(true)        % Add random jitter
    ])
], [
    receive(Request),
    call_flaky_service(Request, Response),
    respond(Response)
]).
```

### Retry Options

| Option | Description | Default |
|--------|-------------|---------|
| `delay(Ms)` | Initial delay | 1000 |
| `max_delay(Ms)` | Maximum delay cap | 60000 |
| `jitter(Bool)` | Add random jitter | false |
| `retry_on(Errors)` | Error types to retry | all |

### Generated Python Retry Logic

```python
import time
import random
from functools import wraps
from typing import Callable, List, Type

class RetryConfig:
    def __init__(
        self,
        max_retries: int = 3,
        strategy: str = "exponential",
        delay: int = 1000,
        max_delay: int = 60000,
        jitter: bool = False,
        retry_on: List[Type[Exception]] = None
    ):
        self.max_retries = max_retries
        self.strategy = strategy
        self.delay = delay
        self.max_delay = max_delay
        self.jitter = jitter
        self.retry_on = retry_on or [Exception]

    def calculate_delay(self, attempt: int) -> float:
        """Calculate delay for the given attempt."""
        if self.strategy == "fixed":
            delay = self.delay
        elif self.strategy == "linear":
            delay = self.delay * attempt
        elif self.strategy == "exponential":
            delay = self.delay * (2 ** (attempt - 1))
        else:
            delay = self.delay

        # Apply max delay cap
        delay = min(delay, self.max_delay)

        # Add jitter (0-25% of delay)
        if self.jitter:
            jitter_amount = delay * random.uniform(0, 0.25)
            delay += jitter_amount

        return delay / 1000  # Convert to seconds

def with_retry(config: RetryConfig):
    """Decorator for retry logic."""
    def decorator(func: Callable):
        @wraps(func)
        def wrapper(*args, **kwargs):
            last_exception = None

            for attempt in range(1, config.max_retries + 2):  # +1 for initial attempt
                try:
                    return func(*args, **kwargs)
                except tuple(config.retry_on) as e:
                    last_exception = e
                    if attempt <= config.max_retries:
                        delay = config.calculate_delay(attempt)
                        print(f"Attempt {attempt} failed, retrying in {delay:.2f}s...")
                        time.sleep(delay)
                    else:
                        break

            raise last_exception
        return wrapper
    return decorator

# Usage
retry_config = RetryConfig(
    max_retries=3,
    strategy="exponential",
    delay=1000,
    jitter=True
)

@with_retry(retry_config)
def call_flaky_service(request):
    # Your service call
    pass
```

### Generated Go Retry Logic

```go
package main

import (
    "math"
    "math/rand"
    "time"
)

type RetryConfig struct {
    MaxRetries int
    Strategy   string
    Delay      int // milliseconds
    MaxDelay   int
    Jitter     bool
}

func NewRetryConfig(maxRetries int, strategy string) *RetryConfig {
    return &RetryConfig{
        MaxRetries: maxRetries,
        Strategy:   strategy,
        Delay:      1000,
        MaxDelay:   60000,
        Jitter:     false,
    }
}

func (rc *RetryConfig) CalculateDelay(attempt int) time.Duration {
    var delay float64

    switch rc.Strategy {
    case "fixed":
        delay = float64(rc.Delay)
    case "linear":
        delay = float64(rc.Delay * attempt)
    case "exponential":
        delay = float64(rc.Delay) * math.Pow(2, float64(attempt-1))
    default:
        delay = float64(rc.Delay)
    }

    // Apply max delay cap
    if delay > float64(rc.MaxDelay) {
        delay = float64(rc.MaxDelay)
    }

    // Add jitter (0-25% of delay)
    if rc.Jitter {
        jitter := delay * rand.Float64() * 0.25
        delay += jitter
    }

    return time.Duration(delay) * time.Millisecond
}

func WithRetry(config *RetryConfig, fn func() (interface{}, error)) (interface{}, error) {
    var lastErr error

    for attempt := 1; attempt <= config.MaxRetries+1; attempt++ {
        result, err := fn()
        if err == nil {
            return result, nil
        }

        lastErr = err
        if attempt <= config.MaxRetries {
            delay := config.CalculateDelay(attempt)
            time.Sleep(delay)
        }
    }

    return nil, lastErr
}
```

## Combining Patterns

Use all patterns together for maximum resilience:

```prolog
service(resilient_api, [
    % Load balancing
    load_balance(least_connections),
    backends([
        backend('http://api1:8080', []),
        backend('http://api2:8080', []),
        backend('http://api3:8080', [])
    ]),

    % Circuit breaker per backend
    circuit_breaker(
        threshold(3),
        timeout(30000)
    ),

    % Retry with backoff
    retry(2, exponential),
    retry_options([
        delay(500),
        max_delay(5000),
        jitter(true)
    ])
], [
    receive(Request),
    forward_with_resilience(Request, Response),
    respond(Response)
]).
```

### Generated Combined Implementation

```python
class ResilientService:
    """Service with load balancing, circuit breaker, and retry."""

    def __init__(self):
        self.backends = [
            Backend("http://api1:8080"),
            Backend("http://api2:8080"),
            Backend("http://api3:8080"),
        ]
        self.load_balancer = LoadBalancer("least_connections", self.backends)

        # Circuit breaker per backend
        self.circuit_breakers = {
            b.url: CircuitBreaker(threshold=3, timeout=30000)
            for b in self.backends
        }

        self.retry_config = RetryConfig(
            max_retries=2,
            strategy="exponential",
            delay=500,
            max_delay=5000,
            jitter=True
        )

    def call(self, request):
        """Call with full resilience stack."""
        return self._call_with_retry(request)

    def _call_with_retry(self, request):
        last_error = None

        for attempt in range(1, self.retry_config.max_retries + 2):
            try:
                return self._call_with_circuit_breaker(request)
            except Exception as e:
                last_error = e
                if attempt <= self.retry_config.max_retries:
                    delay = self.retry_config.calculate_delay(attempt)
                    time.sleep(delay)

        raise last_error

    def _call_with_circuit_breaker(self, request):
        backend = self.load_balancer.select_backend()
        circuit = self.circuit_breakers[backend.url]

        return circuit.call(lambda: self._forward_request(backend, request))

    def _forward_request(self, backend, request):
        backend.active_connections += 1
        try:
            # Make actual HTTP call
            response = requests.post(
                backend.url,
                json={"data": request},
                timeout=5
            )
            return response.json()
        finally:
            backend.active_connections -= 1
```

## Testing Service Mesh

```bash
# Run Phase 4 integration tests
./tests/integration/test_service_mesh.sh
# 61 tests covering all patterns
```

## Chapter Summary

- **Load balancing** distributes traffic across backends
- **Circuit breakers** prevent cascade failures
- **Retry with backoff** handles transient failures
- **Combined patterns** provide defense in depth
- **Thread-safe implementations** for all targets

## Next Steps

Chapter 12 (Distributed Pipelines) covers socket communication. Then Chapter 12b adds:
- Polyglot services (cross-language calls)
- Distributed services (sharding, replication)

## Exercises

1. **Load balancer**: Implement a service with weighted load balancing and verify traffic distribution.

2. **Circuit breaker testing**: Create a service that fails 50% of the time and verify circuit breaker behavior.

3. **Retry analysis**: Compare fixed, linear, and exponential backoff under load.

4. **Combined resilience**: Build a service with all three patterns and inject various failure modes.

---

## Navigation

**←** [Previous: Chapter 12: Distributed Pipelines](12_distributed_pipelines) | [📖 Book 7: Cross-Target Glue](./) | [Next: Chapter 12b: Polyglot & Distributed Services →](12b_polyglot_distributed)
