<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 11a: Service Architecture (Phases 1-3)

## Overview

This chapter introduces UnifyWeaver's **Client-Server Architecture** - a unified approach to defining services that compile to Python, Go, and Rust with transport independence.

The core insight: **client-server communication is fundamentally two pipelines going in opposite directions**:
- **Request Pipeline**: Client → Server (carries request data)
- **Response Pipeline**: Server → Client (carries response data)

This chapter covers:
- Phase 1: In-Process Services
- Phase 2: Unix Socket Services
- Phase 3: Network Services (TCP/HTTP)

## Architecture Layers

```
┌─────────────────────────────────────────────────────────────┐
│                    Application Layer                        │
│  service(Name, [receive(X), transform(X, Y), respond(Y)])  │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    Service Layer                            │
│  - Service Registry (in-process lookup)                     │
│  - Request/Response handling                                │
│  - State management (for stateful services)                 │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   Transport Layer                           │
│  - in_process: Direct function calls                        │
│  - unix_socket: Unix domain sockets                         │
│  - tcp: TCP sockets                                         │
│  - http: HTTP/REST endpoints                                │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                   Protocol Layer                            │
│  - jsonl: JSON Lines (streaming)                            │
│  - json: Standard JSON                                      │
│  - messagepack: Binary MessagePack                          │
│  - protobuf: Protocol Buffers                               │
└─────────────────────────────────────────────────────────────┘
```

## Phase 1: In-Process Services

Phase 1 establishes the foundation with in-process service calls - the simplest form of service communication.

### Service Definition

The `service/3` predicate defines a service:

```prolog
service(Name, Options, HandlerSpec).
```

**Simple stateless service:**
```prolog
service(echo, [], [
    receive(X),
    respond(X)
]).
```

**Service with options:**
```prolog
service(counter, [stateful(true), timeout(5000)], [
    receive(Cmd),
    state_get(count, Current),
    ( Cmd = increment ->
        NewCount is Current + 1,
        state_put(count, NewCount),
        respond(NewCount)
    ;
        respond(Current)
    )
]).
```

**Service with transformation:**
```prolog
service(user_enricher, [], [
    receive(UserId),
    lookup_user(UserId, UserData),
    enrich_user(UserData, Enriched),
    respond(Enriched)
]).
```

### Service Operations

| Operation | Description |
|-----------|-------------|
| `receive(Var)` | Bind incoming request to variable |
| `respond(Value)` | Send response to caller |
| `respond_error(Error)` | Send error response |
| `state_get(Key, Value)` | Get state value (stateful services) |
| `state_put(Key, Value)` | Set state value (stateful services) |
| `state_modify(Key, Func)` | Modify state with function |
| `state_delete(Key)` | Delete state key |
| `call_service(Name, Req, Resp)` | Call another service |
| `transform(In, Out, Goal)` | Transform data with predicate |
| `branch(Cond, True, False)` | Conditional execution |
| `route_by(Field, Routes)` | Route by field value |

### Phase 1 Service Options

| Option | Values | Default | Description |
|--------|--------|---------|-------------|
| `stateful(Bool)` | true/false | false | Enable state management |
| `timeout(Ms)` | Integer | 30000 | Request timeout in ms |
| `max_requests(N)` | Integer | unlimited | Max concurrent requests |

### Compiling In-Process Services

```prolog
% Compile to Python
?- service(echo, [], [receive(X), respond(X)]),
   compile_service_to_python(service(echo, [], [receive(X), respond(X)]), Code).

% Compile to Go
?- compile_service_to_go(Service, Code).

% Compile to Rust
?- compile_service_to_rust(Service, Code).
```

### Generated Python Code

```python
class EchoService:
    """In-process service: echo"""

    def __init__(self):
        self.name = "echo"
        self.stateful = False
        self.timeout_ms = 30000
        self.state = {}

    def call(self, request):
        """Process a request through the service."""
        return self._handle_request(request)

    def _handle_request(self, request):
        # receive(X) - bind request
        x = request
        # respond(X) - return response
        return x

# Service instance
echo_service = EchoService()
```

### Generated Go Code

```go
package main

import (
    "sync"
    "time"
)

// EchoService is an in-process service
type EchoService struct {
    Name      string
    Stateful  bool
    TimeoutMs int
    State     map[string]interface{}
    mu        sync.RWMutex
}

// NewEchoService creates a new Echo service instance
func NewEchoService() *EchoService {
    return &EchoService{
        Name:      "echo",
        Stateful:  false,
        TimeoutMs: 30000,
        State:     make(map[string]interface{}),
    }
}

// Call processes a request through the service
func (s *EchoService) Call(request interface{}) interface{} {
    return s.handleRequest(request)
}

func (s *EchoService) handleRequest(request interface{}) interface{} {
    // receive(X) - bind request
    x := request
    // respond(X) - return response
    return x
}

// Service instance
var echoServiceInstance = NewEchoService()
```

## Phase 2: Unix Socket Services

Phase 2 adds Unix domain socket transport for cross-process communication on the same machine.

### Unix Socket Service Definition

```prolog
service(data_processor, [
    transport(unix_socket('/tmp/processor.sock')),
    protocol(jsonl),
    stateful(true)
], [
    receive(Data),
    process(Data, Result),
    respond(Result)
]).
```

### Transport Options

| Option | Description |
|--------|-------------|
| `transport(unix_socket(Path))` | Unix domain socket path |
| `protocol(jsonl)` | JSON Lines protocol (streaming) |
| `protocol(json)` | Standard JSON protocol |
| `buffer_size(N)` | Socket buffer size |

### Generated Python Unix Socket Server

```python
import socket
import json
import os
import threading

class DataProcessorService:
    """Unix socket service: data_processor"""

    def __init__(self, socket_path='/tmp/processor.sock'):
        self.socket_path = socket_path
        self.stateful = True
        self.state = {}
        self.lock = threading.Lock()
        self.running = False

    def start(self):
        """Start the Unix socket server."""
        # Remove existing socket
        if os.path.exists(self.socket_path):
            os.unlink(self.socket_path)

        self.server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
        self.server.bind(self.socket_path)
        self.server.listen(5)
        self.running = True

        print(f"Listening on {self.socket_path}")

        while self.running:
            try:
                conn, _ = self.server.accept()
                thread = threading.Thread(target=self._handle_connection, args=(conn,))
                thread.daemon = True
                thread.start()
            except Exception as e:
                if self.running:
                    print(f"Accept error: {e}")

    def _handle_connection(self, conn):
        """Handle a client connection."""
        buffer = ""
        try:
            while True:
                data = conn.recv(65536).decode('utf-8')
                if not data:
                    break

                buffer += data
                while '\n' in buffer:
                    line, buffer = buffer.split('\n', 1)
                    if line.strip():
                        request = json.loads(line)
                        response = self._handle_request(request)
                        conn.sendall((json.dumps(response) + '\n').encode())
        finally:
            conn.close()

    def _handle_request(self, request):
        # Your handler logic here
        return {"result": "processed"}

    def stop(self):
        """Stop the server."""
        self.running = False
        self.server.close()
        if os.path.exists(self.socket_path):
            os.unlink(self.socket_path)
```

### Generated Go Unix Socket Server

```go
package main

import (
    "bufio"
    "encoding/json"
    "fmt"
    "net"
    "os"
    "sync"
)

type DataProcessorService struct {
    SocketPath string
    Stateful   bool
    State      map[string]interface{}
    mu         sync.RWMutex
    listener   net.Listener
    running    bool
}

func NewDataProcessorService(socketPath string) *DataProcessorService {
    return &DataProcessorService{
        SocketPath: socketPath,
        Stateful:   true,
        State:      make(map[string]interface{}),
    }
}

func (s *DataProcessorService) Start() error {
    // Remove existing socket
    os.Remove(s.SocketPath)

    listener, err := net.Listen("unix", s.SocketPath)
    if err != nil {
        return err
    }
    s.listener = listener
    s.running = true

    fmt.Printf("Listening on %s\n", s.SocketPath)

    for s.running {
        conn, err := listener.Accept()
        if err != nil {
            if s.running {
                fmt.Fprintf(os.Stderr, "Accept error: %v\n", err)
            }
            continue
        }
        go s.handleConnection(conn)
    }
    return nil
}

func (s *DataProcessorService) handleConnection(conn net.Conn) {
    defer conn.Close()

    reader := bufio.NewReader(conn)
    writer := bufio.NewWriter(conn)

    for {
        line, err := reader.ReadString('\n')
        if err != nil {
            return
        }

        var request map[string]interface{}
        json.Unmarshal([]byte(line), &request)

        response := s.handleRequest(request)

        respBytes, _ := json.Marshal(response)
        writer.WriteString(string(respBytes) + "\n")
        writer.Flush()
    }
}

func (s *DataProcessorService) handleRequest(request map[string]interface{}) map[string]interface{} {
    // Your handler logic here
    return map[string]interface{}{"result": "processed"}
}

func (s *DataProcessorService) Stop() {
    s.running = false
    s.listener.Close()
    os.Remove(s.SocketPath)
}
```

## Phase 3: Network Services (TCP/HTTP)

Phase 3 adds network transport for distributed systems.

### TCP Service Definition

```prolog
service(stream_processor, [
    transport(tcp('0.0.0.0', 9000)),
    protocol(jsonl),
    stateful(false)
], [
    receive(Record),
    transform_record(Record, Transformed),
    respond(Transformed)
]).
```

### HTTP Service Definition

```prolog
service(api_endpoint, [
    transport(http('/api/process')),
    methods(['POST', 'GET']),
    cors(true)
], [
    receive(Request),
    validate(Request, Validated),
    process(Validated, Result),
    respond(Result)
]).
```

### HTTP Service Options

| Option | Values | Default | Description |
|--------|--------|---------|-------------|
| `transport(http(Endpoint))` | String | - | HTTP endpoint path |
| `methods(List)` | ['GET', 'POST', ...] | ['POST'] | Allowed HTTP methods |
| `cors(Bool)` | true/false | false | Enable CORS headers |
| `auth(Type)` | none/basic/bearer | none | Authentication type |

### Generated Python HTTP Service (Flask)

```python
from flask import Flask, request, jsonify
from flask_cors import CORS
import os

app = Flask(__name__)
CORS(app)

class ApiEndpointService:
    """HTTP service: api_endpoint"""

    def __init__(self):
        self.name = "api_endpoint"
        self.stateful = False
        self.state = {}

    def handle_request(self, data):
        # validate(Request, Validated)
        validated = self.validate(data)
        # process(Validated, Result)
        result = self.process(validated)
        # respond(Result)
        return result

    def validate(self, data):
        # Validation logic
        return data

    def process(self, data):
        # Processing logic
        return {"processed": True, "data": data}

service = ApiEndpointService()

@app.route('/api/process', methods=['POST', 'GET'])
def api_process():
    try:
        data = request.get_json() if request.is_json else {}
        result = service.handle_request(data.get('data'))
        return jsonify({"success": True, "data": result})
    except Exception as e:
        return jsonify({"success": False, "error": str(e)}), 400

@app.errorhandler(Exception)
def handle_error(error):
    return jsonify({"success": False, "error": str(error)}), 500

if __name__ == "__main__":
    port = int(os.environ.get("PORT", 8080))
    app.run(host="0.0.0.0", port=port, debug=False)
```

### Generated Go HTTP Service

```go
package main

import (
    "encoding/json"
    "fmt"
    "io"
    "log"
    "net/http"
    "os"
    "sync"
)

type Request struct {
    Data interface{} `json:"data"`
}

type Response struct {
    Success bool        `json:"success"`
    Data    interface{} `json:"data,omitempty"`
    Error   string      `json:"error,omitempty"`
}

type ApiEndpointService struct {
    Name     string
    Stateful bool
    State    map[string]interface{}
    mu       sync.RWMutex
}

func NewApiEndpointService() *ApiEndpointService {
    return &ApiEndpointService{
        Name:     "api_endpoint",
        Stateful: false,
        State:    make(map[string]interface{}),
    }
}

func (s *ApiEndpointService) HandleRequest(data interface{}) interface{} {
    // Your processing logic
    return map[string]interface{}{"processed": true, "data": data}
}

func corsMiddleware(next http.HandlerFunc) http.HandlerFunc {
    return func(w http.ResponseWriter, r *http.Request) {
        w.Header().Set("Access-Control-Allow-Origin", "*")
        w.Header().Set("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
        w.Header().Set("Access-Control-Allow-Headers", "Content-Type")

        if r.Method == "OPTIONS" {
            w.WriteHeader(http.StatusOK)
            return
        }
        next(w, r)
    }
}

var service = NewApiEndpointService()

func apiProcessHandler(w http.ResponseWriter, r *http.Request) {
    w.Header().Set("Content-Type", "application/json")

    body, err := io.ReadAll(r.Body)
    if err != nil {
        json.NewEncoder(w).Encode(Response{Success: false, Error: "Failed to read request"})
        return
    }

    var req Request
    if len(body) > 0 {
        if err := json.Unmarshal(body, &req); err != nil {
            json.NewEncoder(w).Encode(Response{Success: false, Error: "Invalid JSON"})
            return
        }
    }

    result := service.HandleRequest(req.Data)
    json.NewEncoder(w).Encode(Response{Success: true, Data: result})
}

func main() {
    port := os.Getenv("PORT")
    if port == "" {
        port = "8080"
    }

    http.HandleFunc("/api/process", corsMiddleware(apiProcessHandler))

    log.Printf("Server starting on port %s", port)
    log.Fatal(http.ListenAndServe(":"+port, nil))
}
```

## Service Validation

UnifyWeaver validates service definitions at compile time:

```prolog
:- use_module('src/unifyweaver/core/service_validation').

% Check if a service definition is valid
?- validate_service(service(echo, [], [receive(X), respond(X)])).
true.

% Invalid service (missing respond)
?- validate_service(service(broken, [], [receive(X)])).
false.
```

### Validation Rules

- Service name must be an atom
- Handler spec must be a list
- Must have at least `receive` and `respond` operations
- Options must be valid for the transport type
- Stateful services require state operations

## Client Generation

UnifyWeaver also generates clients for services:

```prolog
% Generate Python client
?- generate_service_client_python(
    service(api, [transport(http('/api'))], _),
    ClientCode
).

% Generate Go client
?- generate_service_client_go(Service, ClientCode).
```

### Generated Python Client

```python
import requests
from typing import Any

class ApiClient:
    """Client for api service."""

    def __init__(self, base_url: str, timeout: int = 30):
        self.base_url = base_url.rstrip('/')
        self.timeout = timeout

    def call(self, data: Any = None) -> Any:
        """Call the service."""
        response = requests.post(
            f"{self.base_url}/api",
            json={"data": data},
            timeout=self.timeout,
            headers={"Content-Type": "application/json"}
        )
        response.raise_for_status()
        result = response.json()

        if not result.get("success"):
            raise Exception(result.get("error", "Unknown error"))

        return result.get("data")
```

## Testing Services

Integration tests verify the implementation:

```bash
# Phase 1: In-process services
./tests/integration/test_in_process_services.sh

# Phase 2: Unix socket services
./tests/integration/test_unix_socket_services.sh

# Phase 3: Network services
./tests/integration/test_network_services.sh
```

## Chapter Summary

- **Phase 1** provides in-process services with `service/3` predicate
- **Phase 2** adds Unix socket transport for cross-process communication
- **Phase 3** adds TCP and HTTP transport for network communication
- **All phases** generate code for Python, Go, and Rust
- **Transport independence** - same service definition, different transports

## Next Steps

In Chapter 11 (HTTP Services), we cover additional HTTP server patterns. Then Chapter 12a (Service Mesh) adds:
- Load balancing strategies
- Circuit breakers
- Retry with backoff

## Exercises

1. **Echo service**: Define an echo service and compile it to all three targets.

2. **Stateful counter**: Create a stateful counter service that increments on each call.

3. **Unix socket**: Create a Unix socket service and test it with netcat.

4. **HTTP API**: Create an HTTP service with multiple endpoints using `route_by`.

5. **Transport migration**: Take an in-process service and migrate it to HTTP without changing the handler logic.

---

## Navigation

**←** [Previous: Chapter 11: HTTP Services](11_http_services) | [📖 Book 7: Cross-Target Glue](./) | [Next: Chapter 12: Distributed Pipelines →](12_distributed_pipelines)
