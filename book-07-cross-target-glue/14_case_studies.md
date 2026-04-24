<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 14: Case Studies

## Overview

This chapter presents four complete case studies demonstrating real-world applications of the cross-target glue system.

## Case Study 1: ETL Pipeline

**Scenario:** Process 100GB of log files, extract metrics, and load into a database.

### Requirements

- Parse Apache access logs
- Extract response times and status codes
- Aggregate by endpoint and hour
- Load into PostgreSQL

### Architecture

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│   AWK       │───▶│   Python    │───▶│    Go       │───▶│   Bash      │
│   Parse     │    │   Filter    │    │  Aggregate  │    │   Load      │
└─────────────┘    └─────────────┘    └─────────────┘    └─────────────┘
```

### Prolog Specification

```prolog
:- use_module(library(shell_glue)).
:- use_module(library(native_glue)).

generate_etl_pipeline(Script) :-
    generate_pipeline(
        [
            % Step 1: Parse Apache logs with AWK (fast regex)
            step(parse, awk, '
                /^[0-9]/ {
                    # Extract: IP, timestamp, method, path, status, time
                    match($0, /\\[([^\\]]+)\\]/, ts)
                    match($0, /"([A-Z]+) ([^ ]+)/, req)
                    match($0, /" ([0-9]+) /, status)
                    match($0, /([0-9]+)$/, time)
                    print ts[1], req[1], req[2], status[1], time[1]
                }
            ', []),

            % Step 2: Filter errors and slow requests with Python
            step(filter, python, '
                status = int(fields[3])
                response_time = int(fields[4])
                if status >= 400 or response_time > 1000:
                    output(*fields)
            ', [fields([timestamp, method, path, status, response_time])]),

            % Step 3: Aggregate with Go (memory-efficient)
            step(aggregate, go, './aggregate', [
                source('aggregate.go'),
                auto_build(true)
            ]),

            % Step 4: Load into PostgreSQL
            step(load, bash, '
                psql -h $DB_HOST -d metrics -c "
                    COPY metrics_hourly FROM STDIN WITH (FORMAT csv)
                "
            ', [])
        ],
        [
            input('/var/log/apache2/access.log'),
            output('/dev/null'),
            parallel(gnu_parallel),
            jobs(8)
        ],
        Script
    ).
```

### Go Aggregator

```prolog
generate_go_aggregator(Code) :-
    generate_go_pipe_main('
        // Key: endpoint + hour
        key := fields[2] + "|" + fields[0][:13]

        // Aggregate
        if agg, ok := aggregates[key]; ok {
            agg.count++
            agg.totalTime += parseTime(fields[4])
            agg.errors += countErrors(fields[3])
        } else {
            aggregates[key] = &Aggregate{
                count: 1,
                totalTime: parseTime(fields[4]),
                errors: countErrors(fields[3]),
            }
        }

        return nil  // Batch output at end
    ', [
        flush_handler('
            for key, agg := range aggregates {
                parts := strings.Split(key, "|")
                fmt.Printf("%s,%s,%d,%d,%d\\n",
                    parts[0], parts[1],
                    agg.count, agg.totalTime, agg.errors)
            }
        ')
    ], Code).
```

### Generated Pipeline Script

```bash
#!/bin/bash
set -euo pipefail

# Build aggregator if needed
if [ ! -f "./aggregate" ] || [ "aggregate.go" -nt "./aggregate" ]; then
    echo "Building aggregator..."
    go build -ldflags="-s -w" -o "./aggregate" "aggregate.go"
fi

# Run pipeline with GNU parallel
cat "/var/log/apache2/access.log" \
    | parallel --pipe -j8 -N100000 \
        'awk '\''
            /^[0-9]/ {
                match($0, /\[([^\]]+)\]/, ts)
                match($0, /"([A-Z]+) ([^ ]+)/, req)
                match($0, /" ([0-9]+) /, status)
                match($0, /([0-9]+)$/, time)
                print ts[1], req[1], req[2], status[1], time[1]
            }
        '\'' | python3 -c '\''
import sys
for line in sys.stdin:
    fields = line.strip().split("\t")
    status = int(fields[3])
    response_time = int(fields[4])
    if status >= 400 or response_time > 1000:
        print("\t".join(fields))
        '\''' \
    | ./aggregate \
    | psql -h $DB_HOST -d metrics -c "
        COPY metrics_hourly FROM STDIN WITH (FORMAT csv)
    "

echo "ETL complete"
```

### Performance Results

| Stage | Throughput | Notes |
|-------|------------|-------|
| AWK Parse | 2M lines/s | Regex optimized |
| Python Filter | 500K lines/s | Simple conditions |
| Go Aggregate | 1M lines/s | In-memory hash |
| PostgreSQL COPY | 200K rows/s | Bulk load |

**Overall:** 100GB processed in ~15 minutes with 8 parallel jobs.

---

## Case Study 2: .NET Integration

**Scenario:** Integrate a legacy C# application with Python ML models.

### Requirements

- C# web service receives requests
- Python performs ML inference
- Results returned to C# for business logic
- Minimize latency

### Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    C# Web Service                        │
├─────────────┬─────────────────────────┬─────────────────┤
│ Preprocess  │    ML Inference         │   Postprocess   │
│ (IronPython)│    (CPython + numpy)    │   (IronPython)  │
│ ~5ms        │    ~50ms                │   ~5ms          │
└─────────────┴─────────────────────────┴─────────────────┘
```

### Prolog Specification

```prolog
:- use_module(library(dotnet_glue)).

generate_ml_integration(Code) :-
    % Step 1: Generate IronPython preprocessor
    generate_ironpython_bridge([
        input_format(json),
        output_format(json),
        namespace('MLPipeline')
    ], PreprocessBridge),

    % Step 2: Generate CPython ML bridge
    generate_cpython_bridge([
        input_format(json),
        output_format(json),
        namespace('MLPipeline'),
        python_path('/opt/ml-models'),
        timeout(60)
    ], MLBridge),

    % Step 3: Generate IronPython postprocessor
    generate_ironpython_bridge([
        input_format(json),
        output_format(json),
        namespace('MLPipeline')
    ], PostprocessBridge),

    % Combine into complete integration
    format(string(Code), '
// MLPipeline Integration
// Generated by cross-target glue

namespace MLPipeline {
    // Preprocessor (IronPython - fast, no serialization)
    ~w

    // ML Inference (CPython - numpy/tensorflow)
    ~w

    // Postprocessor (IronPython - fast, no serialization)
    ~w

    public class MLPipelineService {
        private readonly IronPythonBridge _preprocess;
        private readonly CPythonBridge _ml;
        private readonly IronPythonBridge _postprocess;

        public MLPipelineService() {
            _preprocess = new IronPythonBridge();
            _ml = new CPythonBridge();
            _postprocess = new IronPythonBridge();
        }

        public async Task<PredictionResult> PredictAsync(InputData input) {
            // Step 1: Preprocess (IronPython - ~5ms)
            var preprocessed = _preprocess.Execute<InputData, PreprocessedData>(
                @"
import json
def preprocess(data):
    # Normalize, validate, transform
    return {
        ''features'': [data[''x''] / 100, data[''y''] / 100],
        ''metadata'': data.get(''meta'', {})
    }
result = preprocess(input_data)
",
                input
            );

            // Step 2: ML Inference (CPython - ~50ms)
            var prediction = await _ml.ExecuteAsync<PreprocessedData, RawPrediction>(
                @"
import numpy as np
import tensorflow as tf

model = tf.keras.models.load_model(''/opt/ml-models/model.h5'')
features = np.array([input_data[''features'']])
prediction = model.predict(features)
result = {''score'': float(prediction[0][0]), ''confidence'': float(np.max(prediction))}
",
                preprocessed
            );

            // Step 3: Postprocess (IronPython - ~5ms)
            return _postprocess.Execute<RawPrediction, PredictionResult>(
                @"
def postprocess(pred):
    return {
        ''result'': ''positive'' if pred[''score''] > 0.5 else ''negative'',
        ''confidence'': round(pred[''confidence''] * 100, 2),
        ''score'': pred[''score'']
    }
result = postprocess(input_data)
",
                prediction
            );
        }
    }
}
', [PreprocessBridge, MLBridge, PostprocessBridge]).
```

### Runtime Selection Logic

```prolog
% Automatic bridge selection based on imports
select_bridge_for_script(Script, Bridge) :-
    extract_imports(Script, Imports),
    (can_use_ironpython(Imports) ->
        generate_ironpython_bridge([], Bridge)
    ;
        generate_cpython_bridge([], Bridge)
    ).

% Example usage
?- select_bridge_for_script('
import json
import re
data = json.loads(input_data)
result = {"processed": data["value"] * 2}
', Bridge).
% Uses IronPython (json, re are compatible)

?- select_bridge_for_script('
import numpy as np
result = np.mean(input_data)
', Bridge).
% Uses CPython (numpy requires C extensions)
```

### Performance Comparison

| Approach | Latency | Throughput |
|----------|---------|------------|
| All CPython | 120ms | 8 req/s |
| Hybrid (IronPython + CPython) | 65ms | 15 req/s |
| Batched CPython | 80ms (batch of 10) | 125 req/s |

**Key Insight:** Using IronPython for pre/post processing cuts latency by 45%.

---

## Case Study 3: High-Performance Analytics

**Scenario:** Real-time analytics on 1M events/second.

### Requirements

- Sub-millisecond per-event latency
- Horizontal scaling
- Mixed computation (simple and complex)

### Architecture

```
              ┌──────────────────────────────────────────────┐
              │            Load Balancer                      │
              └───────────────┬──────────────────────────────┘
                              │
        ┌─────────────────────┼─────────────────────┐
        │                     │                     │
        ▼                     ▼                     ▼
┌───────────────┐     ┌───────────────┐     ┌───────────────┐
│   Go Router   │     │   Go Router   │     │   Go Router   │
│   (parallel)  │     │   (parallel)  │     │   (parallel)  │
└───────┬───────┘     └───────┬───────┘     └───────┬───────┘
        │                     │                     │
        ▼                     ▼                     ▼
┌───────────────┐     ┌───────────────┐     ┌───────────────┐
│ Rust Compute  │     │ Rust Compute  │     │ Rust Compute  │
│ (zero-copy)   │     │ (zero-copy)   │     │ (zero-copy)   │
└───────────────┘     └───────────────┘     └───────────────┘
```

### Prolog Specification

```prolog
:- use_module(library(native_glue)).
:- use_module(library(network_glue)).

generate_analytics_system :-
    % Generate Go router with worker pool
    generate_go_pipe_main('
        // Route to appropriate handler based on event type
        eventType := fields[0]
        switch eventType {
        case "click":
            return processClick(fields)
        case "view":
            return processView(fields)
        case "purchase":
            return processPurchase(fields)
        default:
            return nil
        }
    ', [
        parallel(64),
        buffer_size(10485760)
    ], GoRouter),
    write_file('router.go', GoRouter),

    % Generate Rust compute engine
    generate_rust_pipe_main('
        // Zero-copy parsing
        let event_type = fields[0];
        let user_id = fields[1];
        let timestamp: i64 = fields[2].parse().unwrap_or(0);

        // Compute metrics
        let metrics = compute_metrics(event_type, user_id, timestamp);

        Some(vec![
            metrics.event_type.to_string(),
            metrics.count.to_string(),
            metrics.latency_p99.to_string(),
        ])
    ', [
        parallel(true),
        buffer_size(10485760)
    ], RustCompute),
    write_file('compute.rs', RustCompute),

    % Generate orchestration script
    generate_pipeline(
        [
            step(route, go, './router', []),
            step(compute, rust, './compute', [])
        ],
        [
            parallel(gnu_parallel),
            jobs(32),
            chunk(10000)
        ],
        Orchestrator
    ),
    write_file('run_analytics.sh', Orchestrator),

    % Generate cross-compilation for deployment
    generate_cross_compile(go, 'router.go',
        [linux-amd64], GoBuild),
    generate_cross_compile(rust, 'src/main.rs',
        [linux-amd64], RustBuild),

    write_file('build_all.sh',
        "#!/bin/bash\n" ++ GoBuild ++ "\n" ++ RustBuild).
```

### Performance Tuning

```prolog
% Optimized buffer configuration
generate_high_throughput_pipeline(Script) :-
    generate_native_pipeline(
        [
            step(ingest, go, './ingest', [
                buffer(block(10485760)),  % 10MB blocks
                parallel(32)
            ]),
            step(process, rust, './process', [
                buffer(block(10485760)),
                memory_limit('8G'),
                cpu_cores([0,1,2,3,4,5,6,7])
            ])
        ],
        [
            monitor(throughput),
            error_handling(exit_codes)
        ],
        Script
    ).
```

### Benchmark Results

| Configuration | Throughput | P99 Latency |
|---------------|------------|-------------|
| Single Go | 200K/s | 5ms |
| Go + Rust | 500K/s | 2ms |
| Parallel Go (32) + Rust | 1.2M/s | 3ms |
| Distributed (3 nodes) | 3.5M/s | 4ms |

---

## Case Study 4: Microservices Pipeline

**Scenario:** Distributed data processing across multiple services.

### Requirements

- Service discovery
- Retry with backoff
- Circuit breaker
- Monitoring

### Architecture

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│  Ingestion  │────▶│  Transform  │────▶│   Storage   │
│  Service    │     │  Service    │     │   Service   │
│  (Python)   │     │  (Go)       │     │  (Rust)     │
└─────────────┘     └─────────────┘     └─────────────┘
       │                   │                   │
       └───────────────────┴───────────────────┘
                           │
                    ┌──────┴──────┐
                    │  Prometheus │
                    │  Metrics    │
                    └─────────────┘
```

### Prolog Specification

```prolog
:- use_module(library(network_glue)).

generate_microservices_system :-
    % Register services
    register_service(ingest, 'http://ingest:8080', [
        timeout(30),
        retries(3)
    ]),
    register_service(transform, 'http://transform:8080', [
        timeout(60),
        retries(2)
    ]),
    register_service(storage, 'http://storage:8080', [
        timeout(30),
        retries(3)
    ]),

    % Generate Python ingestion service
    generate_python_http_server(
        [
            endpoint('/ingest', ingest_handler, [methods(['POST'])])
        ],
        [port(8080), cors(true)],
        IngestServer
    ),
    write_file('services/ingest/server.py', IngestServer),

    % Generate Go transform service
    generate_go_http_server(
        [
            endpoint('/transform', transform_handler, [])
        ],
        [port(8080), cors(true)],
        TransformServer
    ),
    write_file('services/transform/server.go', TransformServer),

    % Generate Rust storage service
    generate_rust_http_server(
        [
            endpoint('/store', store_handler, [])
        ],
        [port(8080)],
        StorageServer
    ),
    write_file('services/storage/server.rs', StorageServer),

    % Generate orchestrator with resilience
    generate_network_pipeline(
        [
            step(ingest, remote, 'http://ingest:8080/ingest', []),
            step(transform, remote, 'http://transform:8080/transform', []),
            step(store, remote, 'http://storage:8080/store', [])
        ],
        [
            retry(3),
            backoff(exponential),
            circuit_breaker(failures(5), reset_after(60)),
            logging(verbose),
            metrics(prometheus)
        ],
        Orchestrator
    ),
    write_file('orchestrator.py', Orchestrator).
```

### Generated Orchestrator with Resilience

```python
#!/usr/bin/env python3
"""
Generated Network Pipeline with Resilience
"""

import requests
import time
import logging
from functools import wraps
from threading import Lock
from prometheus_client import Counter, Histogram

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Prometheus metrics
step_requests = Counter(
    'pipeline_step_requests_total',
    'Total requests per step',
    ['step', 'status']
)

step_latency = Histogram(
    'pipeline_step_latency_seconds',
    'Latency per step',
    ['step']
)

class CircuitBreaker:
    def __init__(self, failure_threshold=5, reset_timeout=60):
        self.failure_count = 0
        self.failure_threshold = failure_threshold
        self.reset_timeout = reset_timeout
        self.last_failure_time = 0
        self.state = 'closed'
        self.lock = Lock()

    def call(self, func, *args, **kwargs):
        with self.lock:
            if self.state == 'open':
                if time.time() - self.last_failure_time > self.reset_timeout:
                    self.state = 'half-open'
                    logger.info("Circuit breaker half-open, testing...")
                else:
                    raise Exception("Circuit breaker is open")

        try:
            result = func(*args, **kwargs)
            with self.lock:
                self.failure_count = 0
                self.state = 'closed'
            return result
        except Exception as e:
            with self.lock:
                self.failure_count += 1
                self.last_failure_time = time.time()
                if self.failure_count >= self.failure_threshold:
                    self.state = 'open'
                    logger.warning(f"Circuit breaker opened after {self.failure_count} failures")
            raise

def with_retry(max_retries=3, backoff_factor=2):
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            last_exception = None
            for attempt in range(max_retries):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    last_exception = e
                    if attempt < max_retries - 1:
                        sleep_time = backoff_factor ** attempt
                        logger.info(f"Retry {attempt + 1}/{max_retries} after {sleep_time}s...")
                        time.sleep(sleep_time)
            raise last_exception
        return wrapper
    return decorator

# Circuit breakers per service
circuits = {
    'ingest': CircuitBreaker(),
    'transform': CircuitBreaker(),
    'store': CircuitBreaker(),
}

def call_service(step_name, url, data):
    """Call a service with metrics."""
    start = time.time()
    try:
        response = requests.post(
            url,
            json={"data": data},
            timeout=30
        )
        result = response.json()

        duration = time.time() - start
        step_requests.labels(step=step_name, status='success').inc()
        step_latency.labels(step=step_name).observe(duration)
        logger.info(f"{step_name}: completed in {duration:.3f}s")

        if not result.get("success"):
            raise Exception(result.get("error"))
        return result.get("data")
    except Exception as e:
        step_requests.labels(step=step_name, status='error').inc()
        logger.error(f"{step_name}: failed - {e}")
        raise

@with_retry(max_retries=3)
def step_ingest(data):
    return circuits['ingest'].call(
        call_service, 'ingest', 'http://ingest:8080/ingest', data
    )

@with_retry(max_retries=2)
def step_transform(data):
    return circuits['transform'].call(
        call_service, 'transform', 'http://transform:8080/transform', data
    )

@with_retry(max_retries=3)
def step_store(data):
    return circuits['store'].call(
        call_service, 'store', 'http://storage:8080/store', data
    )

def run_pipeline(initial_data):
    """Execute the complete pipeline."""
    logger.info("Pipeline started")

    # Step 1: Ingest
    data = step_ingest(initial_data)

    # Step 2: Transform
    data = step_transform(data)

    # Step 3: Store
    data = step_store(data)

    logger.info("Pipeline completed successfully")
    return data

if __name__ == "__main__":
    import json
    import sys

    # Start Prometheus metrics server
    from prometheus_client import start_http_server
    start_http_server(9090)

    # Read input
    input_data = json.loads(sys.stdin.read())

    # Run pipeline
    result = run_pipeline(input_data)

    # Output result
    print(json.dumps(result))
```

### Docker Compose Configuration

```yaml
version: '3'
services:
  ingest:
    build: ./services/ingest
    ports:
      - "8081:8080"

  transform:
    build: ./services/transform
    ports:
      - "8082:8080"

  storage:
    build: ./services/storage
    ports:
      - "8083:8080"

  orchestrator:
    build: ./orchestrator
    ports:
      - "8080:8080"
      - "9090:9090"  # Prometheus metrics

  prometheus:
    image: prom/prometheus
    ports:
      - "9091:9090"
    volumes:
      - ./prometheus.yml:/etc/prometheus/prometheus.yml

  grafana:
    image: grafana/grafana
    ports:
      - "3000:3000"
```

### Monitoring Dashboard

The generated metrics enable Grafana dashboards showing:
- Request rate per service
- Latency percentiles (P50, P95, P99)
- Error rate
- Circuit breaker state

---

## Case Study 5: Streaming Data Ingest — Wikipedia Category Graph

**Scenario:** Parse the full English Wikipedia `categorylinks` SQL dump
(~2.3 GB gzipped, ~220 million rows) into an LMDB store, keeping only
the subcat edges (~10 million), for downstream graph analytics.

### Why this case study is here

Real pipeline. Shipped. And it illustrates a subtle point the earlier
case studies don't address head-on: **what belongs in hand-written
native code versus what belongs in transpilable Prolog composition**.

The MySQL INSERT tokenizer in this pipeline is hand-written Rust
(150 lines in a Cargo crate). It is not generated by UnifyWeaver and
it is not intended to be. Byte-level state machines — gzip decoding,
escape-sequence handling, type dispatch — are not Prolog's natural
shape. Forcing them to be generated would produce slow, allocation-heavy
code and defeat the very point of target specialization.

The **composition** of that parser with a filter, a column projection,
and a database writer *is* Prolog-declared:

```prolog
:- declare_target(parse_mysql_rows/2, rust,
                  [leaf(true), native_crate(mysql_stream)]).

:- declare_target(ingest_to_lmdb/3, python,
                  [leaf(true),
                   script_path('.../ingest_to_lmdb.py'),
                   python_min_version('3.9'),
                   pip_packages([lmdb])]).

process_dump(DumpPath, LmdbPath) :-
    forall(parse_mysql_rows(DumpPath, Row),
           ingest_to_lmdb(LmdbPath, Row)).
```

The glue layer turns this into a running bash pipeline: build the
Rust binary if needed, detect a Python interpreter that satisfies
the declared requirements, `export` the env-var schema for the
consumer, spawn the binary → pipe → Python. The user writes the
pipeline; the glue writes the plumbing.

### Architecture

```
┌───────────────────┐   TSV (tab-separated     ┌───────────────────┐
│ mysql_stream      │   bytes over stdout)     │ ingest_to_lmdb.py │
│  — Rust leaf      │───────────────────────▶  │  — Python leaf    │
│  — Cargo build    │                          │  — filter/project │
│  — zero-copy      │                          │  — LMDB writes    │
│  — gzip + parse   │                          │                   │
└───────────────────┘                          └───────────────────┘
         ▲                                               │
         │ DumpPath (CLI arg)                            ▼
         │                                     ┌───────────────────┐
         │                                     │ LMDB database     │
  enwiki-latest-                                │ — dupsort mode    │
  categorylinks.sql.gz                          │ — 9.93M subcat    │
                                                │   edges (int32_le)│
                                                └───────────────────┘
```

### The one-line swap: AWK instead of Rust

The same declaration structure, with the parser target changed:

```prolog
:- declare_target(parse_mysql_rows/2, awk,
                  [leaf(true),
                   script_path('.../parse_inserts.awk'),
                   input_filter(zcat)]).
```

Everything downstream — the `ingest_to_lmdb/3` declaration, the
`process_dump/2` predicate, the CLI entry point — is unchanged. The
glue layer notices the producer is an AWK script, prepends `zcat |`
so AWK sees decompressed input, and the rest of the pipeline runs
identically.

Byte-identical LMDB output validates the interchange:
- Rust parser on simplewiki: 6.7s, 297,283 subcat edges
- AWK parser on simplewiki: 4m35s, **297,283 subcat edges**

AWK is ~41× slower (interpreted, character-level work in a scripting
language) but produces the same data. That difference is the point:
the target choice is an implementation detail of the pipeline, not a
fundamental change to what the pipeline *is*.

### What's handwritten vs what's generated

| Component | Origin | Why |
|-----------|--------|-----|
| `mysql_stream` Rust crate | Hand-written (~150 LOC) | Byte-level state machine; transpiling it would break stream fusion and lose 10-50× throughput |
| `parse_inserts.awk` | Hand-written (~60 LOC) | Demonstrates target swap; AWK regex + state logic doesn't survive transpilation either |
| `ingest_to_lmdb.py` | Hand-written (~140 LOC) | Parameterized by env-var schema; reusable across any TSV-producing pipeline |
| `streaming_glue.pl` | Hand-written Prolog | Meta-level — orchestrates builds, interpreter detection, pipe wiring |
| **Prolog composition** | **User-authored, target-agnostic** | **What UnifyWeaver generates build commands and pipe orchestration for** |

This is the leaf-primitive rule: *primitives do mechanism, composition
does logic.* Every case study in this chapter follows the same
principle, but this one makes the boundary explicit.

### Scale that validated it

Running the same Prolog declarations on the full enwiki dump:

- **Input**: 2.3 GB gzipped, 218,853,102 INSERT rows
- **Output**: 9,932,244 subcat edges in a 448 MB LMDB
- **Wall time**: 8m26s end-to-end on a laptop (including build
  orchestration and interpreter detection)
- **Throughput**: ~430,000 rows/sec through the pipe

After ingest, the LMDB is queryable from any target with an LMDB
binding. A separate Haskell benchmark against the same database
(see `examples/benchmark/enwiki_dfs_benchmark/` in the main repo)
measured the IntMap-vs-LMDB crossover at this scale — the first
place in UnifyWeaver's measured history where LMDB outperformed
the in-memory backend.

---

## Summary

| Case Study | Languages | Key Techniques |
|------------|-----------|----------------|
| ETL Pipeline | AWK, Python, Go, Bash | GNU parallel, aggregation |
| .NET Integration | C#, IronPython, CPython | Hybrid runtime, batching |
| Analytics | Go, Rust | Zero-copy, parallel workers |
| Microservices | Python, Go, Rust | Circuit breaker, metrics |
| Streaming Ingest | Rust, AWK, Python, C# | Leaf primitives, one-line target swap, LMDB interchange |

### Key Takeaways

1. **Match tool to task**: AWK for parsing, Go for routing, Rust for compute
2. **Minimize serialization**: IronPython for .NET, direct pipes for shell
3. **Build resilience in**: Retry, circuit breakers, timeouts
4. **Monitor everything**: Prometheus metrics, structured logging
5. **Respect the leaf-primitive boundary**: byte-level work is hand-written native code; composition is Prolog-declared and glue-orchestrated
5. **Plan for scale**: GNU parallel, worker pools, horizontal scaling

---

## Appendix: Example Files

The complete code for all case studies is available in `examples/`:

```
examples/
├── 01-etl-pipeline/
│   ├── parse.awk
│   ├── filter.py
│   ├── aggregate.go
│   ├── pipeline.sh
│   └── README.md
├── 02-dotnet-integration/
│   ├── MLPipeline.cs
│   ├── preprocess.py
│   ├── inference.py
│   └── README.md
├── 03-high-performance/
│   ├── router.go
│   ├── compute.rs
│   ├── build.sh
│   └── README.md
└── 04-microservices/
    ├── services/
    │   ├── ingest/
    │   ├── transform/
    │   └── storage/
    ├── orchestrator.py
    ├── docker-compose.yml
    └── README.md
```

---

## Navigation

**←** [Previous: Chapter 13: API Reference](13_api_reference) | [📖 Book 7: Cross-Target Glue](./) | [Next: Chapter 15: Production Deployment (Phase 6) →](15_deployment_production)
