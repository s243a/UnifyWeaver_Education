<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 12: Performance Benchmarking

*Measuring and optimizing federated search performance*

## Overview

This chapter covers the federation benchmark suite for measuring performance across different configurations, network sizes, and query patterns. You'll learn to identify bottlenecks and tune your deployment.

## The Benchmark Suite

The benchmark suite lives in `benchmarks/federation/`:

```
benchmarks/federation/
├── __init__.py
├── synthetic_network.py      # Node network generation
├── workload_generator.py     # Query workload generation
├── benchmark_runner.py       # Main benchmark orchestration
├── metrics.py                # Metric collection/aggregation
├── visualizations.py         # matplotlib charts
└── run_benchmarks.py         # CLI entry point
```

## Quick Start

```bash
# Quick test (10 nodes, 20 queries)
python -m benchmarks.federation.run_benchmarks --nodes 10 --queries 20 --quick

# Full benchmark
python -m benchmarks.federation.run_benchmarks --nodes 10,25,50 --queries 50 --output reports/
```

Output includes:
- `reports/federation_benchmark.html` - Interactive HTML report
- `reports/results.json` - Raw benchmark data
- `reports/*.png` - Chart images (if matplotlib available)

## Synthetic Networks

Generate mock node networks with configurable characteristics:

```python
from benchmarks.federation.synthetic_network import create_synthetic_network

# Create a 25-node network with clustered topics
network = create_synthetic_network(
    num_nodes=25,
    embedding_dim=384,
    topic_distribution="clustered",  # or "uniform"
    latency_profile="mixed",         # fast, normal, slow, or mixed
    seed=42                          # for reproducibility
)

for node in network[:3]:
    print(f"{node.node_id}: topics={node.topics}, latency={node.latency_ms:.0f}ms")
```

### Topic Clusters

Nodes are assigned to topic clusters:

| Cluster | Topics |
|---------|--------|
| data_processing | csv, json, xml, parsing, etl, pandas |
| machine_learning | neural, training, inference, model, pytorch |
| web_development | http, rest, api, flask, django, frontend |
| databases | sql, nosql, query, index, postgres, mongodb |
| devops | docker, kubernetes, ci, deploy, aws, cloud |

### Latency Profiles

| Profile | Distribution | Use Case |
|---------|--------------|----------|
| fast | ~10ms (σ=3) | Local network |
| normal | ~50ms (σ=15) | Data center |
| slow | ~200ms (σ=50) | Cross-region |
| mixed | 60% normal, 30% fast, 10% slow | Realistic |

## Query Workloads

Generate representative query patterns:

```python
from benchmarks.federation.workload_generator import generate_workload, QueryType

workload = generate_workload(
    network,
    num_queries=50,
    query_mix={
        "specific": 0.4,      # High similarity to one node
        "exploratory": 0.3,   # Broad, low-variance queries
        "consensus": 0.3      # Seeking cross-node agreement
    },
    seed=42
)

# Each query includes ground truth for precision/recall
for q in workload[:3]:
    print(f"{q.query_id}: type={q.expected_type.value}, ground_truth={q.ground_truth_nodes}")
```

### Query Types

| Type | Characteristics | Ground Truth |
|------|-----------------|--------------|
| SPECIFIC | Very close to one node's centroid | Top 1-3 most similar nodes |
| EXPLORATORY | Equidistant from multiple clusters | Nodes with similarity > 0.5 |
| CONSENSUS | Close to a topic cluster | Nodes with similarity > 0.6 |

## Metrics

The benchmark collects per-query and aggregate metrics:

```python
from benchmarks.federation.metrics import QueryMetric, BenchmarkResults

# Per-query metrics
@dataclass
class QueryMetric:
    query_id: str
    query_type: QueryType
    latency_ms: float
    nodes_queried: int
    results_returned: int
    precision_at_k: float
    recall_at_k: float

# Aggregate results
@dataclass
class BenchmarkResults:
    config_name: str
    p50_latency_ms: float
    p90_latency_ms: float
    p99_latency_ms: float
    avg_precision: float
    avg_recall: float
    error_rate: float
    metrics_by_type: Dict[str, Dict]  # Breakdown by query type
```

## Running Benchmarks

### Default Configurations

The suite includes these predefined configurations:

| Config | Engine | Strategy | K | Description |
|--------|--------|----------|---|-------------|
| baseline_sum_k3 | Basic | SUM | 3 | Baseline with consensus boost |
| baseline_max_k3 | Basic | MAX | 3 | No consensus boost |
| adaptive_default | Adaptive | SUM | 1-10 | Dynamic k selection |
| hierarchical_2level | Hierarchical | SUM | 3 | Region-based routing |
| streaming_eager | Streaming | SUM | 5 | Early partial results |
| planned_auto | QueryPlanner | auto | auto | Full optimization |

### Custom Benchmarks

```python
from benchmarks.federation.benchmark_runner import (
    FederationBenchmark,
    BenchmarkConfig,
)

# Create custom config
my_config = BenchmarkConfig(
    name="high_k_diversity",
    engine_type="basic",
    strategy="diversity",
    k=7,
)

# Run benchmark
benchmark = FederationBenchmark(network)
results = benchmark.run_config(my_config, workload, runs=3)

print(f"P50 latency: {results.p50_latency_ms:.1f}ms")
print(f"Avg precision: {results.avg_precision:.3f}")
```

### Comparing Configurations

```python
from benchmarks.federation.benchmark_runner import FederationBenchmark, DEFAULT_CONFIGS

benchmark = FederationBenchmark(network)
results = benchmark.compare_configs(DEFAULT_CONFIGS, workload)

# Results is Dict[str, BenchmarkResults]
for name, result in results.items():
    print(f"{name}: latency={result.avg_latency_ms:.1f}ms, precision={result.avg_precision:.3f}")
```

## Visualizations

Generate charts (requires matplotlib):

```python
from benchmarks.federation.visualizations import (
    plot_latency_comparison,
    plot_precision_latency_tradeoff,
    plot_scalability,
    generate_report,
)

# Individual charts
plot_latency_comparison(results, "reports/latency.png")
plot_precision_latency_tradeoff(results, "reports/tradeoff.png")

# Full HTML report
html_path = generate_report(results, "reports/")
print(f"Report: {html_path}")
```

### Available Charts

1. **Latency Distribution**: Box plots comparing configurations
2. **Precision vs Latency**: Scatter plot showing tradeoffs
3. **Scalability**: Line chart of metrics vs network size
4. **Query Type Breakdown**: Bar charts by SPECIFIC/EXPLORATORY/CONSENSUS

## Scalability Testing

Test across multiple network sizes:

```python
from benchmarks.federation.benchmark_runner import run_scalability_benchmark

# Run benchmarks at 10, 25, 50 nodes
results_by_size = run_scalability_benchmark(
    sizes=[10, 25, 50],
    queries_per_size=30,
    seed=42
)

# Analyze scaling behavior
for size, configs in results_by_size.items():
    baseline = configs["baseline_sum_k3"]
    print(f"{size} nodes: P50={baseline.p50_latency_ms:.1f}ms")
```

## Interpreting Results

### Key Metrics

| Metric | Good | Concerning |
|--------|------|------------|
| P99 latency | < 5x P50 | > 10x P50 |
| Precision | > 0.8 | < 0.5 |
| Error rate | < 1% | > 5% |
| Nodes queried | ~k | >> k |

### Common Findings

1. **Adaptive-k trades latency for precision**: Higher k = more nodes = higher latency but better recall

2. **SPECIFIC queries are fastest**: Low variance means fewer nodes needed

3. **Hierarchical helps at scale**: Benefit appears around 50+ nodes

4. **Streaming adds overhead**: ~20-30% latency increase for progressive results

## CLI Reference

```bash
python -m benchmarks.federation.run_benchmarks [OPTIONS]

Options:
  --nodes TEXT      Comma-separated network sizes (default: 10,25,50)
  --queries INT     Queries per size (default: 50)
  --output TEXT     Output directory (default: reports/)
  --seed INT        Random seed (default: 42)
  --runs INT        Runs per config (default: 1)
  --configs TEXT    Comma-separated config names (default: all)
  --quick           Quick mode: fewer queries, single run
```

### Examples

```bash
# Quick validation
python -m benchmarks.federation.run_benchmarks --quick

# Compare specific configs
python -m benchmarks.federation.run_benchmarks --configs baseline_sum_k3,adaptive_default

# High statistical confidence
python -m benchmarks.federation.run_benchmarks --runs 5 --queries 100
```

## Summary

This chapter covered:

1. **Synthetic networks**: Configurable node generation with topic clusters
2. **Query workloads**: SPECIFIC, EXPLORATORY, CONSENSUS patterns
3. **Metrics**: Latency percentiles, precision, recall, error rates
4. **Benchmarking**: Running and comparing configurations
5. **Visualizations**: Charts and HTML reports
6. **Scalability**: Testing across network sizes

## Next Steps

- See `docs/guides/PERFORMANCE_TUNING.md` for optimization recommendations
- Review `docs/guides/KG_PRODUCTION_DEPLOYMENT.md` for production setup
- Explore Phase 6f adversarial robustness for security testing
