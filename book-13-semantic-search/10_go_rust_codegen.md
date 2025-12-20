<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 10: Go and Rust Code Generation

*Generating native federation engines from Prolog specifications*

## Overview

While Python provides excellent prototyping and flexibility, production deployments often require:
- **Go**: Excellent concurrency, single binary deployment, low memory footprint
- **Rust**: Zero-cost abstractions, memory safety, embedded systems support

UnifyWeaver's polyglot architecture allows the same Prolog service definition to generate code for any target language. This chapter covers generating Go and Rust implementations of the Phase 5 advanced federation features.

## Why Multi-Language Code Generation?

| Aspect | Python | Go | Rust |
|--------|--------|-------|------|
| Startup time | ~100ms | ~5ms | ~5ms |
| Memory usage | High | Low | Lowest |
| Deployment | Virtualenv | Single binary | Single binary |
| Concurrency | GIL-limited | Goroutines | async/await |
| Best for | Prototyping, ML | Microservices | Embedded, perf-critical |

A typical deployment might use:
- Python for development and ML-heavy nodes
- Go for high-throughput aggregator nodes
- Rust for edge nodes with constrained resources

## Code Generation Architecture

```
┌─────────────────────────────────────────────────────────┐
│                   Prolog Service Definition              │
│  service(my_node, [federation([adaptive_k(true)])], _)  │
└─────────────────────────────────────────────────────────┘
                            │
            ┌───────────────┼───────────────┐
            ▼               ▼               ▼
    ┌─────────────┐  ┌─────────────┐  ┌─────────────┐
    │python_target│  │  go_target  │  │ rust_target │
    │     .pl     │  │     .pl     │  │     .pl     │
    └─────────────┘  └─────────────┘  └─────────────┘
            │               │               │
            ▼               ▼               ▼
    ┌─────────────┐  ┌─────────────┐  ┌─────────────┐
    │  Python     │  │    Go       │  │   Rust      │
    │  Classes    │  │   Structs   │  │  Structs    │
    │  + asyncio  │  │ + goroutines│  │ + tokio     │
    └─────────────┘  └─────────────┘  └─────────────┘
```

## Phase 5 Code Generation Predicates

### Go Predicates

| Predicate | Phase | Generated Code |
|-----------|-------|----------------|
| `compile_adaptive_federation_go/2` | 5b | AdaptiveKCalculator, AdaptiveFederatedEngine |
| `compile_query_planner_go/2` | 5c | QueryPlanner, PlanExecutor, PlannedQueryEngine |
| `compile_hierarchical_federation_go/2` | 5a | NodeHierarchy, HierarchicalFederatedEngine |
| `compile_streaming_federation_go/2` | 5d | PartialResult, StreamingFederatedEngine |

### Rust Predicates

| Predicate | Phase | Generated Code |
|-----------|-------|----------------|
| `compile_adaptive_federation_rust/2` | 5b | AdaptiveKConfig, AdaptiveKCalculator |
| `compile_query_planner_rust/2` | 5c | QueryType, QueryPlan, QueryPlanner |
| `compile_hierarchical_federation_rust/2` | 5a | RegionalNode, NodeHierarchy |
| `compile_streaming_federation_rust/2` | 5d | PartialResult, StreamingFederatedEngine |

## Generated Go Code Examples

### Phase 5b: Adaptive Federation-K

```prolog
% Generate Go code for adaptive-k federation
?- use_module('src/unifyweaver/targets/go_target'),
   compile_adaptive_federation_go([
       adaptive_k([
           base_k(5),
           min_k(2),
           max_k(10),
           entropy_threshold(0.7)
       ])
   ], GoCode).
```

Generated Go structures:

```go
// AdaptiveKConfig holds configuration for adaptive k selection
type AdaptiveKConfig struct {
    BaseK              int     `json:"base_k"`
    MinK               int     `json:"min_k"`
    MaxK               int     `json:"max_k"`
    EntropyWeight      float64 `json:"entropy_weight"`
    LatencyWeight      float64 `json:"latency_weight"`
    ConsensusWeight    float64 `json:"consensus_weight"`
    EntropyThreshold   float64 `json:"entropy_threshold"`
    SimilarityThreshold float64 `json:"similarity_threshold"`
    ConsensusThreshold float64 `json:"consensus_threshold"`
    HistorySize        int     `json:"history_size"`
}

// QueryMetrics holds metrics for adaptive k computation
type QueryMetrics struct {
    Entropy             float64
    TopSimilarity       float64
    HistoricalConsensus float64
    AvgNodeLatency      float64
}

// AdaptiveKCalculator computes optimal federation_k
type AdaptiveKCalculator struct {
    config       AdaptiveKConfig
    queryHistory []QueryHistoryEntry
    historyMu    sync.RWMutex
}

func (c *AdaptiveKCalculator) ComputeK(
    embedding []float32,
    nodes []KGNode,
    latencyBudgetMs *int,
) int {
    metrics := c.computeMetrics(embedding, nodes)
    k := c.config.BaseK

    // Adjust based on query characteristics
    if metrics.Entropy > c.config.EntropyThreshold {
        k += 2 // Ambiguous query needs more nodes
    }
    if metrics.TopSimilarity < c.config.SimilarityThreshold {
        k += 1 // No strong match
    }
    if metrics.HistoricalConsensus < c.config.ConsensusThreshold {
        k += 1 // Past queries needed more nodes
    }

    // Clamp to valid range
    return clamp(k, c.config.MinK, min(c.config.MaxK, len(nodes)))
}
```

### Phase 5c: Query Plan Optimization

```prolog
% Generate Go query planner
?- compile_query_planner_go([
       query_planning([
           specific_threshold(0.8),
           exploratory_variance(0.1)
       ])
   ], GoCode).
```

Generated Go query classification:

```go
type QueryType int

const (
    QueryTypeSpecific QueryType = iota
    QueryTypeExploratory
    QueryTypeConsensus
)

type QueryClassification struct {
    Type          QueryType
    MaxSimilarity float64
    Variance      float64
    TopNodes      []string
    Confidence    float64
}

type QueryPlanner struct {
    router    *KleinbergRouter
    config    PlannerConfig
    planCount uint64
}

func (p *QueryPlanner) ClassifyQuery(
    embedding []float32,
    nodes []KGNode,
) QueryClassification {
    similarities := make([]float64, len(nodes))
    for i, node := range nodes {
        similarities[i] = cosineSimilarity(embedding, node.Centroid)
    }

    maxSim := max(similarities)
    variance := computeVariance(similarities)

    if maxSim >= p.config.SpecificThreshold {
        return QueryClassification{
            Type:       QueryTypeSpecific,
            Confidence: min(maxSim, 1.0),
        }
    } else if variance >= p.config.ExploratoryVariance {
        return QueryClassification{
            Type:       QueryTypeExploratory,
            Confidence: min(variance*5.0, 1.0),
        }
    }
    return QueryClassification{
        Type:       QueryTypeConsensus,
        Confidence: 0.7,
    }
}
```

### Phase 5a: Hierarchical Federation

```prolog
% Generate Go hierarchical federation
?- compile_hierarchical_federation_go([
       hierarchical([
           min_nodes_per_region(3),
           drill_down_k(2)
       ])
   ], GoCode).
```

Generated Go hierarchy structures:

```go
type RegionalNode struct {
    RegionID   string    `json:"region_id"`
    Centroid   []float32 `json:"centroid"`
    Topics     []string  `json:"topics"`
    ChildNodes []string  `json:"child_nodes"`
    Level      int       `json:"level"`
}

type NodeHierarchy struct {
    config       HierarchyConfig
    regions      map[string]*RegionalNode
    nodeToRegion map[string]string
    leafNodes    map[string]*KGNode
    mu           sync.RWMutex
}

func (h *NodeHierarchy) BuildFromNodes(nodes []KGNode) {
    h.mu.Lock()
    defer h.mu.Unlock()

    // Clear existing hierarchy
    h.regions = make(map[string]*RegionalNode)
    h.nodeToRegion = make(map[string]string)

    // Cluster nodes by centroid similarity
    assigned := make(map[string]bool)
    groupID := 0

    for _, node := range nodes {
        if assigned[node.NodeID] || len(node.Centroid) == 0 {
            continue
        }

        // Find similar unassigned nodes
        cluster := []KGNode{node}
        assigned[node.NodeID] = true

        for _, other := range nodes {
            if assigned[other.NodeID] {
                continue
            }
            sim := cosineSimilarity(node.Centroid, other.Centroid)
            if sim >= h.config.CentroidSimilarityThreshold {
                cluster = append(cluster, other)
                assigned[other.NodeID] = true
            }
        }

        // Create regional node
        regionID := fmt.Sprintf("region_%d", groupID)
        groupID++

        h.regions[regionID] = &RegionalNode{
            RegionID:   regionID,
            Centroid:   computeAverageCentroid(cluster),
            ChildNodes: extractNodeIDs(cluster),
            Level:      0,
        }
    }
}
```

### Phase 5d: Streaming Federation

```prolog
% Generate Go streaming federation
?- compile_streaming_federation_go([
       streaming([
           min_confidence(0.3),
           yield_interval_ms(100)
       ])
   ], GoCode).
```

Generated Go streaming with channels:

```go
type PartialResult struct {
    Results        []AggregatedResult `json:"results"`
    Confidence     float64            `json:"confidence"`
    NodesResponded int                `json:"nodes_responded"`
    NodesTotal     int                `json:"nodes_total"`
    IsFinal        bool               `json:"is_final"`
}

type StreamingFederatedEngine struct {
    engine *FederatedQueryEngine
    config StreamingConfig
}

func (e *StreamingFederatedEngine) StreamingQuery(
    ctx context.Context,
    queryText string,
    embedding []float32,
    topK int,
) (<-chan PartialResult, error) {
    nodes, err := e.engine.router.DiscoverNodes(nil)
    if err != nil {
        return nil, err
    }

    results := make(chan PartialResult, len(nodes)+1)

    go func() {
        defer close(results)

        aggregated := make(map[string]*AggregatedResult)
        var mu sync.Mutex
        var wg sync.WaitGroup
        responded := int32(0)

        // Launch concurrent queries
        for _, node := range nodes {
            wg.Add(1)
            go func(n KGNode) {
                defer wg.Done()

                resp, err := e.queryNode(ctx, n, queryText, embedding)
                if err != nil {
                    return
                }

                mu.Lock()
                e.mergeResults(aggregated, resp)
                count := atomic.AddInt32(&responded, 1)
                confidence := float64(count) / float64(len(nodes))

                if confidence >= e.config.MinConfidence {
                    results <- PartialResult{
                        Results:        e.rankResults(aggregated),
                        Confidence:     confidence,
                        NodesResponded: int(count),
                        NodesTotal:     len(nodes),
                        IsFinal:        false,
                    }
                }
                mu.Unlock()
            }(node)
        }

        wg.Wait()

        // Send final result
        mu.Lock()
        results <- PartialResult{
            Results:        e.rankResults(aggregated),
            Confidence:     float64(responded) / float64(len(nodes)),
            NodesResponded: int(responded),
            NodesTotal:     len(nodes),
            IsFinal:        true,
        }
        mu.Unlock()
    }()

    return results, nil
}
```

## Generated Rust Code Examples

### Phase 5b: Adaptive Federation-K

```prolog
% Generate Rust adaptive-k
?- use_module('src/unifyweaver/targets/rust_target'),
   compile_adaptive_federation_rust([
       adaptive_k([base_k(5), entropy_threshold(0.6)])
   ], RustCode).
```

Generated Rust with async/await:

```rust
#[derive(Debug, Clone)]
pub struct AdaptiveKConfig {
    pub base_k: usize,
    pub min_k: usize,
    pub max_k: usize,
    pub entropy_threshold: f64,
}

impl Default for AdaptiveKConfig {
    fn default() -> Self {
        Self {
            base_k: 5,
            min_k: 2,
            max_k: 10,
            entropy_threshold: 0.6,
        }
    }
}

pub struct AdaptiveKCalculator {
    config: AdaptiveKConfig,
    query_history: RwLock<Vec<(Vec<f32>, f64, usize)>>,
}

impl AdaptiveKCalculator {
    pub fn new(config: AdaptiveKConfig) -> Self {
        Self {
            config,
            query_history: RwLock::new(Vec::new()),
        }
    }

    pub async fn compute_k(
        &self,
        query_embedding: &[f32],
        nodes: &[KGNode],
    ) -> usize {
        if nodes.is_empty() {
            return self.config.min_k;
        }

        // Compute similarities
        let similarities: Vec<f64> = nodes
            .iter()
            .map(|n| cosine_similarity(query_embedding, &n.centroid))
            .collect();

        // Compute normalized entropy
        let sum: f64 = similarities.iter().map(|s| s.abs()).sum();
        let entropy = if sum > 0.0 && similarities.len() > 1 {
            let probs: Vec<f64> = similarities
                .iter()
                .map(|s| s.abs() / (sum + 1e-10))
                .collect();
            -probs
                .iter()
                .filter(|&&p| p > 1e-10)
                .map(|p| p * p.ln())
                .sum::<f64>()
                / (similarities.len() as f64).ln()
        } else {
            0.5
        };

        let max_sim = similarities
            .iter()
            .cloned()
            .fold(f64::NEG_INFINITY, f64::max);

        // Adjust k based on characteristics
        let mut k = self.config.base_k;
        if entropy > self.config.entropy_threshold {
            k += 2;
        }
        if max_sim < 0.5 {
            k += 1;
        }

        k.clamp(self.config.min_k, self.config.max_k.min(nodes.len()))
    }
}
```

### Phase 5d: Streaming Federation

```rust
use tokio::sync::mpsc;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PartialResult {
    pub results: Vec<AggregatedResult>,
    pub confidence: f64,
    pub nodes_responded: usize,
    pub nodes_total: usize,
    pub is_final: bool,
}

pub struct StreamingFederatedEngine {
    engine: FederatedQueryEngine,
    config: StreamingConfig,
}

impl StreamingFederatedEngine {
    pub fn new(router: Arc<KleinbergRouter>) -> Self {
        Self {
            engine: FederatedQueryEngine::new(router),
            config: StreamingConfig::default(),
        }
    }

    pub async fn streaming_query(
        &self,
        query_text: &str,
        query_embedding: &[f32],
        top_k: usize,
    ) -> Result<mpsc::Receiver<PartialResult>, Box<dyn std::error::Error + Send + Sync>> {
        let nodes = self.engine.router.discover_nodes(None)?;
        let (tx, rx) = mpsc::channel(nodes.len() + 1);

        let nodes_total = nodes.len();
        let min_conf = self.config.min_confidence;
        let query_text = query_text.to_string();
        let embedding = query_embedding.to_vec();

        tokio::spawn(async move {
            let mut aggregated: HashMap<String, AggregatedResult> = HashMap::new();
            let mut responded = 0;

            for node in nodes {
                // Simulate querying each node
                responded += 1;
                let confidence = responded as f64 / nodes_total as f64;

                if confidence >= min_conf {
                    let results: Vec<_> = aggregated.values().cloned().collect();
                    let _ = tx
                        .send(PartialResult {
                            results,
                            confidence,
                            nodes_responded: responded,
                            nodes_total,
                            is_final: false,
                        })
                        .await;
                }
            }

            // Send final result
            let results: Vec<_> = aggregated.values().cloned().collect();
            let _ = tx
                .send(PartialResult {
                    results,
                    confidence: responded as f64 / nodes_total as f64,
                    nodes_responded: responded,
                    nodes_total,
                    is_final: true,
                })
                .await;
        });

        Ok(rx)
    }
}
```

## Configuration Mapping

Prolog options map directly to generated code defaults:

| Prolog Option | Go Field | Rust Field |
|--------------|----------|------------|
| `base_k(5)` | `BaseK: 5` | `base_k: 5` |
| `entropy_threshold(0.7)` | `EntropyThreshold: 0.7` | `entropy_threshold: 0.7` |
| `min_confidence(0.3)` | `MinConfidence: 0.3` | `min_confidence: 0.3` |
| `drill_down_k(2)` | `DrillDownK: 2` | `drill_down_k: 2` |

## Testing Generated Code

```prolog
% Test all Phase 5 predicates generate valid code
?- use_module('src/unifyweaver/targets/go_target'),
   use_module('src/unifyweaver/targets/rust_target'),

   % Go tests
   compile_adaptive_federation_go([adaptive_k([base_k(5)])], GoAdapt),
   string_length(GoAdapt, GoAdaptLen),
   format('Go Adaptive-K: ~w chars~n', [GoAdaptLen]),

   compile_query_planner_go([query_planning([])], GoPlan),
   string_length(GoPlan, GoPlanLen),
   format('Go Query Planner: ~w chars~n', [GoPlanLen]),

   % Rust tests
   compile_adaptive_federation_rust([adaptive_k([])], RustAdapt),
   string_length(RustAdapt, RustAdaptLen),
   format('Rust Adaptive-K: ~w chars~n', [RustAdaptLen]),

   compile_streaming_federation_rust([streaming([])], RustStream),
   string_length(RustStream, RustStreamLen),
   format('Rust Streaming: ~w chars~n', [RustStreamLen]).
```

Expected output:
```
Go Adaptive-K: 11166 chars
Go Query Planner: 13227 chars
Rust Adaptive-K: 2112 chars
Rust Streaming: 2138 chars
```

## Deployment Patterns

### Homogeneous Cluster (Single Language)

```
┌─────────────────────────────────────────┐
│            Load Balancer                │
└─────────────────────────────────────────┘
        │           │           │
        ▼           ▼           ▼
   ┌────────┐  ┌────────┐  ┌────────┐
   │ Go Node│  │ Go Node│  │ Go Node│
   └────────┘  └────────┘  └────────┘
```

### Heterogeneous Cluster (Mixed Languages)

```
                 ┌────────────────┐
                 │  Python        │
                 │  Aggregator    │
                 │  (ML-heavy)    │
                 └────────────────┘
                        │
        ┌───────────────┼───────────────┐
        ▼               ▼               ▼
   ┌────────┐     ┌────────┐     ┌────────┐
   │ Go Node│     │ Go Node│     │ Rust   │
   │ (CPU)  │     │ (GPU)  │     │ (Edge) │
   └────────┘     └────────┘     └────────┘
```

### Edge + Cloud Hybrid

```
┌─────────────────────────────────────────┐
│              Cloud Layer                │
│  ┌────────────────────────────────────┐ │
│  │     Python Federation Hub          │ │
│  │     (Query planning, ML fusion)    │ │
│  └────────────────────────────────────┘ │
└─────────────────────────────────────────┘
                    │
    ┌───────────────┼───────────────┐
    ▼               ▼               ▼
┌────────┐     ┌────────┐     ┌────────┐
│ Rust   │     │ Rust   │     │ Rust   │
│ Edge   │     │ Edge   │     │ Edge   │
│ Node 1 │     │ Node 2 │     │ Node 3 │
└────────┘     └────────┘     └────────┘
```

## Summary

This chapter covered:

1. **Why multi-language code generation matters** for production deployments
2. **Go code generation** with goroutines, channels, and sync primitives
3. **Rust code generation** with tokio async/await and mpsc channels
4. **Configuration mapping** from Prolog options to generated defaults
5. **Deployment patterns** for homogeneous and heterogeneous clusters

The polyglot architecture allows you to:
- Prototype in Python
- Deploy high-throughput services in Go
- Run resource-constrained edge nodes in Rust
- Mix languages in a single federation

## Next Steps

- [Chapter 11: Benchmarking Federation](11_benchmarking.md) - Performance comparison across targets
- [Adversarial Robustness Proposal](../../docs/proposals/ADVERSARIAL_ROBUSTNESS.md) - Protecting against malicious nodes
