# Chapter 8: Advanced Federation Features

Building on the distributed search (Chapter 6) and density scoring (Chapter 7), this chapter covers Phase 5's advanced federation capabilities: adaptive node selection, query planning, hierarchical aggregation, and streaming results.

## The Challenge

Basic federated queries use a fixed number of nodes (`federation_k=3`). But real-world queries vary:

- **Specific queries** ("What is the CSV delimiter?") need few nodes
- **Exploratory queries** ("How do I process data?") benefit from many nodes
- **Ambiguous queries** need broader consensus before confidence

How do we adapt our federation strategy dynamically?

## Adaptive Federation-K (Phase 5b)

### Query Metrics

We analyze each query to determine optimal node count:

```python
from dataclasses import dataclass
import numpy as np

@dataclass
class QueryMetrics:
    """Metrics for adaptive k selection."""
    entropy: float              # Semantic diversity (0-1)
    top_similarity: float       # Best node match (0-1)
    similarity_variance: float  # Spread of similarities
    historical_consensus: float # Past query agreement
    avg_node_latency_ms: float  # Expected response time
```

### Entropy Calculation

High entropy = ambiguous query = need more nodes:

```python
def compute_query_entropy(similarities: List[float]) -> float:
    """Compute entropy of similarity distribution.

    High entropy means similarities are spread out (ambiguous).
    Low entropy means one node dominates (specific).
    """
    # Normalize to probabilities
    total = sum(similarities)
    if total < 1e-10:
        return 1.0  # Maximum uncertainty

    probs = [s / total for s in similarities]

    # Shannon entropy (normalized to 0-1)
    entropy = 0.0
    for p in probs:
        if p > 1e-10:
            entropy -= p * np.log2(p)

    max_entropy = np.log2(len(similarities))
    return entropy / max_entropy if max_entropy > 0 else 0.0
```

### Adaptive K Calculator

```python
@dataclass
class AdaptiveKConfig:
    base_k: int = 3
    min_k: int = 1
    max_k: int = 10
    entropy_weight: float = 0.3
    latency_weight: float = 0.2
    consensus_weight: float = 0.5
    entropy_threshold: float = 0.7
    similarity_threshold: float = 0.5

class AdaptiveKCalculator:
    def compute_k(self, metrics: QueryMetrics, num_nodes: int) -> int:
        """Compute optimal federation_k based on query characteristics."""
        k_adjust = 0

        # High entropy (ambiguous) → more nodes
        if metrics.entropy > self.config.entropy_threshold:
            k_adjust += 2

        # Low top similarity → more nodes
        if metrics.top_similarity < self.config.similarity_threshold:
            k_adjust += 1

        # Historical low consensus → more nodes
        if metrics.historical_consensus < 0.6:
            k_adjust += 1

        # Latency constraint → fewer nodes
        if metrics.avg_node_latency_ms > 500:
            k_adjust -= 1

        optimal_k = self.config.base_k + k_adjust
        return max(self.config.min_k, min(optimal_k, self.config.max_k, num_nodes))
```

### Using Adaptive Engine

```python
from federated_query import create_adaptive_engine

engine = create_adaptive_engine(
    router=router,
    base_k=3,
    entropy_weight=0.3,
    latency_budget_ms=500
)

# Engine automatically adjusts k per query
response = engine.federated_query(
    "How do I parse CSV files?",
    query_embedding
)

# Check what k was used
stats = engine.get_stats()
print(f"Used k={stats['last_k']}, entropy={stats['last_entropy']:.2f}")
```

## Query Plan Optimization (Phase 5c)

### Query Classification

Different queries need different strategies:

```python
from enum import Enum

class QueryType(Enum):
    SPECIFIC = "specific"      # High similarity to one node
    EXPLORATORY = "exploratory"  # Low similarity, high variance
    CONSENSUS = "consensus"    # Medium, need agreement
```

Classification logic:

```python
def classify_query(similarities: List[float]) -> QueryType:
    max_sim = max(similarities)
    variance = np.var(similarities)

    if max_sim >= 0.8:
        return QueryType.SPECIFIC
    elif variance >= 0.1:
        return QueryType.EXPLORATORY
    else:
        return QueryType.CONSENSUS
```

### Query Plans

Each type gets a different execution plan:

```python
@dataclass
class QueryPlanStage:
    stage_id: int
    nodes: List[str]
    strategy: AggregationStrategy
    parallel: bool = True
    depends_on: List[int] = field(default_factory=list)

@dataclass
class QueryPlan:
    plan_id: str
    query_type: QueryType
    stages: List[QueryPlanStage]

    def get_execution_order(self) -> List[List[QueryPlanStage]]:
        """Return stages grouped by dependency level."""
        # DAG topological sort
        ...
```

### Strategy by Query Type

| Query Type | Strategy | Nodes | Aggregation |
|------------|----------|-------|-------------|
| SPECIFIC | Greedy | 1-2 | MAX |
| EXPLORATORY | Broad | 5-7 | SUM |
| CONSENSUS | Two-stage | 5 → 3 | SUM → DENSITY_FLUX |

### Two-Stage Consensus Plan

For consensus queries, we use a two-stage DAG:

```
Stage 0: Broad query (5 nodes, SUM)
    ↓
Stage 1: Density refinement (3 nodes from Stage 0 results, DENSITY_FLUX)
```

```python
def build_consensus_plan(nodes: List[KGNode]) -> QueryPlan:
    stage0 = QueryPlanStage(
        stage_id=0,
        nodes=[n.node_id for n in nodes[:5]],
        strategy=AggregationStrategy.SUM,
        parallel=True
    )

    stage1 = QueryPlanStage(
        stage_id=1,
        nodes=[],  # Determined at runtime from stage0 results
        strategy=AggregationStrategy.DENSITY_FLUX,
        parallel=True,
        depends_on=[0]  # Wait for stage0
    )

    return QueryPlan(
        plan_id=str(uuid.uuid4()),
        query_type=QueryType.CONSENSUS,
        stages=[stage0, stage1]
    )
```

### Using the Planner

```python
from query_planner import create_planned_engine

engine = create_planned_engine(router)

# Returns both response and the plan used
response, plan = engine.query("What patterns work best?", embedding)

print(f"Query type: {plan.query_type.value}")
print(f"Stages: {len(plan.stages)}")
for stage in plan.stages:
    print(f"  Stage {stage.stage_id}: {len(stage.nodes)} nodes, {stage.strategy}")
```

## Hierarchical Federation (Phase 5a)

### The Problem

With 100+ nodes, querying all is expensive. Hierarchical federation organizes nodes into regions for efficient routing.

### Regional Nodes

```python
@dataclass
class RegionalNode:
    region_id: str
    centroid: np.ndarray    # Average of children
    topics: List[str]       # Combined topics
    child_nodes: List[str]  # Node IDs
    parent_region: Optional[str] = None
    level: int = 0          # 0 = top level
```

### Building the Hierarchy

Nodes are grouped by topic overlap, then by centroid similarity:

```python
class NodeHierarchy:
    def build_from_nodes(self, nodes: List[KGNode]) -> None:
        # 1. Group by topic overlap
        topic_groups = self._group_by_topics(nodes)

        # 2. Create regions from groups
        for group_id, group_nodes in topic_groups.items():
            if len(group_nodes) >= self.config.min_nodes_per_region:
                self._create_region(group_id, group_nodes)

        # 3. Handle ungrouped nodes by centroid similarity
        ungrouped = [n for n in nodes if n.node_id not in self.node_to_region]
        self._group_by_centroid(ungrouped)
```

### Hierarchical Query Execution

Two-level routing:

```
Query → Rank Regions → Top K Regions → Query Children → Aggregate
```

```python
class HierarchicalFederatedEngine(FederatedQueryEngine):
    def federated_query(self, query_text, embedding, top_k=5):
        # Level 1: Query regional nodes
        regions = self.hierarchy.get_regional_nodes(level=0)
        ranked_regions = self._rank_regions(embedding, regions)

        # Select top regions to drill into
        selected = ranked_regions[:self.drill_down_k]

        # Level 2: Query children in selected regions
        all_responses = []
        for region, sim in selected:
            children = self.hierarchy.get_child_nodes(region.region_id)
            response = self._query_children(children, query_text, embedding)
            all_responses.append((response, sim))

        # Aggregate with region similarity weighting
        return self._aggregate_hierarchical(all_responses)
```

### Configuration

```python
engine = create_hierarchical_engine(
    router=router,
    max_levels=3,
    min_nodes_per_region=2,
    max_nodes_per_region=10,
    drill_down_k=2  # Query top 2 regions
)
```

## Streaming Aggregation (Phase 5d)

### The Problem

Long-running federated queries block until all nodes respond. Users want to see preliminary results immediately.

### Partial Results

```python
@dataclass
class PartialResult:
    results: List[Dict[str, Any]]
    confidence: float        # nodes_responded / nodes_total
    nodes_responded: int
    nodes_total: int
    elapsed_ms: float
    is_final: bool = False
```

### AsyncGenerator Streaming

```python
class StreamingFederatedEngine(FederatedQueryEngine):
    async def federated_query_streaming(
        self,
        query_text: str,
        embedding: np.ndarray,
        **kwargs
    ) -> AsyncGenerator[PartialResult, None]:
        """Yield partial results as nodes respond."""

        # Create async tasks for all nodes
        tasks = {
            asyncio.create_task(self._async_query_node(node, ...))
            for node in nodes
        }

        # Process as they complete
        for coro in asyncio.as_completed(tasks):
            response = await coro
            self._merge_response(aggregated, response)
            responded += 1

            # Yield partial result
            if self._should_yield(responded, elapsed):
                yield PartialResult(
                    results=self._normalize(aggregated),
                    confidence=responded / len(nodes),
                    nodes_responded=responded,
                    nodes_total=len(nodes),
                    elapsed_ms=elapsed,
                    is_final=(responded == len(nodes))
                )
```

### Server-Sent Events (SSE)

For HTTP/2 streaming to browsers:

```python
async def federated_query_sse(self, query_text, embedding, **kwargs):
    """Yield SSE-formatted strings for HTTP streaming."""
    async for partial in self.federated_query_streaming(...):
        yield f"data: {json.dumps(partial.to_dict())}\n\n"
```

Flask endpoint:

```python
@app.route('/kg/stream')
def stream_query():
    def generate():
        async for sse_line in engine.federated_query_sse(query, embedding):
            yield sse_line
    return Response(generate(), mimetype='text/event-stream')
```

### Streaming Configuration

```python
@dataclass
class StreamingConfig:
    yield_interval_ms: int = 100   # Min time between yields
    min_confidence: float = 0.1    # Min confidence before first yield
    max_wait_ms: int = 5000        # Max wait for slow nodes
    eager_yield: bool = True       # Yield on first response

engine = create_streaming_engine(
    router=router,
    yield_interval_ms=100,
    eager_yield=True
)
```

## Prolog Configuration

All Phase 5 features are configurable via Prolog:

```prolog
service(advanced_kg_node, [
    transport(http('/kg', [port(8080)])),
    federation([
        % Phase 5b: Adaptive K
        adaptive_k([
            base_k(3),
            entropy_weight(0.3),
            latency_budget_ms(500)
        ]),

        % Phase 5c: Query Planning
        query_planning(enabled),

        % Phase 5a: Hierarchical
        hierarchical([
            max_levels(3),
            drill_down_k(2)
        ]),

        % Phase 5d: Streaming
        streaming([
            yield_interval_ms(100),
            eager_yield(true),
            sse_enabled(true)
        ])
    ])
], [
    receive(Query),
    handle_federated_query(Query, Response),
    respond(Response)
]).
```

## Combining Features

These features compose naturally:

```python
# Hierarchical + Adaptive + Streaming
engine = HierarchicalFederatedEngine(
    router=router,
    hierarchy=hierarchy
)

# Wrap with adaptive k
adaptive_engine = AdaptiveFederatedEngine(
    router=router,
    base_engine=engine
)

# Stream results
async for partial in adaptive_engine.streaming_query(...):
    display_partial_results(partial)
```

## Summary

Phase 5 adds four capabilities that make federated queries smarter and more responsive:

| Feature | Problem Solved | Key Insight |
|---------|----------------|-------------|
| Adaptive K | Fixed k is suboptimal | Query entropy predicts optimal node count |
| Query Planning | One-size-fits-all strategy | Classify queries, select appropriate plan |
| Hierarchical | Too many nodes | Group by topic/centroid, drill down |
| Streaming | Blocking waits | Yield partial results via AsyncGenerator |

## What's Next

- **Chapter 9**: Production deployment and monitoring
- **Cross-model federation**: Handling nodes with different embedding models
- **Adversarial robustness**: Detecting malicious density inflation
