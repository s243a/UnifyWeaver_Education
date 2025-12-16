<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 7: Cross-Target Deployment

**Train in Python, Deploy Anywhere**

## The Multi-Runtime Philosophy

UnifyWeaver supports training in Python (best ML ecosystem) and deploying in the runtime that fits your use case:

| Runtime | Best For |
|---------|----------|
| Python | Training, experimentation, notebooks |
| Go | Microservices, CLIs, cross-compilation |
| Rust | Performance-critical apps, embedded, WASM |

## The Universal Storage Format

All runtimes share the same storage format:

```
lda.db                    # SQLite (schema, metadata)
embeddings/
└── {model_name}/
    ├── mh_{id}_cluster_{n}_centroid.npy  # Cluster centroids
    └── mh_{id}_cluster_{n}_answer.npy    # Answer embeddings
```

NPY files are the lingua franca—simple binary format readable by all targets.

## Python: Training & Inference

Full-featured with PyTorch:

```python
from lda_database import LDAProjectionDB
from projection_transformer import ProjectionTransformer

# Load trained model
db = LDAProjectionDB('lda.db')
mh_proj = db.get_multi_head_projection(1)

# Or load distilled transformer
transformer = ProjectionTransformer.load('transformer.pt')

# Inference
projected = transformer.project(query_embedding)
```

## Go: Microservices & CLIs

Pure Go with ONNX Runtime:

```go
package main

import (
    "github.com/unifyweaver/projection"
)

func main() {
    // Load multi-head projection
    proj, err := projection.LoadMultiHead(projection.Config{
        DataDir:        "embeddings/all-MiniLM-L6-v2",
        MHProjectionID: 1,
        Temperature:    0.1,
    })
    if err != nil {
        panic(err)
    }

    // Project a query
    queryEmb := embedder.Embed("How do I authenticate?")
    projected, weights := proj.Project(queryEmb)

    // Use projected embedding for search
    results := searcher.Search(projected, 10)
}
```

### Go NPY Loading

```go
// Load NPY file
func LoadNPY(path string) ([]float32, error) {
    // NPY format: magic + header + data
    // UnifyWeaver provides npy.Load() helper
    return npy.Load(path)
}
```

## Rust: High Performance

Zero-copy where possible:

```rust
use unifyweaver::projection::MultiHeadProjection;

fn main() -> Result<()> {
    // Load from unified model files
    let proj = MultiHeadProjection::load_unified(
        "embeddings/all-MiniLM-L6-v2",
        1,    // mh_projection_id
        0.1   // temperature
    )?;

    // Project query
    let query_emb = embedder.embed("How do I authenticate?")?;
    let (projected, weights) = proj.project(&query_emb);

    // Search
    let results = searcher.vector_search_with_projection(
        &query_emb,
        Some(&proj),
        10
    )?;

    Ok(())
}
```

### Rust NPY Loading

```rust
use ndarray::Array1;
use std::fs::File;
use std::io::BufReader;

fn load_npy(path: &str) -> Result<Array1<f32>> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    // Parse NPY header and load data
    npy::read_npy(reader)
}
```

## Unified Model Discovery

All runtimes use the same discovery pattern:

```
1. Read mh_projection_id from config
2. Glob for: mh_{id}_cluster_*_centroid.npy
3. Extract cluster IDs from filenames
4. Load centroid and answer NPY for each cluster
```

### Python

```python
import glob

pattern = f"embeddings/*/mh_{mh_id}_cluster_*_centroid.npy"
centroid_files = sorted(glob.glob(pattern))

for path in centroid_files:
    cluster_id = extract_cluster_id(path)
    centroid = np.load(path)
    answer_path = path.replace('_centroid.npy', '_answer.npy')
    answer = np.load(answer_path)
```

### Go

```go
pattern := fmt.Sprintf("embeddings/*/mh_%d_cluster_*_centroid.npy", mhID)
matches, _ := filepath.Glob(pattern)

for _, path := range matches {
    clusterID := extractClusterID(path)
    centroid := loadNPY(path)
    answerPath := strings.Replace(path, "_centroid.npy", "_answer.npy", 1)
    answer := loadNPY(answerPath)
}
```

### Rust

```rust
let pattern = format!("embeddings/*/mh_{}_cluster_*_centroid.npy", mh_id);
for entry in glob(&pattern)? {
    let path = entry?;
    let cluster_id = extract_cluster_id(&path);
    let centroid = load_npy(&path)?;
    let answer_path = path.replace("_centroid.npy", "_answer.npy");
    let answer = load_npy(&answer_path)?;
}
```

## Performance Comparison

Benchmarks on multi-head projection (18 heads, 1000 queries):

| Runtime | Single Query | Batch (32) | Notes |
|---------|-------------|------------|-------|
| Python (NumPy) | 0.046 ms | 0.046 ms | CPU, optimized BLAS |
| Go (ONNX) | 0.12 ms | 0.08 ms | Pure Go |
| Rust (ndarray) | 0.03 ms | 0.02 ms | Zero-copy, SIMD |

*Note: Actual performance varies by hardware and implementation.*

## Deployment Patterns

### Pattern 1: Python Training Server + Rust Inference

```
┌────────────────┐     ┌────────────────┐
│ Training       │     │ Inference      │
│ (Python)       │────►│ (Rust CLI)     │
│                │     │                │
│ - PyTorch      │ NPY │ - PtSearcher   │
│ - Training     │     │ - Fast queries │
└────────────────┘     └────────────────┘
```

### Pattern 2: Go Microservice

```
┌────────────────┐     ┌────────────────┐
│ Training       │     │ API Server     │
│ (Python)       │────►│ (Go)           │
│                │     │                │
│ - Offline      │ NPY │ - REST/gRPC    │
│ - Periodic     │     │ - Horizontal   │
└────────────────┘     │   scaling      │
                       └────────────────┘
```

### Pattern 3: Edge Deployment

```
┌────────────────┐     ┌────────────────┐
│ Training       │     │ Mobile/Edge    │
│ (Cloud Python) │────►│ (Rust/WASM)    │
│                │     │                │
│ - GPU training │ NPY │ - Small binary │
│ - Full models  │     │ - Offline      │
└────────────────┘     └────────────────┘
```

## Practical Exercise

Deploy the same model in multiple runtimes:

```bash
# Train in Python
python3 scripts/train_multi_head_projection.py --db lda.db

# Test in Python
python3 -c "
from lda_database import LDAProjectionDB
db = LDAProjectionDB('lda.db')
# ... inference
"

# Test in Go
cd src/unifyweaver/targets/go_runtime
go test ./projection/... -v

# Test in Rust
cd examples/pearltrees
cargo test --bin demo_bookmark_filing
```

## Chapter Summary

- Train once in Python, deploy in Python/Go/Rust
- NPY files provide universal storage format
- Unified naming convention enables automatic discovery
- Each runtime has strengths: Python (ML), Go (services), Rust (performance)
- Choose based on deployment requirements

## Next Chapter

[Chapter 8: Advanced Topics](08_advanced_topics.md) - Regularization, hierarchical ensembles, and future directions.
