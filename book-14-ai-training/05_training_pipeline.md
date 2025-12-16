<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 5: Training Pipeline

**Database Schema and Batch Management**

## The LDA Database

Training data and models are stored in SQLite with NPY files for vectors:

```
playbooks/lda-training-data/
├── lda.db                    # SQLite database
└── embeddings/
    └── all-MiniLM-L6-v2/
        ├── q_001.npy         # Question embeddings
        ├── a_001.npy         # Answer embeddings
        └── mh_1_cluster_0_centroid.npy  # Cluster centroids
```

## Database Schema Overview

```sql
-- Embedding models
CREATE TABLE models (
    model_id INTEGER PRIMARY KEY,
    name TEXT UNIQUE,           -- e.g., 'all-MiniLM-L6-v2'
    dimensions INTEGER,         -- e.g., 384
    created_at TIMESTAMP
);

-- Q-A clusters
CREATE TABLE clusters (
    cluster_id INTEGER PRIMARY KEY,
    answer_text TEXT,
    answer_record_id TEXT,      -- External ID
    model_id INTEGER REFERENCES models
);

-- Questions belonging to clusters
CREATE TABLE questions (
    question_id INTEGER PRIMARY KEY,
    cluster_id INTEGER REFERENCES clusters,
    question_text TEXT
);

-- Vector storage (paths to NPY files)
CREATE TABLE embeddings (
    embedding_id INTEGER PRIMARY KEY,
    entity_type TEXT,           -- 'question', 'answer', 'centroid'
    entity_id INTEGER,
    model_id INTEGER REFERENCES models,
    vector_path TEXT            -- Path to .npy file
);

-- Multi-head projections
CREATE TABLE multi_head_projections (
    mh_projection_id INTEGER PRIMARY KEY,
    model_id INTEGER REFERENCES models,
    num_heads INTEGER,
    temperature REAL,
    created_at TIMESTAMP
);

-- Cluster heads (centroid + answer for each cluster)
CREATE TABLE cluster_heads (
    head_id INTEGER PRIMARY KEY,
    mh_projection_id INTEGER REFERENCES multi_head_projections,
    cluster_id INTEGER REFERENCES clusters,
    centroid_path TEXT,         -- Path to centroid .npy
    answer_emb_path TEXT        -- Path to answer embedding .npy
);
```

## Training Batch Tracking

Track which data files have been processed:

```sql
CREATE TABLE training_batches (
    batch_id INTEGER PRIMARY KEY,
    file_path TEXT,
    file_hash TEXT,             -- SHA256 for change detection
    status TEXT,                -- 'pending', 'importing', 'embedding', 'completed', 'failed'
    error_message TEXT,
    created_at TIMESTAMP,
    updated_at TIMESTAMP
);

CREATE TABLE batch_status_history (
    history_id INTEGER PRIMARY KEY,
    batch_id INTEGER REFERENCES training_batches,
    old_status TEXT,
    new_status TEXT,
    changed_at TIMESTAMP
);
```

## Workflow: Adding New Training Data

### Step 1: Scan for New Files

```bash
python3 scripts/migrate_to_lda_db.py \
    --scan \
    --input playbooks/lda-training-data/raw/
```

This:
- Finds JSON files in the input directory
- Computes SHA256 hashes
- Creates `pending` batch records for new/changed files

### Step 2: Process Pending Batches

```bash
python3 scripts/migrate_to_lda_db.py --process-pending
```

For each pending batch:
1. **importing**: Parse JSON, create cluster and question records
2. **embedding**: Embed questions and answers, save NPY files
3. **completed**: Mark batch done

### Step 3: Train Multi-Head Projection

```bash
python3 scripts/train_multi_head_projection.py \
    --db playbooks/lda-training-data/lda.db \
    --temperature 0.1
```

Creates:
- `multi_head_projections` record
- `cluster_heads` records with centroid and answer embedding paths

## The Python Database API

```python
from lda_database import LDAProjectionDB

db = LDAProjectionDB('playbooks/lda-training-data/lda.db')

# Get or create model
model = db.get_or_create_model('all-MiniLM-L6-v2', dimensions=384)

# Add a cluster
cluster_id = db.add_cluster(
    answer_text="The pandas.read_csv() function...",
    answer_record_id="doc_csv_001",
    model_id=model['model_id']
)

# Add questions to cluster
db.add_question(cluster_id, "How do I read a CSV?")
db.add_question(cluster_id, "Loading CSV in pandas")

# Store embedding
db.store_embedding(
    entity_type='question',
    entity_id=question_id,
    model_id=model['model_id'],
    vector=embedding_array,
    embeddings_dir='playbooks/lda-training-data/embeddings'
)

# Search with projection
results = db.search_with_projection(
    query_embedding=query_emb,
    mh_projection_id=1,
    top_k=5
)

db.close()
```

## Incremental Training

The batch tracking system enables incremental updates:

```bash
# Day 1: Initial training
python3 scripts/migrate_to_lda_db.py --scan --input raw/
python3 scripts/migrate_to_lda_db.py --process-pending
python3 scripts/train_multi_head_projection.py --db lda.db

# Day 2: New data added to raw/
python3 scripts/migrate_to_lda_db.py --scan --input raw/
# Only new files are marked pending

python3 scripts/migrate_to_lda_db.py --process-pending
# Only new batches processed

python3 scripts/train_multi_head_projection.py --db lda.db
# Re-trains with all data (new + old)
```

## Handling Failures

```bash
# List all batches
python3 scripts/migrate_to_lda_db.py --list-batches

# Retry failed batches
python3 scripts/migrate_to_lda_db.py --retry-failed
```

Failed batches retain their error messages for debugging:

```
Batch 3: FAILED
  File: raw/broken_data.json
  Error: JSON parse error at line 45
```

## Unified Model File Convention

Multi-head models use a consistent naming scheme:

```
embeddings/{model_name}/mh_{mh_id}_cluster_{cluster_id}_centroid.npy
embeddings/{model_name}/mh_{mh_id}_cluster_{cluster_id}_answer.npy
```

This enables all runtimes (Python, Go, Rust) to discover and load heads automatically.

## Practical Exercise

Inspect the database:

```bash
sqlite3 playbooks/lda-training-data/lda.db

.tables
SELECT * FROM models;
SELECT * FROM clusters LIMIT 5;
SELECT COUNT(*) FROM questions;
SELECT * FROM multi_head_projections;
```

## Chapter Summary

- SQLite + NPY files provide portable model storage
- Batch tracking enables incremental training with change detection
- The Python API provides high-level operations
- Unified naming conventions enable cross-target deployment
- Failed batches retain error context for debugging

## Next Chapter

[Chapter 6: Transformer Distillation](06_transformer_distillation.md) - Compressing multi-head LDA into compact transformers.
