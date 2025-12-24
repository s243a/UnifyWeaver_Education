<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Appendix C: Database Persistence with BoltDB

This appendix provides detailed documentation on BoltDB persistence in the Go target.

## Overview

BoltDB (bbolt) is an embedded key-value store used by generator mode for:
- Persisting derived facts across program runs
- Incremental Datalog computation
- Efficient key-based lookups

**Library:** `go.etcd.io/bbolt v1.3.8`

## Database Schema

### Bucket Organization

Facts are stored in named buckets (one per predicate by default):

```go
// Each predicate gets its own bucket
db.Update(func(tx *bolt.Tx) error {
    _, err := tx.CreateBucketIfNotExists([]byte("ancestor"))
    return err
})
```

### Storage Format

| Component | Format |
|-----------|--------|
| Keys | `[]byte` (string converted) |
| Values | JSON-marshaled `map[string]interface{}` |
| Encoding | UTF-8 strings, JSON objects |

Example stored record:
```json
{"relation":"ancestor","args":{"arg0":"john","arg1":"mary"}}
```

## Key Strategies

### Single Field Key (Default)

```prolog
compile_predicate_to_go(user/2, [
    db_backend(bbolt),
    db_key_field(name)
], Code).
```

Generated key: `"Alice"`, `"Bob"`

### Composite Keys

```prolog
compile_predicate_to_go(employee/3, [
    db_backend(bbolt),
    db_key_strategy(composite([field(dept), field(name)])),
    db_key_delimiter(':')
], Code).
```

Generated keys: `"eng:Alice"`, `"sales:Bob"`

### Hash Keys

```prolog
compile_predicate_to_go(document/2, [
    db_backend(bbolt),
    db_key_strategy(hash(field(content)))
], Code).
```

Generated key: SHA-256 hex digest of content field

### Composite with Hash

```prolog
compile_predicate_to_go(versioned_doc/3, [
    db_backend(bbolt),
    db_key_strategy(composite([field(name), hash(field(content))]))
], Code).
```

Generated key: `"readme:a3f2b1c9..."`

## Secondary Indexes

You can declare secondary indexes to optimize lookups on non-key fields.

```prolog
% Declare index on email field for user/2 predicate
:- index(user/2, email).
```

### Write Path
The compiler automatically generates code to maintain index buckets (e.g., `index_user_email`) alongside the main record bucket.
- **Bucket Name**: `index_Predicate_Field`
- **Key Format**: `Value:PrimaryKey` (allows multiple records with same value)
- **Value**: Empty

### Read Path
When a query filters on an indexed field using equality:

```prolog
user(Id, Email) :-
    json_record([id-Id, email-Email]),
    Email = "alice@example.com".
```

The compiler generates an optimized `cursor.Seek()` on the index bucket to find matching primary keys, then fetches the full records from the main bucket. This avoids a full table scan.

## Write Path

### Populating the Database

```prolog
% Generate a populate program
compile_predicate_to_go(user/3, [
    db_backend(bbolt),
    db_file('users.db'),
    db_key_field(name),
    json_input(true)
], Code).
```

Usage:
```bash
# Pipe JSONL data to populate
echo '{"name":"Alice","age":30,"city":"NYC"}
{"name":"Bob","age":25,"city":"SF"}' | ./populate
```

### Generated Write Code

```go
scanner := bufio.NewScanner(os.Stdin)
for scanner.Scan() {
    var data map[string]interface{}
    json.Unmarshal(scanner.Bytes(), &data)

    // Extract key field
    keyStr := fmt.Sprintf("%v", data["name"])
    key := []byte(keyStr)

    // Store full record
    db.Update(func(tx *bolt.Tx) error {
        bucket := tx.Bucket([]byte("users"))
        value, _ := json.Marshal(data)
        return bucket.Put(key, value)
    })
}
```

## Read Path

### Query Modes

The compiler generates three query strategies:

| Mode | When Used | Complexity |
|------|-----------|------------|
| Direct lookup | Key field in WHERE with equality | O(log n) |
| Prefix scan | Composite key prefix matches | O(k log n) |
| Full scan | No key optimization possible | O(n) |

### Direct Lookup Example

```prolog
% Query: find user where name = "Alice"
user(Name, Age, City) :-
    Name = "Alice".
```

Generated:
```go
db.View(func(tx *bolt.Tx) error {
    bucket := tx.Bucket([]byte("users"))
    value := bucket.Get([]byte("Alice"))
    if value != nil {
        var data map[string]interface{}
        json.Unmarshal(value, &data)
        // Output result
    }
    return nil
})
```

### Prefix Scan Example

```prolog
% Query: find employees in engineering
employee(Dept, Name, Salary) :-
    Dept = "eng".
```

With composite key `[dept, name]`:
```go
cursor := bucket.Cursor()
prefix := []byte("eng:")
for k, v := cursor.Seek(prefix); k != nil && bytes.HasPrefix(k, prefix); k, v = cursor.Next() {
    // Process matching records
}
```

### Full Scan Example

```prolog
% Query: find users over 25 (age not in key)
user(Name, Age, City) :-
    Age > 25.
```

Generated:
```go
bucket.ForEach(func(k, v []byte) error {
    var data map[string]interface{}
    json.Unmarshal(v, &data)
    age := data["age"].(float64)
    if age > 25 {
        // Output result
    }
    return nil
})
```

## Querying Outside Generated Programs

### Option 1: Custom Go Code

```go
package main

import (
    "encoding/json"
    "fmt"
    bolt "go.etcd.io/bbolt"
)

func main() {
    db, _ := bolt.Open("users.db", 0600, &bolt.Options{ReadOnly: true})
    defer db.Close()

    db.View(func(tx *bolt.Tx) error {
        bucket := tx.Bucket([]byte("users"))

        // Direct lookup
        v := bucket.Get([]byte("Alice"))
        if v != nil {
            var data map[string]interface{}
            json.Unmarshal(v, &data)
            fmt.Printf("Found: %v\n", data)
        }

        // Full scan
        bucket.ForEach(func(k, v []byte) error {
            fmt.Printf("Key: %s\n", k)
            return nil
        })

        return nil
    })
}
```

### Option 2: Export to JSON

Generate a query program that outputs all records:

```prolog
compile_predicate_to_go(user/3, [
    db_backend(bbolt),
    db_file('users.db'),
    db_mode(read),
    json_output(true)
], Code).
```

```bash
./query_users > all_users.jsonl
```

### Option 3: Runtime Store Wrapper

Use the UnifyWeaver Go runtime library:

```go
import "unifyweaver/targets/go_runtime/storage"

store, _ := storage.NewStore("users.db")
defer store.Close()

// Get single object
obj, _ := store.GetObject("Alice")
fmt.Println(obj["age"])

// Count records
count, _ := store.CountObjects()
fmt.Printf("Total: %d\n", count)
```

## Incremental Computation

BoltDB enables incremental Datalog:

```bash
# Initial run - compute and persist
./ancestor

# Add new facts - continue from saved state
echo '{"relation":"parent","args":{"arg0":"alice","arg1":"bob"}}' | ./ancestor

# Query persisted results
./query_ancestors
```

The database stores all derived facts, so subsequent runs:
1. Load existing facts from database
2. Add new input facts
3. Run fixpoint until no new derivations
4. Persist new facts

## Configuration Options

| Option | Default | Description |
|--------|---------|-------------|
| `db_backend(bbolt)` | - | Enable BoltDB persistence |
| `db_file(Path)` | `'facts.db'` | Database file path |
| `db_bucket(Name)` | predicate name | Bucket name |
| `db_key_field(F)` | first field | Single field key |
| `db_key_strategy(S)` | `field(F)` | Key generation strategy |
| `db_key_delimiter(D)` | `':'` | Composite key separator |
| `db_mode(read)` | - | Generate read-only query |
| `db_mode(write)` | - | Generate write program |

## Limitations

1. **Single writer**: BoltDB allows only one process to write at a time
2. **No secondary indexes**: Only primary key lookups are O(log n)
3. **Memory-mapped**: Large databases may consume significant virtual memory
4. **No SQL**: Must use Go API or generated programs for queries

## Best Practices

1. **Choose key fields wisely**: Fields frequently used in WHERE clauses
2. **Use composite keys**: For multi-field lookups, order by selectivity
3. **Batch writes**: Collect records and write in single transaction
4. **Close connections**: Always `db.Close()` to flush pages
5. **Read-only mode**: Use `&bolt.Options{ReadOnly: true}` for queries

---

## Navigation

**←** [Appendix B: Complexity Guide](A2_complexity_guide) | [📖 Book 6: Go Target](./)
