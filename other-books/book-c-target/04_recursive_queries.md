# Chapter 4: Recursive Queries

This chapter covers compiling recursive predicates to C.

## Compiling to C

```prolog
?- compile_recursive(ancestor/2, [target(c)], Code).
```

## Generated C Code

The C target generates a complete BFS implementation with:

```c
#define MAX_NODES 1000

/* Simple hash table for adjacency list */
typedef struct Edge {
    char* to;
    struct Edge* next;
} Edge;

typedef struct {
    char* from;
    Edge* edges;
} Node;

static Node nodes[MAX_NODES];
static int node_count = 0;

/* BFS to find all reachable nodes */
static void find_all(const char* start) {
    char* queue[MAX_NODES];
    int visited[MAX_NODES] = {0};
    int head = 0, tail = 0;
    
    // ... BFS implementation
}

/* BFS to check if target is reachable */
static int check_path(const char* start, const char* target) {
    // ... BFS with early exit
}
```

## Running

```bash
gcc -o ancestor ancestor.c
echo 'abraham:isaac' | ./ancestor abraham
```

## C-Specific Features

- Manual memory management with `strdup`/`malloc`
- Static arrays for BFS queue and visited set
- Linked list for adjacency edges
- `strcmp` for string comparison
