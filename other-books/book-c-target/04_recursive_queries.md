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

## Advanced Recursion Patterns

The C target also supports tail and linear recursion via multifile dispatch:

```prolog
?- compile_tail_recursion(test_sum/3, [target(c)], Code).
?- compile_linear_recursion(factorial/2, [target(c)], Code).
```

| Pattern | Multifile Predicate | C Idiom |
|---------|-------------------|---------|
| Tail Recursion | `tail_recursion:compile_tail_pattern/9` | `for` loop with accumulator |
| Linear Recursion | `linear_recursion:compile_linear_pattern/8` | Static array memoization (`#define MAX_MEMO 10000`) |

### Tail Recursion Example

```c
int test_sum(int* items, int count) {
    int acc = 0;
    for (int i = 0; i < count; i++) {
        acc = acc + items[i];
    }
    return acc;
}
```

### Linear Recursion Example

```c
#define MAX_MEMO 10000
static int memo[MAX_MEMO];
static int memo_valid[MAX_MEMO];

int factorial(int n) {
    if (n >= 0 && n < MAX_MEMO && memo_valid[n]) return memo[n];
    if (n == 0) return 1;
    int result = 1;
    for (int current = n; current >= 1; current--) {
        result = current * result;
    }
    if (n >= 0 && n < MAX_MEMO) { memo[n] = result; memo_valid[n] = 1; }
    return result;
}
```

C uses static arrays for memoization since it lacks built-in hash maps. The `MAX_MEMO` bound limits cache size.
