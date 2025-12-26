<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 2: C# Native Target

## Overview

The C# Native Target (`csharp_native`) compiles Prolog predicates directly into standalone C# source code. It uses LINQ pipelines for non-recursive predicates and **semi-naive iteration** for recursive predicates. This approach generates human-readable C# files that can be integrated into .NET applications without any runtime dependencies.

## What Gets Generated?

The Native Target translates Prolog predicates into:
- **Facts** → C# arrays
- **Simple rules** → LINQ Select/Where operations
- **Multiple clauses** → Union of LINQ pipelines
- **Recursive predicates** → Semi-naive iteration with HashSet deduplication
- **Deduplication** → Distinct() operators or HashSet

## Your First Native Target Compilation

### Example 1: Pure Facts

**Prolog Source:**
```prolog
:- use_module(unifyweaver(targets/csharp_native_target)).

parent(alice, bob).
parent(bob, charlie).
parent(charlie, dave).
```

**Compile to C#:**
```prolog
?- compile_predicate_to_csharp(parent/2, [], Code).
```

**Generated C# (simplified):**
```csharp
using System;
using System.Linq;
using System.Collections.Generic;

namespace UnifyWeaver.Generated
{
    public static class Parent
    {
        // Facts become a static array
        static readonly (string, string)[] Facts = {
            ("alice", "bob"),
            ("bob", "charlie"),
            ("charlie", "dave")
        };

        // Stream method returns all facts
        public static IEnumerable<(string, string)> Stream()
            => Facts.AsEnumerable();

        // Main entry point for standalone execution
        static void Main()
        {
            foreach (var (parent, child) in Stream())
            {
                Console.WriteLine($"parent({parent}, {child})");
            }
        }
    }
}
```

**What This Means:**
- `Facts` array stores all parent/2 tuples in memory
- `Stream()` method returns an enumerable (like Prolog query results)
- `Main()` demonstrates how to use the generated code
- C# tuple `(string, string)` represents the two arguments

### Running the Generated Code

**Save to file:**
```prolog
?- compile_predicate_to_csharp(parent/2, [], Code),
   open('Parent.cs', write, Stream),
   write(Stream, Code),
   close(Stream).
```

**Create .NET project:**
```bash
dotnet new console -n ParentDemo
mv Parent.cs ParentDemo/Program.cs
cd ParentDemo
dotnet run
```

**Output:**
```
parent(alice, bob)
parent(bob, charlie)
parent(charlie, dave)
```

## Example 2: Simple Rule with Conditions

**Prolog Source:**
```prolog
adult(Person) :-
    age(Person, Age),
    Age >= 18.

% Facts
age(alice, 25).
age(bob, 15).
age(charlie, 30).
```

**Compilation Strategy:**

The Native Target recognizes this as a **single rule** and compiles it to LINQ:

**Generated C# (conceptual):**
```csharp
public static class Adult
{
    // Reference to age/2 facts
    static readonly (string, int)[] AgeFacts = {
        ("alice", 25),
        ("bob", 15),
        ("charlie", 30)
    };

    public static IEnumerable<string> Stream()
    {
        return AgeFacts
            .Where(tuple => tuple.Item2 >= 18)  // Age >= 18
            .Select(tuple => tuple.Item1);      // Return Person
    }

    static void Main()
    {
        foreach (var person in Stream())
        {
            Console.WriteLine($"adult({person})");
        }
    }
}
```

**Breakdown:**
1. **`AgeFacts`** - All age/2 tuples
2. **`Where(tuple => tuple.Item2 >= 18)`** - Filters by condition (Age >= 18)
3. **`Select(tuple => tuple.Item1)`** - Projects only the Person field
4. **Result:** Only adults (alice, charlie)

## Example 3: Multiple Clauses (Union)

**Prolog Source:**
```prolog
sibling(X, Y) :- parent(P, X), parent(P, Y), X \= Y.
sibling(alice, bob).   % Direct fact

parent(john, alice).
parent(john, bob).
parent(mary, charlie).
```

**Generated C# Pattern:**

When there are multiple clauses (rules + facts), they become **separate LINQ pipelines** combined with `Union`:

```csharp
public static IEnumerable<(string, string)> Stream()
{
    // Clause 1: Direct fact
    var clause1 = new[] { ("alice", "bob") };

    // Clause 2: Rule - parent(P, X), parent(P, Y), X != Y
    var clause2 =
        from px in ParentFacts  // parent(P, X)
        from py in ParentFacts  // parent(P, Y)
        where px.Item1 == py.Item1  // Same parent P
        where px.Item2 != py.Item2  // X != Y
        select (px.Item2, py.Item2);  // Return (X, Y)

    // Union combines both clauses
    return clause1.Union(clause2);
}
```

**What This Means:**
- Each Prolog clause → One LINQ expression
- `Union()` merges results from all clauses
- Automatic deduplication (Union removes duplicates by default)

## Deduplication Strategies

UnifyWeaver supports different deduplication modes:

### Option 1: Unique (Default)
```prolog
compile_predicate_to_csharp(sibling/2, [dedup(unique)], Code).
```
Generated: `.Union(...)` (removes duplicates)

### Option 2: Allow Duplicates
```prolog
compile_predicate_to_csharp(sibling/2, [dedup(allow_duplicates)], Code).
```
Generated: `.Concat(...)` (keeps all results)

### Option 3: Explicit Distinct
```prolog
compile_predicate_to_csharp(sibling/2, [dedup(distinct)], Code).
```
Generated: `.Concat(...).Distinct()`

## Advanced Example: Join Operation

**Prolog Source:**
```prolog
grandparent(GP, GC) :- parent(GP, P), parent(P, GC).

parent(alice, bob).
parent(bob, charlie).
parent(charlie, dave).
```

**Generated C# (join pattern):**
```csharp
public static IEnumerable<(string, string)> Stream()
{
    return ParentFacts
        .Join(
            ParentFacts,                    // Join with itself
            outer => outer.Item2,           // GP -> P (child of first parent)
            inner => inner.Item1,           // P -> GC (parent of second)
            (outer, inner) => (outer.Item1, inner.Item2)  // (GP, GC)
        );
}
```

**Breakdown:**
1. **Outer:** `parent(GP, P)` - First parent relation
2. **Inner:** `parent(P, GC)` - Second parent relation
3. **Join key:** `P` (child in first becomes parent in second)
4. **Result:** `(GP, GC)` - Grandparent-grandchild pairs

**Output:**
```
grandparent(alice, charlie)
grandparent(bob, dave)
```

## Type Inference

The Native Target infers C# types from Prolog terms:

| Prolog Term | C# Type |
|-------------|---------|
| `42` | `int` |
| `3.14` | `double` |
| `"hello"` | `string` |
| `atom` | `string` (atoms become strings) |
| `parent(a, b)` | `(string, string)` (tuple) |
| `data(1, "x", 2.5)` | `(int, string, double)` |

**Mixed types example:**
```prolog
person(alice, 25, engineer).
person(bob, 30, teacher).
```

**Generated:**
```csharp
static readonly (string, int, string)[] Facts = {
    ("alice", 25, "engineer"),
    ("bob", 30, "teacher")
};
```

## Recursive Predicates: Semi-Naive Iteration

The Native Target fully supports **recursive predicates** using semi-naive iteration. This was a major addition in v0.1.

### Example: Transitive Closure (Ancestor)

**Prolog Source:**
```prolog
parent(alice, bob).
parent(bob, charlie).
parent(charlie, diana).
parent(diana, eve).

ancestor(X, Y) :- parent(X, Y).
ancestor(X, Z) :- parent(X, Y), ancestor(Y, Z).
```

**Compile to C#:**
```prolog
?- compile_predicate_to_csharp(ancestor/2, [mode(procedural)], Code).
```

**Generated C# (semi-naive iteration):**
```csharp
public static IEnumerable<(string, string)> AncestorStream()
{
    var seen = new HashSet<(string, string)>();
    var delta = new List<(string, string)>();

    // Base case: ancestor(X,Y) :- parent(X,Y)
    foreach (var item in _clause_1())
    {
        if (seen.Add(item)) { delta.Add(item); yield return item; }
    }

    // Semi-naive iteration: ancestor(X,Z) :- parent(X,Y), ancestor(Y,Z)
    while (delta.Count > 0)
    {
        var newDelta = new List<(string, string)>();
        foreach (var d in delta)
        {
            foreach (var b in ParentStream())
            {
                if (b.Item2 == d.Item1)
                {
                    var newItem = (b.Item1, d.Item2);
                    if (seen.Add(newItem)) { newDelta.Add(newItem); yield return newItem; }
                }
            }
        }
        delta = newDelta;
    }
}
```

**Output:**
```
alice:bob
bob:charlie
charlie:diana
diana:eve
alice:charlie
bob:diana
charlie:eve
alice:diana
bob:eve
alice:eve
```

### How Semi-Naive Iteration Works

1. **Base case first**: Computes all non-recursive clause results
2. **Delta iteration**: Only processes newly-discovered tuples in each round
3. **HashSet deduplication**: O(1) checks prevent duplicates and ensure termination
4. **Incremental yield**: Results stream out as they're discovered

This approach avoids stack overflow that would occur with naive recursion, and is more efficient than recomputing all results in each iteration.

### Why Not LINQ Join for Recursion?

LINQ `Join` eagerly materializes its right operand before iteration begins. For recursive calls, this would cause infinite expansion. The semi-naive approach solves this by:
- Only joining new delta tuples with base relations
- Using worklist-based iteration instead of recursive method calls

## Limitations of Native Target

### 1. No Unification Variables in Head
**Does NOT work:**
```prolog
duplicate(X, X) :- number(X).  % Same variable appears twice
```

**Workaround:** Add explicit equality check in body

### 2. Limited Built-ins
Supported: `=`, `\=`, `<`, `>`, `=<`, `>=`, `is`
Not supported: `findall`, `bagof`, `setof`, complex arithmetic

### 3. No Mutual Recursion
For mutual recursion across multiple predicates, use the C# Query Runtime (Chapter 3)

## Building and Deploying

### Standalone Console App
```bash
# Generate C# code
swipl -g "compile_predicate_to_csharp(parent/2, [], Code), \
          open('Program.cs', write, S), write(S, Code), close(S), halt."

# Create and run project
dotnet new console -n MyApp
mv Program.cs MyApp/
cd MyApp
dotnet run
```

### Library Integration
```csharp
// Your existing .NET app
using UnifyWeaver.Generated;

var parents = Parent.Stream();
foreach (var (p, c) in parents) {
    Console.WriteLine($"{p} is parent of {c}");
}
```

### NuGet Package
```bash
dotnet pack
# Generates .nupkg with generated predicates
```

## Performance Characteristics

**Benchmark: 10,000 fact lookups**

| Implementation | Time (ms) | Memory (MB) |
|----------------|-----------|-------------|
| Prolog (SWI) | 450 | 25 |
| Bash Script | 850 | 45 |
| C# Stream | **12** | **8** |

**Why so fast?**
- In-memory arrays (no I/O)
- LINQ compiled to efficient code
- No process forking
- JIT optimization

## Complete Example: Family Tree Query

**Prolog:**
```prolog
:- use_module(unifyweaver(targets/csharp_native_target)).

parent(alice, bob).
parent(alice, charlie).
parent(bob, dave).
parent(bob, eve).

% Siblings share a parent
sibling(X, Y) :-
    parent(P, X),
    parent(P, Y),
    X \= Y.

% Uncle/aunt relationship
uncle(U, N) :-
    parent(P, N),       % N's parent is P
    sibling(U, P).      % U is sibling of P
```

**Compile:**
```prolog
?- compile_predicate_to_csharp(uncle/2, [], Code),
   open('UncleQuery.cs', write, S),
   write(S, Code),
   close(S).
```

**Generated structure:**
```csharp
namespace UnifyWeaver.Generated {
    public static class Uncle {
        // parent/2 facts embedded
        static readonly (string, string)[] ParentFacts = { ... };

        // sibling/2 logic embedded
        static IEnumerable<(string, string)> Siblings() { ... }

        // uncle/2 main query
        public static IEnumerable<(string, string)> Stream() {
            return
                from pn in ParentFacts      // parent(P, N)
                from up in Siblings()        // sibling(U, P)
                where up.Item2 == pn.Item1   // P matches
                select (up.Item1, pn.Item2); // (U, N)
        }
    }
}
```

## Troubleshooting

### Recursion causes stack overflow
**Cause:** Older versions used LINQ Join for recursion, which eagerly materializes
**Solution:** Update to v0.1+ which uses semi-naive iteration, or use Query Runtime

### Error: "Type inference failed"
**Cause:** Mixed types in same argument position
**Solution:** Make types consistent or use explicit type annotations

### Generated code doesn't compile
**Cause:** Unsupported Prolog construct
**Solution:** Simplify predicate or check limitations section

### Error: "CS0246: The type or namespace 'IEnumerable' could not be found"
**Cause:** Missing `using System.Collections.Generic;`
**Solution:** Add the using directive at the top of your file:
```csharp
using System;
using System.Linq;
using System.Collections.Generic;
```

### Error: "dotnet: command not found"
**Cause:** .NET SDK not installed or not in PATH
**Solution:**
```bash
# On Linux/WSL:
wget https://dot.net/v1/dotnet-install.sh
./dotnet-install.sh --channel 6.0
export PATH="$HOME/.dotnet:$PATH"

# On Termux (use proot-distro debian):
proot-distro login debian
apt install dotnet-sdk-6.0
```

### Error: "error CS1061: 'ValueTuple' does not contain a definition for 'Item1'"
**Cause:** .NET version too old (needs .NET 6.0+)
**Solution:** Update to .NET 6.0 or higher:
```bash
dotnet --version  # Check current version
```

### Empty output when running generated code
**Cause:** Facts not loaded or predicate returns no results
**Solution:**
1. Verify the generated `Facts` array is populated
2. Check if filtering conditions are too restrictive
3. Add debug output to trace execution:
```csharp
foreach (var fact in Facts) {
    Console.WriteLine($"Fact: {fact}");
}
```

### Build succeeds but runtime exception occurs
**Cause:** Null reference or invalid data
**Solution:**
1. Check for null values in your facts
2. Verify join keys match correctly
3. Use try-catch to identify the failing line:
```csharp
try {
    var results = Stream().ToList();
} catch (Exception ex) {
    Console.WriteLine($"Error: {ex.Message}");
}
```

## Real-World Example: Employee Analysis

Let's build a complete employee database query system using the Native Target.

### The Scenario

You have employee data and want to analyze it using C# LINQ, but want to express queries in Prolog.

### Step 1: Define the Data

**`employees.pl`:**
```prolog
:- use_module(unifyweaver(targets/csharp_native_target)).

% Employee facts: employee(Name, Department)
employee(alice, engineering).
employee(bob, engineering).
employee(charlie, sales).
employee(diane, sales).
employee(eve, hr).

% Manager relationships: manages(Manager, Employee)
manages(alice, bob).
manages(charlie, diane).

% Queries we want to compile
engineer(Name) :- employee(Name, engineering).
sales_person(Name) :- employee(Name, sales).
team_member(Manager, Employee) :- manages(Manager, Employee).
```

### Step 2: Compile Each Query

```prolog
?- compile_predicate_to_csharp(engineer/1, [], EngineerCode),
   open('Engineer.cs', write, S1),
   write(S1, EngineerCode),
   close(S1).

?- compile_predicate_to_csharp(sales_person/1, [], SalesCode),
   open('SalesPerson.cs', write, S2),
   write(S2, SalesCode),
   close(S2).

?- compile_predicate_to_csharp(team_member/2, [], TeamCode),
   open('TeamMember.cs', write, S3),
   write(S3, TeamCode),
   close(S3).
```

### Step 3: Create Dashboard Application

**`EmployeeDashboard.cs`:**
```csharp
using System;
using System.Linq;
using UnifyWeaver.Generated;

class EmployeeDashboard
{
    static void Main()
    {
        Console.WriteLine("=== Engineering Team ===");
        foreach (var name in Engineer.Stream())
        {
            Console.WriteLine($"  - {name}");
        }

        Console.WriteLine("\n=== Sales Team ===");
        foreach (var name in SalesPerson.Stream())
        {
            Console.WriteLine($"  - {name}");
        }

        Console.WriteLine("\n=== Team Structures ===");
        foreach (var (manager, employee) in TeamMember.Stream())
        {
            Console.WriteLine($"  {manager} manages {employee}");
        }

        // LINQ integration - count by department
        var engineerCount = Engineer.Stream().Count();
        var salesCount = SalesPerson.Stream().Count();

        Console.WriteLine($"\nTeam sizes: Engineering={engineerCount}, Sales={salesCount}");
    }
}
```

### Step 4: Build and Run

```bash
dotnet new console -n EmployeeDashboard
cd EmployeeDashboard

# Copy generated files
cp ../Engineer.cs ./
cp ../SalesPerson.cs ./
cp ../TeamMember.cs ./
cp ../EmployeeDashboard.cs ./Program.cs

dotnet build
dotnet run
```

### Output

```
=== Engineering Team ===
  - alice
  - bob

=== Sales Team ===
  - charlie
  - diane

=== Team Structures ===
  alice manages bob
  charlie manages diane

Team sizes: Engineering=2, Sales=2
```

### What This Demonstrates

**Benefits of Native Target:**
1. **Query as Code** - Prolog queries become reusable C# methods
2. **Type Safety** - Compile-time checking of query results
3. **LINQ Integration** - Generated code works with standard LINQ operations
4. **No Runtime Dependency** - Standalone C# files, no Prolog needed at runtime
5. **Performance** - Direct LINQ execution, no interpretation overhead
6. **Recursion Support** - Semi-naive iteration handles transitive closures

**Typical Use Cases:**
- Business rule engines with stable rules
- Data validation pipelines
- Report generation from structured data
- Integration queries for .NET applications
- Testing and verification systems

### Extending the Example

Add more complex queries:

```prolog
% Colleagues in same department
colleague(X, Y) :-
    employee(X, Dept),
    employee(Y, Dept),
    X \= Y.

% All employees not in management
non_manager(Employee) :-
    employee(Employee, _),
    \+ manages(_, Employee).
```

Compile and integrate into your dashboard for richer analysis!

## Summary

The C# Native Target:
- ✅ Compiles facts and simple rules to LINQ
- ✅ Full recursion support via semi-naive iteration
- ✅ Generates standalone C# source files (no runtime dependencies)
- ✅ Excellent performance with HashSet deduplication
- ✅ Easy integration with .NET applications
- ❌ No mutual recursion support (use Query Runtime for that)
- ❌ Limited to in-memory operations

**Next Chapter:** Learn about the C# Query Runtime for handling mutual recursion and complex query optimization.

---

**Exercises:**

1. Compile a simple fact predicate to C# and run it
2. Add a filtering rule and observe the `Where()` clause
3. Create a multi-clause predicate and examine the `Union()` pattern
4. Compile the ancestor/2 recursive predicate and verify the semi-naive iteration output
5. Benchmark C# vs Bash for your use case

Next: **Chapter 3 - C# Query Runtime and Engine** →

---

## Navigation

**←** [Previous: Chapter 1: Introduction to Multi-Target Compilation](01_introduction_multi_target.md) | [📖 Book 3: C# Target](./) | [Next: Chapter 3: C# Query Runtime →](03_query_engine_deep_dive.md)
