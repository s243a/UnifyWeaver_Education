# Chapter 17: Fuzzy Logic DSL

This chapter covers UnifyWeaver's Prolog-based fuzzy logic domain-specific language (DSL) for probabilistic scoring in semantic search.

## Motivation

Embedding models don't always recognize domain-specific terms (e.g., "bash-reduce"). Users need to boost or filter results using fuzzy logic operations that go beyond simple boolean matching.

## Core Concepts

### Weighted Terms

The canonical representation uses explicit `w/2` functors:

```prolog
% w(Term, Weight) - a term with its weight in [0.0, 1.0]
w(bash, 0.9)    % High weight
w(shell, 0.5)   % Medium weight
w(bash, 1.0)    % Full weight (or just: bash)
```

### Fuzzy Operations

Each operation has symbolic and evaluation forms:

| Operation | Formula | Use Case |
|-----------|---------|----------|
| f_and | w₁×t₁ × w₂×t₂ × ... | Require multiple terms |
| f_or | 1 - (1-w₁×t₁)(1-w₂×t₂)... | Match any alternative |
| f_dist_or(S) | 1 - (1-S×w₁×t₁)(1-S×w₂×t₂)... | Blend base into terms |
| f_union(S) | S × f_or([...]) | Scale combined OR |
| f_not | 1 - score | Complement |

## Quick Start

```prolog
% Load the fuzzy logic DSL
:- use_module('src/unifyweaver/fuzzy/fuzzy').

% Evaluate fuzzy AND with term scores
?- eval_fuzzy_expr(
       f_and([w(bash, 0.9), w(shell, 0.5)]),
       [bash-0.8, shell-0.6],
       Result
   ).
% Result = 0.216  (0.9×0.8 × 0.5×0.6)

% Evaluate fuzzy OR
?- eval_fuzzy_expr(
       f_or([w(bash, 0.9), w(shell, 0.5)]),
       [bash-0.8, shell-0.6],
       Result
   ).
% Result = 0.804  (1 - (1-0.72)(1-0.3))
```

## Operator Syntax (Optional)

Load the operators module for concise syntax:

```prolog
:- use_module('src/unifyweaver/fuzzy/operators').

% & for AND, v for OR
?- fuzzy_and(bash:0.9 & shell:0.5, Result).
?- fuzzy_or(bash:0.9 v shell:0.5, Result).

% Expands to:
% f_and([w(bash, 0.9), w(shell, 0.5)], Result)
```

## Distributed vs Non-Distributed OR

The key difference between `f_dist_or` and `f_union`:

```
f_or([a, b])        →  a + b - ab
f_union(S, [a,b])   →  S×(a + b - ab) = Sa + Sb - Sab
f_dist_or(S, [a,b]) →  1-(1-Sa)(1-Sb) = Sa + Sb - S²ab
```

The interaction term differs: `Sab` (union) vs `S²ab` (dist_or).

- **f_or**: Combine term scores without a base
- **f_union**: Multiplicatively scale combined OR by base
- **f_dist_or**: Blend base into each term before OR

## Hierarchical Filters

Filter by tree structure:

```prolog
child_of(Node)        % Direct children only
descendant_of(Node)   % Any depth below
parent_of(Node)       % Immediate parent
ancestor_of(Node)     % Any depth above
sibling_of(Node)      % Same parent
has_depth(N)          % Exactly at depth N
depth_between(Min, Max)
near(Node, Decay)     % Score decays with distance
```

## Boolean Metadata Filters

```prolog
is_type(tree)              % Match item type
has_account(s243a)         % Match account
has_parent(bash)           % Match parent folder
in_subtree("Unix/Linux")   % Path contains
has_tag(linux)             % Match tag
```

## Evaluation Pipeline

Combine fuzzy scoring with filtering:

```prolog
boost_query(Query, Result) :-
    semantic_search(Query, S0),
    f_dist_or(S0, [w(bash, 0.9), w(shell, 0.5)], S1),
    apply_filter(S1, descendant_of("Unix"), S2),
    top_k(S2, 10, Result).
```

## Module Structure

```
src/unifyweaver/fuzzy/
  fuzzy.pl         % Main module (re-exports all)
  core.pl          % f_and, f_or, f_dist_or, f_union, f_not
  boolean.pl       % b_and, b_or, b_not
  predicates.pl    % Metadata and hierarchical filters
  operators.pl     % Optional & and v syntax
  eval.pl          % Evaluation engine
```

## Mathematical Foundation

### Product T-Norm (AND)

The fuzzy AND uses product t-norm, which satisfies:
- Commutativity: A ∧ B = B ∧ A
- Associativity: (A ∧ B) ∧ C = A ∧ (B ∧ C)
- Monotonicity: If A ≤ A', then A ∧ B ≤ A' ∧ B

### Probabilistic Sum (OR)

The fuzzy OR uses probabilistic sum:
```
A ∨ B = A + B - A×B
```

This is equivalent to De Morgan's law with product t-norm:
```
A ∨ B = 1 - (1-A)(1-B)
```

### Non-Distributivity

Unlike Boolean logic, fuzzy AND does not distribute over OR:
```
S × (A ∨ B) ≠ (S×A) ∨ (S×B)

Left side:  S×(A + B - AB) = SA + SB - SAB
Right side: SA + SB - S²AB
```

This is why we have both `f_union` (left) and `f_dist_or` (right).

## Connection to Functional Analysis

Fuzzy membership functions can be viewed as elements of the dual space V* → [0,1], mapping embedding vectors to scores. The weighted term `w(term, weight)` acts as a linear functional scaled by its weight.

## Next Steps

- Chapter 18 will cover compilation of fuzzy expressions to Python/NumPy
- Chapter 19 will cover SQL target generation for database queries
