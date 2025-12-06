<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)
-->

# Chapter 3: Target Registry and Mapping

## Overview

The target registry is the foundation of cross-target glue. It manages:

- **Target metadata** - What targets exist and their capabilities
- **Runtime families** - How targets are grouped
- **Predicate mappings** - Which target compiles each predicate
- **Location resolution** - Where predicates execute

## The Target Registry Module

Located at `src/unifyweaver/core/target_registry.pl`, this module provides:

```prolog
:- module(target_registry, [
    register_target/3,        % Register a new target
    unregister_target/1,      % Remove a target
    target_exists/1,          % Check if target exists
    target_family/2,          % Query target's family
    target_capabilities/2,    % Query capabilities
    targets_same_family/2,    % Check family membership
    family_targets/2,         % List targets in family
    list_targets/1,           % List all targets
    list_families/1,          % List all families
    default_location/2,       % Default location for target
    default_transport/3       % Default transport between locations
]).
```

## Built-in Targets

The registry comes pre-configured with 15+ targets:

### Shell Family

```prolog
target_family(bash, shell).
target_family(awk, shell).
target_family(sed, shell).
target_family(perl, shell).
```

**Characteristics:**
- Always run as separate processes
- Communicate via pipes
- Best for text processing

### Python Family

```prolog
target_family(python, python).
target_family(ironpython, dotnet).  % Note: IronPython is .NET family
```

**Characteristics:**
- CPython runs as separate process
- IronPython can be in-process with .NET

### .NET Family

```prolog
target_family(csharp, dotnet).
target_family(fsharp, dotnet).
target_family(powershell, dotnet).
target_family(ironpython, dotnet).
```

**Characteristics:**
- Can communicate in-process
- Share the CLR runtime
- Zero serialization between them

### JVM Family

```prolog
target_family(java, jvm).
target_family(scala, jvm).
target_family(clojure, jvm).
target_family(jython, jvm).
```

**Characteristics:**
- Can communicate in-process
- Share the JVM runtime

### Native Family

```prolog
target_family(go, native).
target_family(rust, native).
target_family(c, native).
target_family(cpp, native).
```

**Characteristics:**
- Compiled to machine code
- Communicate via pipes or shared memory
- Best for performance-critical code

### Database Family

```prolog
target_family(sql, database).
target_family(sqlite, database).
target_family(postgresql, database).
```

**Characteristics:**
- Execute queries against databases
- Typically accessed via network or file

## Querying the Registry

### Check if a Target Exists

```prolog
?- target_exists(python).
true.

?- target_exists(fortran).
false.
```

### Query Target Family

```prolog
?- target_family(powershell, Family).
Family = dotnet.

?- target_family(Target, shell).
Target = bash ;
Target = awk ;
Target = sed ;
Target = perl.
```

### Check Same Family

```prolog
?- targets_same_family(csharp, powershell).
true.

?- targets_same_family(python, rust).
false.
```

### List Family Members

```prolog
?- family_targets(native, Targets).
Targets = [go, rust, c, cpp].
```

## Registering Custom Targets

You can register your own targets:

```prolog
% Register a custom Rust target with specific capabilities
register_target(myrust, native, [compiled, typed, async, simd]).

% Register a custom Python target
register_target(micropython, python, [embedded, limited_stdlib]).
```

### Capabilities

Capabilities describe what a target can do:

| Capability | Meaning |
|------------|---------|
| `compiled` | Compiles to machine code |
| `typed` | Statically typed |
| `async` | Supports async/await |
| `streaming` | Can process streams |
| `pipes` | Can use pipe I/O |
| `http` | Can make HTTP requests |
| `simd` | Supports SIMD operations |

Query capabilities:

```prolog
?- target_capabilities(go, Caps).
Caps = [compiled, typed, async, streaming, pipes, http].
```

## The Target Mapping Module

Located at `src/unifyweaver/core/target_mapping.pl`:

```prolog
:- module(target_mapping, [
    declare_target/2,          % Basic declaration
    declare_target/3,          % Declaration with options
    undeclare_target/1,        % Remove declaration
    declare_location/2,        % Specify location
    declare_connection/3,      % Specify connection
    predicate_target/2,        % Query mapping
    predicate_location/2,      % Query location
    resolve_location/2,        % Resolve with defaults
    resolve_transport/3,       % Resolve transport
    list_mappings/1,           % List all mappings
    validate_mapping/2         % Validate configuration
]).
```

## Declaring Target Mappings

### Basic Declaration

```prolog
% Declare that filter/2 compiles to AWK
:- declare_target(filter/2, awk).

% Declare that transform/2 compiles to Python
:- declare_target(transform/2, python).
```

### Declaration with Options

```prolog
:- declare_target(analyze/2, python, [
    location(local_process),
    format(json),
    timeout(30)
]).
```

Available options:

| Option | Values | Description |
|--------|--------|-------------|
| `location(L)` | `in_process`, `local_process`, `remote(Host)` | Execution location |
| `transport(T)` | `direct`, `pipe`, `socket`, `http` | Communication method |
| `format(F)` | `tsv`, `json`, `binary` | Data format |
| `timeout(S)` | Integer | Timeout in seconds |

## Declaring Locations

Override where a predicate executes:

```prolog
% Run on a specific remote host
:- declare_location(heavy_compute/2, [
    host('gpu-worker.example.com'),
    port(8080),
    transport(http)
]).

% Force separate process for isolation
:- declare_location(untrusted/2, [
    process(separate),
    sandbox(true)
]).
```

## Declaring Connections

Specify how two predicates communicate:

```prolog
% Use JSON format between these predicates
:- declare_connection(producer/2, consumer/2, [
    format(json),
    buffer(line)
]).

% Use socket for bidirectional communication
:- declare_connection(client/2, server/2, [
    transport(socket),
    format(json)
]).
```

## Resolution Rules

When explicit configuration isn't provided, defaults are computed.

### Location Resolution

```prolog
resolve_location(Pred/Arity, Location) :-
    % 1. Check explicit declaration
    predicate_location(Pred/Arity, Location), !.

resolve_location(Pred/Arity, Location) :-
    % 2. Use target's default
    predicate_target(Pred/Arity, Target),
    default_location(Target, Location).
```

### Default Location Rules

```prolog
% Same family = in-process
default_location(Target, in_process) :-
    target_family(Target, Family),
    member(Family, [dotnet, jvm]).

% Different family = separate process
default_location(Target, local_process) :-
    target_family(Target, Family),
    member(Family, [shell, python, native]).
```

### Transport Resolution

```prolog
resolve_transport(Pred1, Pred2, Transport) :-
    resolve_location(Pred1, Loc1),
    resolve_location(Pred2, Loc2),
    default_transport(Loc1, Loc2, Transport).
```

### Default Transport Rules

```prolog
% In-process = direct calls
default_transport(in_process, in_process, direct).

% Local processes = pipes
default_transport(local_process, local_process, pipe).

% Remote = HTTP
default_transport(local_process, remote(_), http).
default_transport(remote(_), remote(_), http).
```

## Querying Mappings

### Find Target for Predicate

```prolog
?- predicate_target(filter/2, Target).
Target = awk.
```

### Find Location for Predicate

```prolog
?- resolve_location(filter/2, Location).
Location = local_process.
```

### Find Transport Between Predicates

```prolog
?- resolve_transport(filter/2, transform/2, Transport).
Transport = pipe.
```

### List All Mappings

```prolog
?- list_mappings(Mappings).
Mappings = [
    mapping(filter/2, awk, local_process),
    mapping(transform/2, python, local_process),
    mapping(store/2, sql, remote('db.local'))
].
```

## Validation

Validate your configuration:

```prolog
?- validate_mapping(unknown_pred/2, Errors).
Errors = [no_target_declared].

?- validate_mapping(filter/2, Errors).
Errors = [].  % No errors
```

Common validation errors:

| Error | Meaning |
|-------|---------|
| `no_target_declared` | Predicate has no target mapping |
| `unknown_target` | Target doesn't exist in registry |
| `invalid_location` | Location specification is invalid |
| `incompatible_transport` | Transport not valid for locations |

## Practical Example

Let's configure a complete pipeline:

```prolog
% file: pipeline_config.pl

:- use_module('src/unifyweaver/core/target_registry').
:- use_module('src/unifyweaver/core/target_mapping').

% Declare targets for each predicate
:- declare_target(fetch_logs/2, bash).
:- declare_target(parse_logs/2, awk).
:- declare_target(analyze_logs/2, python, [format(json)]).
:- declare_target(generate_report/2, python).
:- declare_target(store_report/2, sql).

% Configure remote database
:- declare_location(store_report/2, [
    host('db.production.local'),
    port(5432)
]).

% Specify JSON between parse and analyze
:- declare_connection(parse_logs/2, analyze_logs/2, [
    format(json)
]).
```

Query the configuration:

```prolog
?- resolve_location(fetch_logs/2, L).
L = local_process.

?- resolve_location(store_report/2, L).
L = remote('db.production.local').

?- resolve_transport(parse_logs/2, analyze_logs/2, T).
T = pipe.
```

## Chapter Summary

- **Target Registry** manages target metadata and families
- **Target Mapping** connects predicates to targets
- **Location** specifies where predicates run
- **Transport** specifies how they communicate
- **Defaults** are computed from family membership
- **Validation** catches configuration errors early

## Next Steps

In Chapter 4, we'll explore the pipe protocols in detail:
- TSV format specification
- JSON Lines format
- Header negotiation
- Field mapping between targets

## Exercises

1. **Registry query**: Write a query to find all targets that support `async`.

2. **Custom target**: Register a custom `nodejs` target in the `native` family with capabilities `[interpreted, async, http]`.

3. **Configuration**: Configure a pipeline where:
   - `input/1` is Bash
   - `process/2` is Go (with parallel(4))
   - `output/1` is remote SQL at 'db.local:5432'

4. **Resolution**: Given `csharp` calling `powershell`, trace through the resolution rules to determine location and transport.

## API Reference

See `docs/design/cross-target-glue/04-api-reference.md` for complete predicate documentation.
