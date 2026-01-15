<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 5: Data Binding

## Overview

The data binding generator creates reactive connections between Prolog data sources and UI components. Define data sources and bindings declaratively, then generate React hooks, data providers, and WebSocket synchronization code.

## Loading the Module

```prolog
:- use_module('src/unifyweaver/glue/data_binding_generator').
```

## Data Sources

### Defining a Data Source

```prolog
data_source(Name, Options)
```

**Options:**
- `predicate(Pred/Arity)` - Prolog predicate providing data
- `fields(List)` - Field names
- `primary_key(Field)` - Primary key field
- `refresh_interval(Ms)` - Polling interval (0 = no polling)
- `cache(Bool)` - Enable caching
- `sort_by(Field, Order)` - Sort order (`asc` or `desc`)

**Example:**

```prolog
data_source(sales_data, [
    predicate(sales_record/4),
    fields([date, product, quantity, amount]),
    primary_key(date),
    refresh_interval(5000),
    cache(true)
]).
```

### Built-in Data Sources

| Source | Description |
|--------|-------------|
| `default` | Simple key-value records |
| `time_series` | Timestamped data points |
| `graph_data` | Nodes and edges |
| `hierarchical` | Tree-structured data |

### Graph Data Source

For network visualizations:

```prolog
data_source(my_graph, [
    predicate(node/2),
    edge_predicate(edge/3),
    node_fields([id, label, properties]),
    edge_fields([source, target, properties]),
    primary_key(id),
    cache(true)
]).
```

### Hierarchical Data Source

For tree views and org charts:

```prolog
data_source(org_chart, [
    predicate(employee/3),
    fields([id, parent_id, name]),
    primary_key(id),
    parent_field(parent_id),
    cache(true)
]).
```

## Computed Sources

Derive data from other sources with transformations:

```prolog
computed_source(Name, Options)
```

### Aggregation

```prolog
computed_source(sales_summary, [
    base_source(sales_data),
    computation(aggregate),
    group_by([product]),
    aggregations([
        sum(amount, total_sales),
        avg(amount, average_sale),
        count(_, sale_count)
    ]),
    cache(true),
    cache_ttl(60000)
]).
```

### Filtering

```prolog
computed_source(positive_sales, [
    base_source(sales_data),
    computation(filter),
    filter_predicate(amount > 0),
    cache(false)
]).
```

### Joining

```prolog
computed_source(sales_with_products, [
    sources([products, sales]),
    computation(join),
    join_on(product_id),
    join_type(inner),
    cache(true)
]).
```

## Bindings

### One-Way Bindings

Connect a data source to a component:

```prolog
binding(Component, Source, Mapping)
```

**Mapping Options:**
- `x_axis(Field)` - X-axis field (for charts)
- `y_axis(Field)` - Y-axis field
- `series(Field)` - Series/group field
- `color(Field)` - Color mapping
- `size(Field)` - Size mapping
- `transform(Type)` - Data transformation

**Chart Examples:**

```prolog
% Line chart
binding(revenue_chart, time_series, [
    x_axis(timestamp),
    y_axis(value),
    series(category),
    transform(none)
]).

% Bar chart
binding(category_bars, aggregated, [
    x_axis(category),
    y_axis(total),
    color(category),
    transform(none)
]).

% Scatter plot
binding(correlation_plot, time_series, [
    x_axis(x_value),
    y_axis(y_value),
    size(constant(5)),
    color(group),
    transform(none)
]).
```

**Graph Binding:**

```prolog
binding(network_viz, graph_data, [
    nodes(node_predicate),
    edges(edge_predicate),
    node_id(id),
    node_label(name),
    edge_source(from),
    edge_target(to),
    transform(none)
]).
```

**Tree Binding:**

```prolog
binding(org_treemap, hierarchical, [
    id(id),
    parent(parent_id),
    label(name),
    value(headcount),
    transform(none)
]).
```

### Two-Way Bindings

For editable components with data sync back to source:

```prolog
two_way_binding(Component, Source, Mapping)
```

**Additional Options:**
- `editable(Fields)` - Editable fields
- `on_edit(Handler)` - Edit callback
- `on_change(Handler)` - Change callback
- `validation(Rules)` - Validation rules
- `debounce(Ms)` - Debounce delay

**Data Table Example:**

```prolog
two_way_binding(editable_table, sales_data, [
    columns([date, product, quantity, amount]),
    editable([quantity, amount]),
    on_edit(update_record),
    validation([
        field(quantity, number),
        field(quantity, min(0)),
        field(amount, number),
        field(amount, min(0))
    ])
]).
```

**Form Example:**

```prolog
two_way_binding(user_form, user_data, [
    fields([name, email, role]),
    editable([name, email, role]),
    on_change(update_field),
    debounce(300)
]).
```

## Code Generation

### Generate React Hook

```prolog
?- generate_binding_hook(revenue_chart, Hook).
```

**Generated Code:**

```typescript
import { useState, useEffect, useCallback } from "react";

interface TimeSeriesData {
  timestamp: string;
  value: number;
  category: string;
}

interface UseRevenueChartResult {
  data: TimeSeriesData[];
  loading: boolean;
  error: Error | null;
  refetch: () => Promise<void>;
}

export const useRevenueChart = (): UseRevenueChartResult => {
  const [data, setData] = useState<TimeSeriesData[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<Error | null>(null);

  const fetchData = useCallback(async () => {
    setLoading(true);
    setError(null);
    try {
      const response = await fetch("/api/data/time_series");
      if (!response.ok) throw new Error("Failed to fetch data");
      const rawData = await response.json();
      setData(rawData);
    } catch (err) {
      setError(err instanceof Error ? err : new Error("Unknown error"));
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    fetchData();
  }, [fetchData]);

  return { data, loading, error, refetch: fetchData };
};
```

### Polling Hook

For data sources with `refresh_interval > 0`:

```prolog
data_source(live_metrics, [
    predicate(metric/3),
    fields([name, value, timestamp]),
    refresh_interval(5000),
    cache(true)
]).

?- generate_binding_hook(metrics_chart, Hook).
```

The generated hook includes:
- `isPolling` - Current polling state
- `startPolling()` - Start polling
- `stopPolling()` - Stop polling

### Generate Data Provider

```prolog
?- generate_data_provider(sales_data, Provider).
```

Generates a React Context provider for sharing data across components.

### Generate WebSocket Sync

```prolog
?- generate_websocket_sync(live_metrics, SyncCode).
```

Generates real-time synchronization code using WebSockets.

### Generate Mutation Handler

```prolog
?- generate_mutation_handler(sales_data, Handler).
```

Generates code for updating data back to the server.

### Generate Types

```prolog
?- generate_binding_types(sales_data, Types).
```

Generates TypeScript interfaces for the data source.

### Generate Context

```prolog
?- generate_binding_context(sales_data, Context).
```

Generates React Context for dependency injection.

## Utility Predicates

```prolog
% Get fields for a source
?- get_source_fields(sales_data, Fields).
% Fields = [date, product, quantity, amount]

% Get mapping for a component
?- get_binding_mapping(revenue_chart, Mapping).

% Check if binding is two-way
?- is_two_way(editable_table).

% Infer field type
?- infer_field_type(amount, Type).
% Type = number
```

## Management

```prolog
% Add data source dynamically
declare_data_source(Name, Options)

% Add binding dynamically
declare_binding(Component, Source, Mapping)

% Clear all bindings
clear_bindings
```

## Complete Example

```prolog
:- use_module('src/unifyweaver/glue/data_binding_generator').

% Define product sales data
:- declare_data_source(product_sales, [
    predicate(sale/4),
    fields([date, product, units, revenue]),
    primary_key(date),
    refresh_interval(10000),
    cache(true)
]).

% Aggregated view
:- computed_source(product_totals, [
    base_source(product_sales),
    computation(aggregate),
    group_by([product]),
    aggregations([
        sum(revenue, total_revenue),
        sum(units, total_units)
    ])
]).

% Chart binding
:- declare_binding(sales_chart, product_sales, [
    x_axis(date),
    y_axis(revenue),
    series(product)
]).

% Summary binding
:- declare_binding(totals_bar, product_totals, [
    x_axis(product),
    y_axis(total_revenue),
    color(product)
]).

% Editable table
:- two_way_binding(sales_table, product_sales, [
    columns([date, product, units, revenue]),
    editable([units, revenue]),
    on_edit(update_sale),
    validation([
        field(units, number),
        field(units, min(0)),
        field(revenue, number)
    ])
]).

% Generate all bindings
generate_sales_bindings :-
    generate_binding_hook(sales_chart, ChartHook),
    generate_binding_hook(totals_bar, BarHook),
    generate_binding_hook(sales_table, TableHook),
    format('// Sales Chart Hook~n~w~n~n', [ChartHook]),
    format('// Totals Bar Hook~n~w~n~n', [BarHook]),
    format('// Sales Table Hook~n~w~n', [TableHook]).
```

---

**Previous**: [Chapter 4: Layout System](04_layout_system.md) | **Next**: [Chapter 6: Accessibility](06_accessibility.md)
