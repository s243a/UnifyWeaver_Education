<!--
SPDX-License-Identifier: MIT AND CC-BY-4.0
Copyright (c) 2025 John William Creighton (s243a)

This documentation is dual-licensed under MIT and CC-BY-4.0.
-->

# Chapter 24: Composability Patterns

**Building Complex UIs from Declarative Primitives**

This chapter demonstrates the composability and generalizability of the UnifyWeaver visualization system. While previous chapters covered individual generators, this chapter shows how to compose them into complete applications.

## Overview

The visualization glue system is built on **composition over inheritance**. Complex UIs emerge from combining simple, independent layers:

```
┌─────────────────────────────────────────────────────────────────┐
│                    Application Layer                             │
│   Templates + Dashboards + Pages                                │
├─────────────────────────────────────────────────────────────────┤
│                    Interaction Layer                             │
│   Controls + Animations + Tooltips + Drill-down                 │
├─────────────────────────────────────────────────────────────────┤
│                    Layout Layer                                  │
│   Grid/Flex + Responsive Breakpoints + Regions                  │
├─────────────────────────────────────────────────────────────────┤
│                    Visualization Layer                           │
│   Charts + Graphs + Heatmaps + Gauges + Maps                    │
├─────────────────────────────────────────────────────────────────┤
│                    Styling Layer                                 │
│   Themes + CSS Variables + Typography + Spacing                 │
├─────────────────────────────────────────────────────────────────┤
│                    Data Layer                                    │
│   Prolog Facts + Data Binding + WebSocket Sync                  │
└─────────────────────────────────────────────────────────────────┘
```

Each layer is independent and can be used alone or combined with others.

## End-to-End Workflow

This section walks through a complete workflow from raw data to deployed dashboard.

### Step 1: Define Your Data

Start by defining your data as Prolog facts:

```prolog
% sales_data.pl - Raw sales data
:- module(sales_data, [
    sales_record/4,      % sales_record(Date, Product, Quantity, Revenue)
    product_info/3       % product_info(Product, Category, Color)
]).

% Monthly sales records
sales_record('2024-01', laptop, 150, 225000).
sales_record('2024-01', phone, 320, 192000).
sales_record('2024-01', tablet, 89, 44500).
sales_record('2024-02', laptop, 180, 270000).
sales_record('2024-02', phone, 290, 174000).
sales_record('2024-02', tablet, 110, 55000).
sales_record('2024-03', laptop, 210, 315000).
sales_record('2024-03', phone, 350, 210000).
sales_record('2024-03', tablet, 95, 47500).

% Product metadata
product_info(laptop, electronics, '#3b82f6').
product_info(phone, electronics, '#10b981').
product_info(tablet, electronics, '#f59e0b').
```

### Step 2: Create Visualization Specifications

Transform your data into visualization specs:

```prolog
% sales_dashboard_spec.pl - Dashboard specification
:- module(sales_dashboard_spec, [
    build_dashboard/0
]).

:- use_module(sales_data).
:- use_module('../src/unifyweaver/glue/curve_plot_generator').
:- use_module('../src/unifyweaver/glue/gauge_chart_generator').
:- use_module('../src/unifyweaver/glue/funnel_chart_generator').
:- use_module('../src/unifyweaver/glue/layout_generator').
:- use_module('../src/unifyweaver/glue/theme_generator').

% Build revenue trend curve from data
build_revenue_curve :-
    % Aggregate revenue by month
    findall(Month-Total, (
        findall(Rev, sales_record(Month, _, _, Rev), Revs),
        sum_list(Revs, Total)
    ), MonthlyRevenue),

    % Create curve specification
    declare_curve_spec(revenue_trend, [
        title("Monthly Revenue"),
        x_label("Month"),
        y_label("Revenue ($)"),
        theme(dark)
    ]),

    % Add data points
    forall(
        nth1(Index, MonthlyRevenue, _Month-Revenue),
        declare_curve_point(revenue_trend, Index, Revenue)
    ).

% Build product breakdown gauge
build_product_gauges :-
    % Calculate total revenue
    findall(Rev, sales_record(_, _, _, Rev), AllRevs),
    sum_list(AllRevs, TotalRevenue),

    % Calculate target (e.g., 2M annual = 500K quarterly)
    Target = 500000,
    Percentage is (TotalRevenue / Target) * 100,

    gauge_chart_generator:declare_gauge_spec(revenue_gauge, [
        title("Q1 Revenue Target"),
        min(0),
        max(100),
        value(Percentage),
        unit("%"),
        thresholds([
            threshold(behind, 0, 70, '#ef4444'),
            threshold(on_track, 70, 90, '#f59e0b'),
            threshold(ahead, 90, 100, '#22c55e')
        ]),
        size(180)
    ]).

% Build sales funnel from conversion data
build_sales_funnel :-
    funnel_chart_generator:declare_funnel_spec(sales_funnel, [
        title("Sales Pipeline"),
        width(300),
        height(250),
        color_scheme(blues)
    ]),

    % Define funnel stages (example conversion data)
    funnel_chart_generator:declare_funnel_stage(sales_funnel, visitors,
        [label("Website Visitors"), value(10000), order(1)]),
    funnel_chart_generator:declare_funnel_stage(sales_funnel, leads,
        [label("Leads"), value(2500), order(2)]),
    funnel_chart_generator:declare_funnel_stage(sales_funnel, qualified,
        [label("Qualified"), value(800), order(3)]),
    funnel_chart_generator:declare_funnel_stage(sales_funnel, customers,
        [label("Customers"), value(250), order(4)]).

% Main build function
build_dashboard :-
    build_revenue_curve,
    build_product_gauges,
    build_sales_funnel.
```

### Step 3: Define Layout and Composition

Compose visualizations into a layout:

```prolog
% dashboard_layout.pl - Layout composition
:- module(dashboard_layout, [
    generate_dashboard/1
]).

:- use_module('../src/unifyweaver/glue/layout_generator').
:- use_module('../src/unifyweaver/glue/theme_generator').
:- use_module('../src/unifyweaver/glue/curve_plot_generator').
:- use_module('../src/unifyweaver/glue/gauge_chart_generator').
:- use_module('../src/unifyweaver/glue/funnel_chart_generator').

% Define the dashboard layout
setup_layout :-
    layout_generator:declare_layout(sales_dashboard, grid, [
        areas([
            ["header",  "header",  "header"],
            ["sidebar", "main",    "main"],
            ["sidebar", "charts",  "charts"]
        ]),
        columns(["280px", "1fr", "1fr"]),
        rows(["auto", "1fr", "1fr"]),
        gap("1rem")
    ]),

    % Place components in regions
    layout_generator:declare_placement(sales_dashboard, header, [title_bar, theme_toggle]),
    layout_generator:declare_placement(sales_dashboard, sidebar, [revenue_gauge, sales_funnel]),
    layout_generator:declare_placement(sales_dashboard, main, [revenue_trend]),
    layout_generator:declare_placement(sales_dashboard, charts, [product_breakdown]).

% Generate complete dashboard code
generate_dashboard(Code) :-
    setup_layout,

    % Generate individual components
    curve_plot_generator:generate_curve_component(revenue_trend, CurveCode),
    gauge_chart_generator:generate_gauge_component(revenue_gauge, GaugeCode),
    funnel_chart_generator:generate_funnel_component(sales_funnel, FunnelCode),

    % Generate layout wrapper
    layout_generator:generate_layout_jsx(sales_dashboard, LayoutCode),
    layout_generator:generate_layout_css(sales_dashboard, LayoutCSS),

    % Generate theme
    theme_generator:generate_theme_css(dark, ThemeCSS),
    theme_generator:generate_theme_toggle([light, dark], ToggleCode),

    % Combine all code
    format(atom(Code), '
// === THEME ===
~w

// === THEME TOGGLE ===
~w

// === REVENUE TREND CHART ===
~w

// === REVENUE GAUGE ===
~w

// === SALES FUNNEL ===
~w

// === LAYOUT ===
~w

// === CSS ===
/*
~w

~w
*/
', [ThemeCSS, ToggleCode, CurveCode, GaugeCode, FunnelCode, LayoutCode, LayoutCSS, ThemeCSS]).
```

### Step 4: Add Interactivity

Layer controls and interactions on top:

```prolog
% dashboard_controls.pl - Add interactivity
:- module(dashboard_controls, [
    generate_interactive_dashboard/1
]).

:- use_module('../src/unifyweaver/glue/layout_generator').
:- use_module('../src/unifyweaver/glue/interaction_generator').
:- use_module('../src/unifyweaver/glue/animation_presets').

% Define controls for the dashboard
setup_controls :-
    % Date range selector
    layout_generator:declare_control(date_range, select, [
        options(['Q1 2024', 'Q2 2024', 'Q3 2024', 'Q4 2024', 'Full Year']),
        default('Q1 2024'),
        label("Time Period")
    ]),

    % Product filter
    layout_generator:declare_control(product_filter, select, [
        options([all, laptop, phone, tablet]),
        default(all),
        label("Product")
    ]),

    % Refresh interval
    layout_generator:declare_control(refresh_interval, slider, [
        min(5), max(60), step(5),
        default(30),
        label("Refresh (seconds)")
    ]),

    % Group controls into panel
    layout_generator:declare_control_panel(dashboard_controls,
        [date_range, product_filter, refresh_interval]).

% Add animations
setup_animations :-
    % Chart entry animations
    animation_presets:apply_preset(revenue_trend, fade_in_up),
    animation_presets:apply_preset(revenue_gauge, scale_in),
    animation_presets:apply_preset(sales_funnel, fade_in),

    % Data update animations
    animation_presets:apply_preset(data_point, data_point_pop).

% Add interactions
setup_interactions :-
    % Tooltip on hover
    interaction_generator:declare_interaction(revenue_trend, tooltip, [
        trigger(hover),
        content(data_point),
        format("${value:,.0f}")
    ]),

    % Drill-down on click
    interaction_generator:declare_interaction(sales_funnel, drill_down, [
        trigger(click),
        target(stage_detail),
        animation(slide_in_right)
    ]),

    % Pan and zoom on chart
    interaction_generator:declare_interaction(revenue_trend, pan_zoom, [
        enable_pan(true),
        enable_zoom(true),
        zoom_extent([0.5, 10])
    ]).

% Generate complete interactive dashboard
generate_interactive_dashboard(Code) :-
    setup_controls,
    setup_animations,
    setup_interactions,

    % Generate wired component with controls connected to visualizations
    layout_generator:generate_wired_component(sales_dashboard, [
        panel(dashboard_controls),
        components([revenue_trend, revenue_gauge, sales_funnel]),
        layout(sales_dashboard)
    ], Code).
```

### Step 5: Export and Deploy

Generate export capabilities and deployment artifacts:

```prolog
% dashboard_export.pl - Export and deployment
:- module(dashboard_export, [
    generate_exportable_dashboard/1,
    generate_storybook_story/1
]).

:- use_module('../src/unifyweaver/glue/export_generator').

% Add export controls
setup_exports :-
    export_generator:declare_export_config(sales_dashboard, [
        formats([png, pdf, csv]),
        filename_template("sales-dashboard-{date}"),
        default_size(1200, 800)
    ]).

% Generate dashboard with export capabilities
generate_exportable_dashboard(Code) :-
    setup_exports,

    % Generate export hook and controls
    export_generator:generate_export_hook(sales_dashboard, ExportHook),
    export_generator:generate_export_menu(sales_dashboard, ExportMenu),

    % Generate main dashboard
    generate_interactive_dashboard(DashboardCode),

    format(atom(Code), '
~w

~w

// Dashboard with export
export const SalesDashboardWithExport: React.FC = () => {
  const chartRef = useRef<HTMLDivElement>(null);
  const exportControls = useExport(chartRef);

  return (
    <div ref={chartRef}>
      <SalesDashboard />
      ~w
    </div>
  );
};
', [ExportHook, DashboardCode, ExportMenu]).

% Generate Storybook story for documentation
generate_storybook_story(StoryCode) :-
    format(atom(StoryCode), '
import type { Meta, StoryObj } from "@storybook/react";
import { SalesDashboardWithExport } from "./SalesDashboard";

const meta: Meta<typeof SalesDashboardWithExport> = {
  title: "Dashboards/Sales Dashboard",
  component: SalesDashboardWithExport,
  tags: ["autodocs"],
  parameters: {
    layout: "fullscreen",
    docs: {
      description: {
        component: `
Complete sales dashboard generated from UnifyWeaver Prolog specifications.

**Data Sources:**
- Revenue trend from \\`sales_record/4\\` facts
- Gauge target from quarterly goals
- Funnel from conversion tracking

**Composition Layers:**
1. Data layer: Prolog facts
2. Visualization layer: Curve, Gauge, Funnel charts
3. Layout layer: CSS Grid with sidebar
4. Interaction layer: Tooltips, drill-down, pan/zoom
5. Export layer: PNG, PDF, CSV
        `,
      },
    },
  },
};

export default meta;
type Story = StoryObj<typeof SalesDashboardWithExport>;

export const Default: Story = {};

export const DarkTheme: Story = {
  decorators: [(Story) => (
    <div data-theme="dark" style={{ minHeight: "100vh" }}>
      <Story />
    </div>
  )],
};

export const LightTheme: Story = {
  decorators: [(Story) => (
    <div data-theme="light" style={{ minHeight: "100vh" }}>
      <Story />
    </div>
  )],
};
', []).
```

## Multi-Level Composition Patterns

### Pattern 1: Nested Layouts

Layouts can contain other layouts for complex arrangements:

```prolog
% Outer layout: page structure
layout(page, grid, [
    areas([["nav"], ["content"], ["footer"]]),
    rows(["60px", "1fr", "40px"])
]).

% Inner layout: content area with sidebar
layout(content_area, grid, [
    areas([["sidebar", "main"]]),
    columns(["300px", "1fr"])
]).

% Innermost layout: main area with charts grid
layout(charts_grid, grid, [
    areas([
        ["chart1", "chart2"],
        ["chart3", "chart4"]
    ]),
    columns(["1fr", "1fr"]),
    rows(["1fr", "1fr"]),
    gap("1rem")
]).

% Compose them together
place(page, content, [content_area]).
place(content_area, main, [charts_grid]).
place(charts_grid, chart1, [revenue_chart]).
place(charts_grid, chart2, [users_chart]).
place(charts_grid, chart3, [conversion_chart]).
place(charts_grid, chart4, [performance_chart]).
```

### Pattern 2: Parameterized Templates

Create reusable templates with parameters:

```prolog
% Define a parameterized KPI card template
kpi_card_template(Name, Title, Value, Trend, Color) :-
    declare_gauge_spec(Name, [
        title(Title),
        value(Value),
        min(0), max(100),
        size(150),
        thresholds([
            threshold(low, 0, 33, '#ef4444'),
            threshold(medium, 33, 66, '#f59e0b'),
            threshold(high, 66, 100, '#22c55e')
        ])
    ]),

    % Add trend indicator
    (Trend > 0
    ->  TrendIcon = '↑', TrendColor = '#22c55e'
    ;   TrendIcon = '↓', TrendColor = '#ef4444'
    ),

    declare_annotation(Name, trend, [
        text(TrendIcon),
        color(TrendColor),
        position(bottom_right)
    ]).

% Use the template to create multiple KPIs
build_kpi_dashboard :-
    kpi_card_template(revenue_kpi, "Revenue", 78, 5, '#3b82f6'),
    kpi_card_template(users_kpi, "Active Users", 92, 12, '#10b981'),
    kpi_card_template(conversion_kpi, "Conversion", 45, -3, '#f59e0b'),
    kpi_card_template(satisfaction_kpi, "Satisfaction", 88, 2, '#8b5cf6'),

    % Arrange in a grid
    declare_layout(kpi_grid, grid, [
        columns(["1fr", "1fr", "1fr", "1fr"]),
        gap("1rem")
    ]),

    place(kpi_grid, pos(1,1), [revenue_kpi]),
    place(kpi_grid, pos(1,2), [users_kpi]),
    place(kpi_grid, pos(1,3), [conversion_kpi]),
    place(kpi_grid, pos(1,4), [satisfaction_kpi]).
```

### Pattern 3: Conditional Composition

Compose differently based on data or context:

```prolog
% Choose visualization based on data characteristics
auto_visualize(DataName, ChartSpec) :-
    % Get data characteristics
    data_point_count(DataName, Count),
    data_dimensions(DataName, Dims),
    data_type(DataName, Type),

    % Choose appropriate visualization
    (Count > 1000, Dims =:= 2
    ->  % Large 2D data: use heatmap
        ChartSpec = heatmap(DataName, [color_scale(viridis)])

    ; Count > 100, Type = time_series
    ->  % Time series: use line chart with virtual scroll
        ChartSpec = curve(DataName, [virtual_scroll(true), lazy_load(true)])

    ; Dims =:= 3
    ->  % 3D data: use 3D scatter
        ChartSpec = scatter3d(DataName, [interactive(true)])

    ; Type = categorical
    ->  % Categorical: use bar or pie
        (Count > 10
        ->  ChartSpec = bar(DataName, [orientation(horizontal)])
        ;   ChartSpec = pie(DataName, [])
        )

    ;   % Default: simple line chart
        ChartSpec = curve(DataName, [])
    ).

% Build dashboard with automatic visualization selection
build_adaptive_dashboard(DataSources, DashboardCode) :-
    findall(ChartSpec, (
        member(DataName, DataSources),
        auto_visualize(DataName, ChartSpec)
    ), ChartSpecs),

    % Generate layout based on number of charts
    length(ChartSpecs, NumCharts),
    auto_layout(NumCharts, LayoutSpec),

    % Generate code
    generate_dashboard_from_specs(ChartSpecs, LayoutSpec, DashboardCode).

% Auto-generate layout based on chart count
auto_layout(1, single).
auto_layout(2, split_horizontal).
auto_layout(N, grid(Rows, Cols)) :-
    N > 2,
    Cols is ceiling(sqrt(N)),
    Rows is ceiling(N / Cols).
```

### Pattern 4: Theme Composition

Compose themes through inheritance and overrides:

```prolog
% Base brand theme
theme(brand_base, [
    colors([
        primary('#1e40af'),
        secondary('#7c3aed'),
        accent('#f59e0b')
    ]),
    typography([
        font_family("Inter, system-ui, sans-serif"),
        font_size_base("16px")
    ]),
    spacing([
        unit("0.25rem"),
        scale([1, 2, 3, 4, 6, 8, 12, 16, 24])
    ])
]).

% Light variant - extends base
theme(brand_light, [
    extends(brand_base),
    colors([
        background('#ffffff'),
        surface('#f8fafc'),
        text_primary('#1e293b'),
        text_secondary('#64748b'),
        border('#e2e8f0')
    ])
]).

% Dark variant - extends base
theme(brand_dark, [
    extends(brand_base),
    colors([
        background('#0f172a'),
        surface('#1e293b'),
        text_primary('#f1f5f9'),
        text_secondary('#94a3b8'),
        border('#334155')
    ])
]).

% High contrast variant - extends dark with overrides
theme(brand_high_contrast, [
    extends(brand_dark),
    colors([
        text_primary('#ffffff'),
        text_secondary('#e2e8f0'),
        border('#ffffff')
    ]),
    accessibility([
        min_contrast_ratio(7.0),
        focus_ring_width("3px")
    ])
]).

% Generate all theme variants
generate_theme_system(Code) :-
    generate_theme_css(brand_light, LightCSS),
    generate_theme_css(brand_dark, DarkCSS),
    generate_theme_css(brand_high_contrast, HighContrastCSS),
    generate_theme_provider([brand_light, brand_dark, brand_high_contrast], ProviderCode),

    format(atom(Code), '~w~n~n~w~n~n~w~n~n~w',
           [LightCSS, DarkCSS, HighContrastCSS, ProviderCode]).
```

## Extension Patterns

### Adding Custom Curve Types

Extend the curve system with domain-specific curves:

```prolog
% custom_curves.pl - Domain-specific curve extensions
:- module(custom_curves, [
    stock_curve/2,
    ecg_curve/2,
    audio_waveform/2
]).

:- use_module('../src/unifyweaver/glue/curve_plot_generator').

% Stock price curve with candlestick option
stock_curve(Name, Options) :-
    member(data(OHLC), Options),  % Open, High, Low, Close data
    member(style(Style), Options),

    (Style = candlestick
    ->  generate_candlestick(Name, OHLC, Code)
    ;   Style = line
    ->  % Use standard curve with close prices
        extract_close_prices(OHLC, ClosePrices),
        declare_curve_spec(Name, [
            type(line),
            data(ClosePrices),
            color('#22c55e')
        ])
    ;   % Area chart showing range
        declare_curve_spec(Name, [
            type(area),
            fill_between(low, high),
            color('#3b82f6'),
            fill_opacity(0.3)
        ])
    ).

% ECG/medical waveform with baseline and annotations
ecg_curve(Name, Options) :-
    member(samples(Samples), Options),
    member(sample_rate(Rate), Options),

    % Calculate time axis
    length(Samples, NumSamples),
    Duration is NumSamples / Rate,

    declare_curve_spec(Name, [
        title("ECG"),
        data(Samples),
        x_range(0, Duration),
        y_range(-2, 2),
        color('#ef4444'),
        line_width(1),

        % Medical-specific options
        grid(true),
        grid_color('#ffcccc'),
        baseline(0),
        baseline_color('#666'),

        % Annotations for peaks
        annotations(auto_detect_peaks)
    ]).

% Audio waveform with zoom and playback position
audio_waveform(Name, Options) :-
    member(samples(Samples), Options),
    member(sample_rate(Rate), Options),

    declare_curve_spec(Name, [
        title("Waveform"),
        data(Samples),
        color('#3b82f6'),
        fill(true),
        fill_color('#3b82f620'),

        % Audio-specific options
        show_rms(true),
        rms_color('#f59e0b'),

        % Playback integration
        playback_position(true),
        position_color('#ef4444'),

        % Virtual scroll for long audio
        virtual_scroll(true),
        overscan(1000)
    ]).
```

### Adding Custom Templates

Create domain-specific dashboard templates:

```prolog
% custom_templates.pl - Domain-specific templates
:- module(custom_templates, [
    ecommerce_dashboard/2,
    devops_dashboard/2,
    iot_dashboard/2
]).

:- use_module('../src/unifyweaver/glue/template_library').

% E-commerce dashboard template
ecommerce_dashboard(Name, Options) :-
    (member(title(Title), Options) -> true ; Title = "E-commerce Dashboard"),

    declare_template(Name, dashboard, [
        title(Title),
        layout(grid),
        regions([
            region(header, [row(1), col_span(4)]),
            region(kpis, [row(2), col_span(4)]),
            region(revenue, [row(3), col_span(2)]),
            region(orders, [row(3), col_span(2)]),
            region(products, [row(4), col_span(2)]),
            region(customers, [row(4), col_span(2)])
        ]),

        % Pre-configured widgets
        widgets([
            widget(header, title_bar, [text(Title)]),
            widget(kpis, kpi_row, [
                metrics([revenue, orders, customers, conversion])
            ]),
            widget(revenue, curve_chart, [
                data_source(daily_revenue),
                chart_type(area)
            ]),
            widget(orders, bar_chart, [
                data_source(orders_by_category),
                orientation(horizontal)
            ]),
            widget(products, table, [
                data_source(top_products),
                columns([name, sales, revenue])
            ]),
            widget(customers, pie_chart, [
                data_source(customer_segments)
            ])
        ]),

        % E-commerce specific controls
        controls([
            date_range_picker,
            category_filter,
            compare_toggle
        ])
    ]).

% DevOps monitoring dashboard
devops_dashboard(Name, Options) :-
    (member(title(Title), Options) -> true ; Title = "DevOps Dashboard"),

    declare_template(Name, dashboard, [
        title(Title),
        layout(grid),
        regions([
            region(alerts, [row(1), col_span(4)]),
            region(services, [row(2), col_span(2)]),
            region(metrics, [row(2), col_span(2)]),
            region(logs, [row(3), col_span(3)]),
            region(deploys, [row(3), col_span(1)])
        ]),

        widgets([
            widget(alerts, alert_banner, [
                severity_colors(true),
                auto_refresh(5000)
            ]),
            widget(services, service_grid, [
                data_source(service_health),
                show_status(true)
            ]),
            widget(metrics, gauge_grid, [
                metrics([cpu, memory, disk, network])
            ]),
            widget(logs, log_viewer, [
                data_source(application_logs),
                virtual_scroll(true),
                syntax_highlight(true)
            ]),
            widget(deploys, timeline, [
                data_source(recent_deploys)
            ])
        ]),

        % Real-time updates
        refresh([
            interval(5000),
            websocket(true)
        ])
    ]).

% IoT sensor dashboard
iot_dashboard(Name, Options) :-
    (member(title(Title), Options) -> true ; Title = "IoT Dashboard"),
    (member(sensors(Sensors), Options) -> true ; Sensors = []),

    % Generate gauge for each sensor
    findall(widget(SensorId, gauge, [data_source(SensorId)]),
            member(SensorId, Sensors),
            SensorWidgets),

    declare_template(Name, dashboard, [
        title(Title),
        layout(responsive_grid),

        % Auto-arrange sensors
        widgets(SensorWidgets),

        % IoT-specific features
        features([
            real_time(true),
            alert_thresholds(true),
            historical_comparison(true),
            export_csv(true)
        ]),

        % Responsive breakpoints
        responsive([
            breakpoint(mobile, columns(1)),
            breakpoint(tablet, columns(2)),
            breakpoint(desktop, columns(4))
        ])
    ]).
```

### Adding Custom Animation Presets

Extend the animation library with custom presets:

```prolog
% custom_animations.pl - Domain-specific animations
:- module(custom_animations, [
    chart_animation/2,
    notification_animation/2,
    loading_animation/2
]).

:- use_module('../src/unifyweaver/glue/animation_presets').

% Chart-specific reveal animation
chart_animation(bar_cascade, Props) :-
    Props = [
        keyframes([
            frame(0, [transform('scaleY(0)'), transform_origin('bottom')]),
            frame(100, [transform('scaleY(1)')])
        ]),
        duration('0.5s'),
        timing('ease-out'),
        stagger('0.05s'),  % Each bar delayed
        direction(left_to_right)
    ].

chart_animation(pie_spin_reveal, Props) :-
    Props = [
        keyframes([
            frame(0, [clip_path('polygon(50% 50%, 50% 0%, 50% 0%)')]),
            frame(100, [clip_path('polygon(50% 50%, 50% 0%, 100% 0%, 100% 100%, 0% 100%, 0% 0%, 50% 0%)')])
        ]),
        duration('1s'),
        timing('ease-in-out')
    ].

chart_animation(line_draw, Props) :-
    Props = [
        keyframes([
            frame(0, [stroke_dashoffset('1000')]),
            frame(100, [stroke_dashoffset('0')])
        ]),
        duration('1.5s'),
        timing('ease-in-out'),
        requires([stroke_dasharray('1000')])
    ].

% Notification animations
notification_animation(slide_in_toast, Props) :-
    Props = [
        keyframes([
            frame(0, [transform('translateX(100%)'), opacity(0)]),
            frame(10, [transform('translateX(0)'), opacity(1)]),
            frame(90, [transform('translateX(0)'), opacity(1)]),
            frame(100, [transform('translateX(100%)'), opacity(0)])
        ]),
        duration('4s'),
        timing('ease-in-out')
    ].

notification_animation(bounce_alert, Props) :-
    Props = [
        keyframes([
            frame(0, [transform('scale(0)')]),
            frame(50, [transform('scale(1.2)')]),
            frame(70, [transform('scale(0.9)')]),
            frame(100, [transform('scale(1)')])
        ]),
        duration('0.5s'),
        timing('ease-out')
    ].

% Loading state animations
loading_animation(skeleton_pulse, Props) :-
    Props = [
        keyframes([
            frame(0, [background_position('-200px 0']),
            frame(100, [background_position('calc(200px + 100%) 0'])
        ]),
        duration('1.5s'),
        timing('linear'),
        iteration('infinite'),
        requires([
            background('linear-gradient(90deg, #f0f0f0 25%, #e0e0e0 50%, #f0f0f0 75%)'),
            background_size('200px 100%')
        ])
    ].

loading_animation(spinner, Props) :-
    Props = [
        keyframes([
            frame(0, [transform('rotate(0deg)')]),
            frame(100, [transform('rotate(360deg)')])
        ]),
        duration('1s'),
        timing('linear'),
        iteration('infinite')
    ].
```

## Introspection and Validation

### Querying the System

Discover what's available in the system:

```prolog
% introspection.pl - Query the visualization system
:- module(introspection, [
    list_visualizations/1,
    list_layouts/1,
    list_themes/1,
    list_animations/1,
    describe_visualization/2,
    compatible_layouts/2
]).

% List all available visualization types
list_visualizations(Types) :-
    Types = [
        curve, graph, heatmap, treemap, plot3d,
        radar, funnel, gauge, sankey, chord
    ].

% List all defined layouts
list_layouts(Layouts) :-
    findall(Name, layout(Name, _, _), Layouts).

% List all defined themes
list_themes(Themes) :-
    findall(Name, theme(Name, _), Themes).

% List all animation presets
list_animations(Animations) :-
    findall(Name, animation_preset(Name, _, _), Animations).

% Describe a visualization's capabilities
describe_visualization(Type, Description) :-
    visualization_description(Type, Description).

visualization_description(curve, [
    name("Curve/Line Chart"),
    inputs([x_data, y_data]),
    supports([
        multiple_series,
        area_fill,
        annotations,
        pan_zoom,
        tooltips
    ]),
    targets([react, matplotlib, plotly])
]).

visualization_description(gauge, [
    name("Gauge/Meter"),
    inputs([value, min, max]),
    supports([
        thresholds,
        animations,
        needle,
        arc_styles
    ]),
    targets([react, plotly])
]).

% Find layouts compatible with a visualization
compatible_layouts(VisualizationType, Layouts) :-
    findall(Layout, (
        layout(Layout, _, Options),
        \+ member(exclude(VisualizationType), Options)
    ), Layouts).
```

### Spec Validation

Validate specifications before generation:

```prolog
% validation.pl - Validate specs before generation
:- module(validation, [
    validate_curve_spec/2,
    validate_layout_spec/2,
    validate_dashboard_spec/2
]).

% Validate curve specification
validate_curve_spec(Name, Errors) :-
    (curve_spec(Name, Config)
    ->  validate_curve_config(Config, Errors)
    ;   Errors = [error(not_found, "Curve spec '~w' not found", [Name])]
    ).

validate_curve_config(Config, Errors) :-
    findall(Error, validate_curve_option(Config, Error), Errors).

validate_curve_option(Config, error(missing_data, "Curve must have data or expr")) :-
    \+ member(data(_), Config),
    \+ member(expr(_), Config),
    \+ member(type(_), Config).

validate_curve_option(Config, error(invalid_range, "x_range must be [Min, Max] with Min < Max")) :-
    member(x_range(Min, Max), Config),
    Min >= Max.

validate_curve_option(Config, error(invalid_color, "Invalid color format: ~w", [Color])) :-
    member(color(Color), Config),
    \+ valid_color(Color).

valid_color(Color) :-
    atom(Color),
    atom_codes(Color, [0'#|Rest]),
    length(Rest, Len),
    member(Len, [3, 6, 8]).

% Validate layout specification
validate_layout_spec(Name, Errors) :-
    (layout(Name, Strategy, Options)
    ->  validate_layout_config(Strategy, Options, Errors)
    ;   Errors = [error(not_found, "Layout '~w' not found", [Name])]
    ).

validate_layout_config(grid, Options, Errors) :-
    findall(Error, validate_grid_option(Options, Error), Errors).

validate_layout_config(flex, Options, Errors) :-
    findall(Error, validate_flex_option(Options, Error), Errors).

validate_grid_option(Options, error(missing_areas, "Grid layout requires 'areas' or 'columns'")) :-
    \+ member(areas(_), Options),
    \+ member(columns(_), Options).

validate_grid_option(Options, error(mismatched_areas, "Grid areas rows must have same column count")) :-
    member(areas(Rows), Options),
    findall(Len, (member(Row, Rows), length(Row, Len)), Lengths),
    \+ all_same(Lengths).

all_same([]).
all_same([_]).
all_same([X,X|Rest]) :- all_same([X|Rest]).

% Validate complete dashboard
validate_dashboard_spec(Name, Errors) :-
    findall(Error, validate_dashboard_component(Name, Error), Errors).

validate_dashboard_component(Name, Error) :-
    dashboard_uses_layout(Name, LayoutName),
    validate_layout_spec(LayoutName, LayoutErrors),
    LayoutErrors \= [],
    member(Error, LayoutErrors).

validate_dashboard_component(Name, Error) :-
    dashboard_uses_visualization(Name, VizName),
    validate_visualization_exists(VizName, Error).

validate_visualization_exists(VizName, error(missing_viz, "Visualization '~w' not defined", [VizName])) :-
    \+ curve_spec(VizName, _),
    \+ graph_spec(VizName, _),
    \+ heatmap_spec(VizName, _),
    \+ gauge_spec(VizName, _).
```

## Best Practices

### 1. Separate Data from Presentation

```prolog
% GOOD: Data module is independent
:- module(my_data, [metric/3]).
metric(revenue, '2024-01', 150000).
metric(revenue, '2024-02', 180000).

% Presentation module imports data
:- module(my_dashboard, []).
:- use_module(my_data).

% BAD: Data embedded in visualization spec
curve_spec(revenue, [
    data([150000, 180000]),  % Hard to update/test
    ...
]).
```

### 2. Use Composition Over Large Specs

```prolog
% GOOD: Compose small specs
kpi_card(Name, Value) :- ...
layout_row(Cards) :- ...
dashboard(kpi_row(Cards)) :- ...

% BAD: One giant spec
dashboard_spec(my_dashboard, [
    layout(...),
    theme(...),
    widget1(...),
    widget2(...),
    % 200 more lines...
]).
```

### 3. Validate Early

```prolog
% GOOD: Validate before generation
generate_safe(Name, Code) :-
    validate_spec(Name, Errors),
    (Errors = []
    ->  generate(Name, Code)
    ;   format_errors(Errors, Message),
        throw(validation_error(Message))
    ).

% BAD: Generate and hope for the best
generate(Name, Code) :-
    % Might fail deep in generation with unclear error
    ...
```

### 4. Document Your Extensions

```prolog
%% my_custom_chart(+Name, +Options) is det.
%
% Creates a custom chart visualization.
%
% Options:
%   - data(+Data): List of data points
%   - style(+Style): One of [line, bar, area]
%   - color(+Color): Hex color string
%
% Example:
%   ?- my_custom_chart(sales, [data([1,2,3]), style(bar)]).
%
my_custom_chart(Name, Options) :-
    ...
```

## Summary

The UnifyWeaver visualization system provides:

- **Layered composition** - Independent layers that combine freely
- **Multi-level nesting** - Layouts within layouts, templates within templates
- **Parameterized patterns** - Reusable templates with customization
- **Conditional composition** - Data-driven visualization selection
- **Extensibility** - Add custom curves, templates, themes, animations
- **Introspection** - Query what's available
- **Validation** - Catch errors before generation

The key insight is that **complex UIs emerge from simple, composable primitives**. Each layer does one thing well and combines cleanly with others.

## What's Next?

- See Chapter 25 for advanced Storybook integration
- See Chapter 26 for performance optimization patterns
- See the API reference for complete predicate documentation
