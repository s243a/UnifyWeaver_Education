% SPDX-License-Identifier: MIT
% Compilation script for AWK target example

:- use_module(unifyweaver(targets/awk_target)).

compile_awk_example :-
    % 1. Compile the filter script
    ['sales_filter.pl'],
    compile_predicate_to_awk(high_value_electronics/3, 
        [field_separator('\t'), unique(false)], 
        FilterCode),
    write_awk_script(FilterCode, 'filter_sales.awk'),
    
    % 2. Compile an aggregation script (sum)
    % We compile a dummy predicate 'total_sales/1' with aggregation option
    compile_predicate_to_awk(total_sales/1, 
        [aggregation(sum), field_separator('\t')], 
        SumCode),
    write_awk_script(SumCode, 'sum_sales.awk').

:- initialization(compile_awk_example, now).

