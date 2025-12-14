% filter_high_value.pl - UnifyWeaver Java Target Example
% A simple predicate that filters records where value > 50
%
% Usage:
%   ?- use_module('src/unifyweaver/targets/java_target').
%   ?- ['education/other-books/book-java-target/examples/filter_high_value'].
%   ?- compile_predicate_to_java(filter_high_value/2, [pipeline_input(true)], Code),
%      write_java_program(Code, 'FilterHighValue.java').

:- module(filter_high_value, [filter_high_value/2]).

% Filter: keep records where value > 50
filter_high_value(Input, Output) :-
    get_field(Input, "value", Value),
    Value > 50,
    Output = Input.

% Helper predicate (would need to be implemented in java_target)
get_field(Record, Key, Value) :-
    get_dict(Key, Record, Value).
