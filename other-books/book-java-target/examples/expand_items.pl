% expand_items.pl - UnifyWeaver Java Target Example
% A generator predicate that expands items list into separate records
%
% Usage:
%   ?- use_module('src/unifyweaver/targets/java_target').
%   ?- ['education/other-books/book-java-target/examples/expand_items'].
%   ?- compile_predicate_to_java(expand_items/2, [generator_mode(true)], Code),
%      write_java_program(Code, 'ExpandItems.java').

:- module(expand_items, [expand_items/2]).

% Generator: each item in items list becomes a separate record
expand_items(Input, Output) :-
    get_field(Input, "items", Items),
    member(Item, Items),
    set_field(Input, "item", Item, Temp),
    remove_field(Temp, "items", Output).

% Helper predicates (would need to be implemented in java_target)
get_field(Record, Key, Value) :-
    get_dict(Key, Record, Value).

set_field(Record, Key, Value, NewRecord) :-
    put_dict(Key, Record, Value, NewRecord).

remove_field(Record, Key, NewRecord) :-
    del_dict(Key, Record, _, NewRecord).
