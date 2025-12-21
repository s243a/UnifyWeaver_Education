% SPDX-License-Identifier: MIT
% Compilation script for Go family tree example

:- use_module(unifyweaver(targets/go_target)).

compile_family_tree :-
    % Load the family tree
    ['family_tree.pl'],
    
    % Compile ancestor (generator mode handles dependencies like parent automatically)
    compile_predicate_to_go(ancestor/2, [mode(generator)], GoCode),
    
    % Write to file
    write_go_program(GoCode, 'ancestor.go').

:- initialization(compile_family_tree, now).
