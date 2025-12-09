% SPDX-License-Identifier: MIT
% Compilation script for Python family tree example

:- use_module(unifyweaver(targets/python_target)).

compile_family_tree :-
    % Load the family tree
    ['family_tree.pl'],
    
    % Compile parent facts (procedural mode for facts)
    compile_predicate_to_python(parent/2, [mode(generator)], ParentCode),
    open('parent.py', write, ParentStream),
    write(ParentStream, ParentCode),
    close(ParentStream),
    
    % Compile ancestor rules (procedural mode - recursive)
    compile_predicate_to_python(ancestor/2, [mode(generator)], AncestorCode),
    open('ancestor.py', write, AncestorStream),
    write(AncestorStream, AncestorCode),
    close(AncestorStream).

:- initialization(compile_family_tree, now).
