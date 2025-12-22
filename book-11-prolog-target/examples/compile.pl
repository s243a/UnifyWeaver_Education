% SPDX-License-Identifier: MIT
% Compilation script for Prolog target example

:- use_module(unifyweaver(targets/prolog_target)).

compile_prolog_example :-
    % Load the factorial code
    ['factorial.pl'],

    % Generate standalone script (interpreted mode for portability)
    generate_prolog_script([factorial/2, run_test/0],
                          [dialect(swi), compile(false), entry_point(run_test)],
                          Code),

    % Write to file
    write_prolog_script(Code, 'factorial_script.pl', [dialect(swi)]).

:- initialization(compile_prolog_example, now).
