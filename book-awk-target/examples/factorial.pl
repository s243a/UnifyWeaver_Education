% SPDX-License-Identifier: MIT OR Apache-2.0
% Copyright (c) 2026 John William Creighton (s243a)
% Copyright (c) 2026 UnifyWeaver contributors
%
% Run from the compiler root with init.pl loaded. See VALIDATION.md.
:- use_module(unifyweaver(targets/awk_target)).
:- use_module(library(filesex)).

factorial(0, Acc, Acc).
factorial(N, Acc, Result) :-
    N > 0,
    N1 is N - 1,
    Acc1 is Acc * N,
    factorial(N1, Acc1, Result).

:- initialization(main, main).

main(Args) :-
    ( Args = [] -> OutputDir = 'output/awk-book'
    ; Args = [OutputDir]
    ),
    make_directory_path(OutputDir),
    directory_file_path(OutputDir, 'factorial.awk', FilePath),
    awk_target:compile_predicate_to_awk(factorial/3, [], Code),
    awk_target:write_awk_script(Code, FilePath).
