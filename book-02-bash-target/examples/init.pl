% SPDX-License-Identifier: MIT OR Apache-2.0
% Copyright (c) 2025 John William Creighton (s243a)
%
% This file is part of UnifyWeaver.
% Licensed under either MIT or Apache-2.0 at your option.

% This file initializes the UnifyWeaver educational environment.
% It should be loaded before running any of the examples.
%
% Expected directory structure:
%   UnifyWeaver/
%   ├── education/
%   │   └── book-02-bash-target/
%   │       └── examples/
%   │           └── init.pl  (this file)
%   └── src/
%       └── unifyweaver/

:- dynamic user:file_search_path/2.
:- dynamic user:unifyweaver_root/1.

% Load necessary library for path manipulation
:- use_module(library(filesex)).

unifyweaver_education_init :-
    prolog_load_context(directory, Here),
    % Go up 4 levels: examples -> book-02-bash-target -> education -> UnifyWeaver
    directory_file_path(Here, '..', Book02Dir),
    directory_file_path(Book02Dir, '..', EducationDir),
    directory_file_path(EducationDir, '..', ProjDir),
    retractall(user:unifyweaver_root(_)),
    assertz(user:unifyweaver_root(ProjDir)),
    directory_file_path(ProjDir, 'src', AbsSrcDir),
    directory_file_path(AbsSrcDir, 'unifyweaver', AbsUnifyweaverDir),
    asserta(user:file_search_path(unifyweaver, AbsUnifyweaverDir)),
    format('[UnifyWeaver] Educational environment initialized.~n', []),
    format('  Project root: ~w~n', [ProjDir]),
    format('  \'unifyweaver\' library alias configured for: ~w~n', [AbsUnifyweaverDir]),
    format('You can now use use_module(unifyweaver(...)) to load modules.~n').

:- initialization(unifyweaver_education_init, now).
