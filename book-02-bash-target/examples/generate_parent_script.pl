% SPDX-License-Identifier: MIT OR Apache-2.0
% Copyright (c) 2025 John William Creighton (s243a)
%
% This file is part of UnifyWeaver.
% Licensed under either MIT or Apache-2.0 at your option.

% Load from project root (run with: swipl -f init.pl)
:- ['education/book-02-bash-target/examples/family_tree'].
:- use_module(unifyweaver(core/stream_compiler)).

generate_parent_script :-
    stream_compiler:compile_facts(parent, 2, [], BashCode),
    open('education/output/parent.sh', write, Stream),
    write(Stream, BashCode),
    close(Stream).

:- generate_parent_script.
:- halt.
