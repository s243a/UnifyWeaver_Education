% SPDX-License-Identifier: MIT
% Prolog Target Example - Factorial

% Standard factorial implementation
factorial(0, 1) :- !.
factorial(N, R) :- 
    N > 0, 
    N1 is N - 1, 
    factorial(N1, R1), 
    R is N * R1.

% Test predicate to run from generated script
run_test :-
    factorial(5, R),
    format('Factorial of 5 is ~w~n', [R]).
