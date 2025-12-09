% SPDX-License-Identifier: MIT
% SQL Target Example - Employee Database

:- encoding(utf8).
:- use_module(unifyweaver(targets/sql_target)).

% Table declarations
:- sql_table(employees, [id-integer, name-text, dept-text, salary-integer, hire_date-text]).

% Predicates
all_employees(Id, Name, Dept, Salary) :-
    employees(Id, Name, Dept, Salary, _).

engineering_team(Name, Salary) :-
    employees(_, Name, Dept, Salary, _),
    Dept = 'Engineering'.

high_earners(Name, Salary) :-
    employees(_, Name, _, Salary, _),
    Salary > 80000.

% Test runner
test_sql_generation :-
    format('~n=== SQL Target Example ===~n~n'),

    format('1. All employees:~n'),
    compile_predicate_to_sql(all_employees/4, [format(select)], SQL1),
    format('   ~w~n~n', [SQL1]),

    format('2. Engineering team:~n'),
    compile_predicate_to_sql(engineering_team/2, [format(select)], SQL2),
    format('   ~w~n~n', [SQL2]),

    format('3. High earners:~n'),
    compile_predicate_to_sql(high_earners/2, [format(select)], SQL3),
    format('   ~w~n~n', [SQL3]),

    format('4. As CREATE VIEW:~n'),
    compile_predicate_to_sql(high_earners/2, [format(view), view_name(top_earners)], SQL4),
    format('   ~w~n~n', [SQL4]).

:- initialization(test_sql_generation, now).
