% SPDX-License-Identifier: MIT
% Family Tree Example for Python Target

% --- Facts ---
parent(abraham, ishmael).
parent(abraham, isaac).
parent(sarah, isaac).
parent(isaac, esau).
parent(isaac, jacob).
parent(rebekah, esau).
parent(rebekah, jacob).
parent(jacob, reuben).
parent(jacob, simeon).
parent(jacob, levi).
parent(jacob, judah).

% --- Rules ---

% ancestor(Ancestor, Descendant)
% Base case: A is an ancestor of D if A is a parent of D.
ancestor(A, D) :- parent(A, D).

% Recursive step: A is an ancestor of D if A is a parent of P,
% and P is an ancestor of D.
ancestor(A, D) :- 
    parent(A, P),
    ancestor(P, D).
