% SPDX-License-Identifier: MIT
% Sales Filtering Logic

% Row format: item, category, amount
% We define a rule that matches rows where Category is 'Electronics'
% and Amount is greater than 500.

high_value_electronics(Item, Category, Amount) :-
    Category = 'Electronics',
    Amount > 500.
