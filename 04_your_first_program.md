# Chapter 4: Your First UnifyWeaver Program

This chapter is a hands-on tutorial that will guide you through the process of defining, compiling, and running a simple program with UnifyWeaver. We will use the concepts from the previous chapters to create a practical example.

We will be working with a simple family tree, defining parents and then creating a rule to find any ancestor of a person. We have already created the Prolog file for this tutorial: `education/family_tree.pl`.

## Step 1: Review the Prolog Code

First, let's examine the contents of `education/family_tree.pl`. It contains a set of facts and rules.

### The Facts
It starts by defining a series of `parent/2` facts. These represent the direct parent-child relationships.

```prolog
% parent(Parent, Child).
parent(abraham, ishmael).
parent(abraham, isaac).
parent(sarah, isaac).
parent(isaac, esau).
parent(isaac, jacob).
% ... and so on
```

### The Rules
Next, it defines two rules:

1.  `grandparent/2`: A simple, non-recursive rule that defines a grandparent as a parent of a parent.

    ```prolog
    grandparent(GP, GC) :-
        parent(GP, P),
        parent(P, GC).
    ```

2.  `ancestor/2`: A recursive rule that defines an ancestor using the classic transitive closure pattern. This is the kind of rule UnifyWeaver is built to optimize.

    ```prolog
    % Base case
    ancestor(A, D) :- parent(A, D).
    % Recursive step
    ancestor(A, D) :- 
        parent(A, P),
        ancestor(P, D).
    ```

## Step 2: Start SWI-Prolog

Now, open a terminal in the root directory of the UnifyWeaver project and start the SWI-Prolog interactive console. You can usually do this by typing `swipl`.

```bash
$ swipl
Welcome to SWI-Prolog ...
...
?- 
```
The `?-` is the Prolog prompt, waiting for your query.

## Step 3: Load the UnifyWeaver Compiler

To use UnifyWeaver, you first need to load its core modules. The easiest way to do this is to load the `recursive_compiler`, which also loads the other modules it depends on.

At the Prolog prompt, enter the following query. Note that we are using the path from the root of the project.

```prolog
?- use_module('src/unifyweaver/core/recursive_compiler').
true.
```

## Step 4: Load Your Program

Next, load the `family_tree.pl` file into the Prolog environment. This will make its facts and rules available to the compiler.

```prolog
?- ['education/family_tree'].
true.
```
(Note: You can omit the `.pl` extension.)

## Step 5: Compile the `ancestor/2` Predicate

Now for the main event. We will ask UnifyWeaver to compile our `ancestor/2` predicate. We will use the `compile_recursive/3` predicate, which takes three arguments:
1.  The predicate to compile, in `functor/arity` format.
2.  A list of options (we will use an empty list `[]` for now).
3.  A variable to hold the generated Bash code (we will use `BashCode`).

```prolog
?- compile_recursive(ancestor/2, [], BashCode).
```

Prolog will execute the compiler. The `BashCode` variable will be instantiated with a long string containing the full Bash script. Prolog will print this string to the console.

## Step 6: Save the Generated Code

While seeing the code on the screen is interesting, it's not very useful. We need to save it to a file. We can do this directly from Prolog using built-in predicates.

Let's re-run the compilation but this time, we will also write the output to a file named `family_tree.sh` in the `education` directory.

```prolog
?- compile_recursive(ancestor/2, [], BashCode),
   open('education/family_tree.sh', write, Stream),
   write(Stream, BashCode),
   close(Stream).
true.
```

This command chain does the following:
1.  `compile_recursive/3`: Compiles the predicate as before.
2.  `open/3`: Opens the file `education/family_tree.sh` for writing.
3.  `write/2`: Writes the content of the `BashCode` variable to the file.
4.  `close/1`: Closes the file stream.

You should now have a new file named `family_tree.sh` in the `education` folder.

## Step 7: Run the Bash Script

Finally, let's exit Prolog (`halt.`) and run our new script. Open `education/family_tree.sh` in a text editor. You will see that it has generated several functions, including `ancestor_all/1` and `ancestor_check/2`.

*   `ancestor_all [NAME]`: Finds all descendants of `[NAME]`.
*   `ancestor_check [ANCESTOR] [DESCENDANT]`: Checks if the relationship is true.

Let's try them out. From your terminal, first `source` the script to make the functions available, and then call them.

```bash
$ source education/family_tree.sh

# Find all known descendants of abraham
$ ancestor_all abraham
abraham:ishmael
abraham:isaac
abraham:esau
abraham:jacob
abraham:reuben
...

# Check if isaac is an ancestor of judah
$ ancestor_check isaac judah && echo "Yes" || echo "No"
Yes

# Check if sarah is an ancestor of esau
$ ancestor_check sarah esau && echo "Yes" || echo "No"
Yes

# Check if ishmael is an ancestor of jacob
$ ancestor_check ishmael jacob && echo "Yes" || echo "No"
No
```

## Congratulations!

You have successfully created a Prolog program, compiled it with UnifyWeaver, and executed the resulting high-performance Bash script. You have seen the entire workflow from a declarative logic program to an imperative, optimized script.

## Next Steps

In the next chapter, we will delve deeper into the compilation process, exploring the different options and how UnifyWeaver handles non-recursive predicates with the `stream_compiler`.
