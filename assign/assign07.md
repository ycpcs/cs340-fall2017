---
layout: default
title: "Assignment 7: Abstract Syntax Trees"
---

**Due**: Tuesday, Dec 5th by 11:59 PM

*Update 12/1*: Added a hint about implementing `flatten-statement-list`

# Getting Started

Download [cs340-assign07.zip](cs340-assign07.zip).

If you are using Counterclockwise under Eclipse, you can import the zipfile as an Eclipse project.

You should copy your `parser2.clj` file from [Assignment 6](assign06.html) into the `src/minilang` directory.

# Your task

Your task is to transform the parse trees produced by your parser into [abstract syntax trees](../lectures/lecture06.html).  Do this by implementing the **build-ast** function in `astbuilder.clj`.

There are three things that your **build-ast** function should do:

* All nonterminal nodes that have a single child should be eliminated.  The only exception is the **:unit** node at the root of the parse tree, which should be preserved.
* **:statement\_list** nodes should be simplified so that all of the statements are direct children of the statement list node.
* All terminal nodes should be eliminated, unless they contain information that is important.  For example, "punctuation" nodes such as **:lparen**, **:semicolon** should be eliminated, but **:identifier**, **:int\_literal**, and **str\_literal** nodes should be preserved

# Hints

In general, building an AST will involve recursively converting child parse trees into ASTs.  Of course, there will be some base cases where a parse node is already in the form of an AST.  (For example, identifiers, int literals, and string literals can be considered to already be ASTs.)

You will need to think carefully about the structure of the parse nodes you will encounter, and how to transform them into equivalent AST nodes.

There are several helper functions that you may find useful.

The **node/make-node** function provides a convenient way to create a new AST node.  It takes a symbol and a sequence of child nodes as parameters.

The **node/children** function takes a parse node as a parameter and returns a vector containing the children of the parse node.  (It will throw an exception if passed a terminal node.)

The **node/get-child** function takes a parse node and an integer *n*, and returns the *n*th child of the parse node (with 0 being the index of the first child).

The **recur-on-children** function takes a parse node as a parameter, and returns an AST node whose symbol is the same as the parse node, and whose children are ASTs constructed from the children of the parse node.  (Hint: this should be useful for nodes representing binary operators.)

**:primary** nodes representing parenthesized expressions will require special handling: specifically, the *second* child should be recursively turned into an AST, rather than the first child (which is the correct approach for the other kinds of primary expressions.)

Implementing the `flatten-statement-list` requires accumulating all of the reachable `:statement` nodes, converting them to a sequence of ASTs, and creating a `:statement_list` AST with the `:statement` ASTs as children.  As a way of getting started, here is an implementation of `flatten-statement-list` that correctly handles the first statement:

{% highlight clojure %}
(defn flatten-statement-list [node]
  (let [stmt (node/get-child node 0)
        stmt-ast (build-ast stmt)]
    (node/make-node :statement_list [stmt-ast])))
{% endhighlight %}

The full version of `flatten-statement-list` should use a `loop/recur` construct to recursively find all of the statements and convert them to ASTs.

# Testing

You can test your **build-ast** function by changing the definition of the **testprog** variable (defined towards the bottom of `astbuilder.clj`.  The value of this variable is parsed, an AST is constructed from the resulting parse tree, **build-ast** is called to convert the parse tree to an AST, and the AST is assigned to the variable **prog**.

In a REPL, you can evaluate

{% highlight clojure %}
(pp/pretty-print ast)
{% endhighlight %}

to print the AST.

Here are some example inputs and the expected ASTs:

Example input:

    var a; a := 3*4;

Expected AST:

    :unit
    +--:statement_list
       +--:var_decl_statement
       |  +--:identifier["a"]
       +--:expression_statement
          +--:op_assign
             +--:identifier["a"]
             +--:op_mul
                +--:int_literal["3"]
                +--:int_literal["4"]

Example input:

    a * (b + 3);

Expected AST:

    :unit
    +--:statement_list
       +--:expression_statement
          +--:op_mul
             +--:identifier["a"]
             +--:op_plus
                +--:identifier["b"]
                +--:int_literal["3"]

Example input:

    while (a <= b) { c; d*e*4; }

Expected AST:

    :unit
    +--:statement_list
       +--:while_statement
          +--:op_lte
          |  +--:identifier["a"]
          |  +--:identifier["b"]
          +--:statement_list
             +--:expression_statement
             |  +--:identifier["c"]
             +--:expression_statement
                +--:op_mul
                   +--:op_mul
                   |  +--:identifier["d"]
                   |  +--:identifier["e"]
                   +--:int_literal["4"]

Example input:

    if (x != 4) { y := z*3; }

Expected AST:

    :unit
    +--:statement_list
       +--:if_statement
          +--:op_neq
          |  +--:identifier["x"]
          |  +--:int_literal["4"]
          +--:statement_list
             +--:expression_statement
                +--:op_assign
                   +--:identifier["y"]
                   +--:op_mul
                      +--:identifier["z"]
                      +--:int_literal["3"]

# Grading

Your assignment grade will be determined as follows:

* Flattening of statement lists: 40%
* Removing unnecessary nonterminal nodes: 15%
* Removing unnecessary terminal nodes: 15%
* Binary expressions: 15%
* If and while statements: 15%

# Submitting

When you are done, submit the assignment to the Marmoset server using either of the methods below.

> **Important**: after you submit, log into the submission server and verify that the correct files were uploaded. You are responsible for ensuring that you upload the correct files. I may assign a grade of 0 for an incorrectly submitted assignment.

From a web browser
------------------

Create a zip file containing your completed project.  (If you are in Eclipse, you can use **File &rarr; Export... &rarr; General &rarr; Archive File**.)

Upload the saved zip file to the Marmoset server as **assign07**. The server URL is

> [https://cs.ycp.edu/marmoset/](https://cs.ycp.edu/marmoset/)

From the command line
---------------------

From the command line, run the command

    make submit

Type your Marmoset username and password when prompted.
