---
layout: default
title: "Assignment 8: Code Generation"
---

**Due**: Tuesday, Dec 12th by 11:59 PM

# Getting Started

Download [cs340-assign08.zip](cs340-assign08.zip).

If you are using Counterclockwise under Eclipse, you can import the zipfile as an Eclipse project.

You should copy your `parser2.clj` and `astbuilder.clj` files from [Assignment 7](assign07.html) into the `src/minilang` directory.  <!-- Note that you will need to make a few changes to them as described below. -->

<!--
*Update 12/6*: The original assignment skeleton included the wrong version of `prettyprint.clj`.  Please download the correct one and copy it into the `src/minilang` folder:

> [prettyprint.clj](prettyprint.clj)
-->

# Your Task

The goal of this project is to write a code generator that can take the ASTs produced from parse trees of minilang programs and generate a sequence of [MiniVM](https://github.com/daveho/MiniVM) instructions which carry out the computation specified by the program.

## Augmented ASTs

This assignment introduces *augmented* ASTs.  An augmented AST has the same form as the basic ASTs produced by your `build-ast` function, but some of the nodes will have *properties*.  A node's properties can be used to store extra information about the node: specifically, information that will be useful during code generation.

The `analyzer.clj` module is provided to transform a plain AST into an augmented AST: the `analyzer/augment-ast` function does the transformation.  The analyzer adds three kinds of properties to the AST:

* `:statement_list` nodes will have an `:nlocals` property, specifying how many local variables are used within the statement list (and any nested statement lists in **if** or **while** statements)
* `:identifier` nodes will have a `:regnum` property, specifying an integer which uniquely identifies the variable named by the identifier
* the last `:expression_statement`, `:var_decl_statement`, `:if_statement`, or `:while_statement` node in the top-level statement list will have a `:last` property whose value is `true`

The `:regnum` property is very useful for generating code for assignments and variable references, since it tells you which MiniVM local variable corresponds to a program variable.

The `:last` property is useful to allow the code generator to emit code to print the result of the last statement in the top-level statement list: see "Code generation" below.

You will probably not need to use the `:nlocals` property for anything.

The `pp/pretty-print` function has been updated to print out property values.  For example, when the minilang program

    var a; a := 4 * 5; a;

is converted to an augmented AST and pretty printed, the output is

    :unit
    +--:statement_list :nlocals=1
       +--:var_decl_statement :regnum=0
       |  +--:identifier["a"]
       +--:expression_statement
       |  +--:op_assign
       |     +--:identifier["a"] :regnum=0
       |     +--:op_mul
       |        +--:int_literal["4"]
       |        +--:int_literal["5"]
       +--:expression_statement :last=true
          +--:identifier["a"] :regnum=0

Here is an example with multiple variables:

    var a; var b; var c; b := 6; c := 3; a := b*c;

This produces the following augmented AST:

    :unit
    +--:statement_list :nlocals=3
       +--:var_decl_statement :regnum=0
       |  +--:identifier["a"]
       +--:var_decl_statement :regnum=1
       |  +--:identifier["b"]
       +--:var_decl_statement :regnum=2
       |  +--:identifier["c"]
       +--:expression_statement
       |  +--:op_assign
       |     +--:identifier["b"] :regnum=1
       |     +--:int_literal["6"]
       +--:expression_statement
       |  +--:op_assign
       |     +--:identifier["c"] :regnum=2
       |     +--:int_literal["3"]
       +--:expression_statement :last=true
          +--:op_assign
             +--:identifier["a"] :regnum=0
             +--:op_mul
                +--:identifier["b"] :regnum=1
                +--:identifier["c"] :regnum=2

Notice that each variable is assigned a different `:regnum` property value.

## Code generation

The `generate-code` function takes an augmented AST as a parameter, and should print MiniVM instructions which execute the computation specified by the AST.  Note that `generate-code` does not need to return any specific value: it should just print the MiniVM instructions using the `println` function.

Here is the basic idea:

* For statement lists, recursively generate code for each child statement
* Nothing is required for var decl statements
* For expression statements, recursively generate code for the expression, then either pop the result value off by emitting a `pop` instruction (if the expression statement does not have the `:last` property), or print it by emitting `syscall $println` followed by `pop` (if the expression statement does have the `:last` property)
* For binary operators other than `:op_assign`, recursively generate code for the two child sub-expressions, and then emit an appropriate arithmetic instruction (e.g., `add`, `sub`, `mul`, etc.)
* For `:op_assign`, recursively emit code for the right-hand-side expression, the emit an `stlocal` instruction to store the result in a local variable, using the value of the `:regnum` property of the identifier to know which MiniVM local variable to store into; also see the note below in the "Stack management" section
* For `:int_literal` nodes, emit an `ldc_i` instruction to load the constant integer onto the operand stack
* For `:identifier` nodes which appear in expressions, emit an `ldlocal` instruction, using the value of the `:regnum` property to know which MiniVM local variable to load from

The `compile-unit` function is provided for you: it takes a complete augmented AST (with a `:unit` node as its root) and generates a complete MiniVM program for you.  This function is necessary because some prologue and epilogue code is required to form a complete MiniVM program: the prologue creates an initial stack frame, and the epilogue causes the MiniVM program to exit cleanly.

When you test your code generator, you should do so by using the `compile-unit` function, e.g.

{% highlight clojure %}
(compile-unit aast)
{% endhighlight %}

to generate code for the `testprog` test program at the bottom of `codegen.clj`.  I suggest running this in a REPL.

To try out your generated code, copy the generated instructions and save them to a file &mdash; e.g., "prog.mvm" &mdash; then execute it interactively with the `MiniVM.rb` program, e.g.:

    ./MiniVM.rb -x -i prog.mvm

## Stack management

You will need to think carefully about how to manage the MiniVM operand stack.  The basic idea is that the code generated for each statement should not cause the operand stack either to grow or shrink.

For `:op_assign` expressions, the value of the right-hand-side expression should be left on the stack.  You can use the `dup` MiniVM instruction to make a copy of it just before you use `stlocal` to store it in a local variable: this ensures that a copy is left on the stack after the value is stored.

## Hints

The [MiniVM test programs](https://github.com/daveho/MiniVM/tree/master/t), [MiniVM documentation](https://github.com/daveho/MiniVM/blob/master/Documentation.md), and [MiniVM instruction reference](https://github.com/daveho/MiniVM/blob/master/InstructionSet.md) will probably be useful.

The functions in the `node.clj` module will be useful for working with augmented AST nodes:

* `node/has-prop?` checks whether a node has a particular property
* `node/get-prop` retrieves the value of a property from a node
* `node/children` retrieves the children of a node
* `node/num-children` returns a count of how many children a node has
* `node/get-child` returns a specified child (0 for the first child, etc.)

Each of these functions has a detailed comment explaining how to use it.

You can check the label of a node by applying the `:symbol` property to the node, e.g.

{% highlight clojure %}
(:symbol node)
{% endhighlight %}

would return `:expression_statement` if `node` is an expression statement node.

When printing, you can use the `str` function to concatenate multiple values into a single string.  For example,

{% highlight clojure %}
(println (str "\tldlocal " regnum))
{% endhighlight %}

would emit the instruction

    ldlocal 4

assuming that `regnum` has the value 4.

You can use the `do` construct to execute several Clojure expressions.  This is useful, for example, if you need to recursively generate code and then print an instruction, e.g.:

{% highlight clojure %}
(do
  (generate-code (node/get-child node 0))
  (generate-code (node/get-child node 1))
  (println "\tadd"))
{% endhighlight %}

## Examples

Here are some tests programs and example outputs.

For the following minilang program:

    var a; a := 4 * 5; a;

A possible MiniVM program is

{% highlight asm %}
main:
    enter 0, 1
    ldc_i 4
    ldc_i 5
    mul
    dup
    stlocal 0
    pop
    ldlocal 0
    syscall $println
    pop
    ldc_i 0
    ret
{% endhighlight %}

For the following minilang program:

    var a; var b; var c; b := 6; c := 3; a := b*c;

A possible MiniVM program is

{% highlight asm %}
main:
    enter 0, 3
    ldc_i 6
    dup
    stlocal 1
    pop
    ldc_i 3
    dup
    stlocal 2
    pop
    ldlocal 1
    ldlocal 2
    mul
    dup
    stlocal 0
    syscall $println
    pop
    ldc_i 0
    ret
{% endhighlight %}

# Grading

* Evaluation of simple expressions with just constants: 20%
* Evaluation of complex expressions with just constants: 20%
* Assignments of constants to variables: 15%
* Evaluating variables in expressions: 15%
* Evaluation of expressions with constants and variables: 20%
* Printing the value of the last expression using `syscall $println`: 10%

## Insane extra credit

For extra credit (up to 50 points), implement code generation for `:if_statement` and `:while_statement` AST nodes.

See [Implementing Control Flow](assign08controlflow.html) for more information.

This is actually not particularly difficult, and allows you to compile and run rather sophisticated programs.  And, imagine how cool it will be to mention that you implemented a compiler for a Turing-complete language with full support for variables and arbitrary control flow targeting a stack-based virtual machine in a functional language at the next party you go to.

<div class="callout">
<b>Important</b>: If you decide to do the extra credit, the code you submit must be <i>entirely your own work</i>, meaning that you should not talk about the extra credit with anyone else.
</div>

# Submitting

When you are done, submit the lab to the Marmoset server using either of the methods below.

> **Important**: after you submit, log into the submission server and verify that the correct files were uploaded. You are responsible for ensuring that you upload the correct files. I may assign a grade of 0 for an incorrectly submitted assignment.

From Eclipse
------------

If you have the [Simple Marmoset Uploader Plugin](http://ycpcs.github.io/cs201-fall2014/resources/index.html) installed, select the project (**CS340\_Assign08**) in the package explorer and then press the blue up arrow button in the toolbar. Enter your Marmoset username and password when prompted.

From a web browser
------------------

Create a zip file containing your completed project.  (If you are in Eclipse, you can use **File &rarr; Export... &rarr; General &rarr; Archive File**.)

Upload the saved zip file to the Marmoset server as **assign08**. The server URL is

> [https://cs.ycp.edu/marmoset/](https://cs.ycp.edu/marmoset/)

From the command line
---------------------

From the command line, run the command

    make submit

Type your Marmoset username and password when prompted.
