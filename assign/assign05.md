---
layout: default
title: "Assignment 5: Boolean Function Synthesis"
---

Due: **Wednesday, Nov 8th** by 11:59 PM

Getting Started
===============

You can use tuProlog to write and test your Prolog code:

> [tuProlog.zip](../resources/tuProlog.zip)

Boolean Functions
=================

A *boolean operator* operates on one or more boolean values (true/false) to produce a boolean result. A binary boolean operator has two boolean operands.

We can describe a binary boolean operator using a *truth table*, which shows the result of the operator for each of the four possible combinations of operator values. For example, here is the truth table for the **and** operator, denoted **\^**:

L | R | L ^ R
--|---|------
t | t | t
t | f | f
f | t | f
f | f | f


For operands *L* and *R*, *L* **\^** *R* is true when *L* and *R* are both true, false otherwise. Here are the truth tables for four additional binary boolean operators, **or** (denoted **+**), **nand**, **nor**, and **xor**.

L | R | L + R
--|---|------
t | t | t
t | f | t
f | t | t
f | f | f

L | R | L nand R
--|---|------
t | t | f
t | f | t
f | t | t
f | f | t

L | R | L nor R
--|---|------
t | t | f
t | f | f
f | t | f
f | f | t

L | R | L xor R
--|---|------
t | t | f
t | f | t
f | t | t
f | f | f

We can think of boolean operators as being mathematical functions with two parameters. By combining two boolean operators with two operands, we can create a boolean function with three operands. For example, *A* **\^** (*B* **+** *C*) is a boolean function with the following truth table:

A | B | C | A ^ (B + C)
--|---|---|------------
t | t | t | t
t | t | f | t
t | f | t | t
t | f | f | f
f | t | t | f
f | t | f | f
f | f | t | f
f | f | f | f

Synthesizing an arbitrary boolean function
------------------------------------------

In digital circuit design, it is sometimes helpful to be able to create an arbitrary boolean functions out of binary operators. For example, *A* *op1* (*B* *op2* *C*) is a "template" for a boolean function with three parameters. By substituting different operators for *op1* and *op2*, we can create various functions.

Your Task
=========

Using Prolog, determine whether it is possible to synthesize the following binary functions by substituting the **and**, **or**, **nand**, **nor**, and **xor** operators in

> *A* *op1* (*B* *op2* *C*)

If it is possible to synthesize a function, determine which operands to use for *op1* and *op2*.

Functions 1-3
-------------

A | B | C | A op1 (B op2 C)
--|---|---|----------------
t | t | t | t
t | t | f | t
t | f | t | t
t | f | f | f
f | t | t | f
f | t | f | f
f | f | t | f
f | f | f | t

A | B | C | A op1 (B op2 C)
--|---|---|----------------
t | t | t | f
t | t | f | f
t | f | t | f
t | f | f | f
f | t | t | f
f | t | f | t
f | f | t | t
f | f | f | t

A | B | C | A op1 (B op2 C)
--|---|---|----------------
t | t | t | t
t | t | f | t
t | f | t | t
t | f | f | t
f | t | t | f
f | t | f | t
f | f | t | t
f | f | f | t


Functions 4-6
-------------

A | B | C | A op1 (B op2 C)
--|---|---|----------------
t | t | t | t
t | t | f | t
t | f | t | f
t | f | f | t
f | t | t | t
f | t | f | t
f | f | t | t
f | f | f | f

A | B | C | A op1 (B op2 C)
--|---|---|----------------
t | t | t | f
t | t | f | f
t | f | t | f
t | f | f | f
f | t | t | t
f | t | f | f
f | f | t | f
f | f | f | t

A | B | C | A op1 (B op2 C)
--|---|---|----------------
t | t | t | f
t | t | f | t
t | f | t | t
t | f | f | t
f | t | t | t
f | t | f | f
f | f | t | f
f | f | f | f

Hints
=====

Here is a suggestion for how to model the binary boolean operators:

{% highlight prolog %}
eval(and,t,t,t).
eval(and,t,f,f).
eval(and,f,t,f).
eval(and,f,f,f).
{% endhighlight %}

This establishes the ground truths for the **and** operator. The **eval** predicate specifies what the result of a given boolean operator is when given particular input values. A query with a free variable can be used to determine the result value for particular inputs: for example, for the query

{% highlight prolog %}
eval(and,t,f,What).
{% endhighlight %}

Prolog will infer that **What** is **f**.

The key will be finding a way to evaluate two boolean operators for three input values. Suggestion: define a predicate of the following form:

<pre>
composeTwo(X, Y, A, B, C, Z) :-  <i>something</i>.
</pre>

This predicate asserts that boolean operators *X* and *Y*, when evaluated on the expression

> *A* *X* (*B* *Y* *C*)

will produce the result *Z*. This is useful for specifying one row of the synthesized function's truth table. For example:

{% highlight prolog %}
composeTwo(X, Y, t, t, t, f)
{% endhighlight %}

would assert that the result of the function is **f** when *A* =**t**, *B* =**t**, and *C* =**t**.

Deliverables
============

There are two deliverables.

The first deliverable is a text file which specifies, for functions 1-6, either

-   which operators *op1* and *op2* can be used to define the function, or
-   that there is no way to substitute the 5 binary operators for *op1* and *op2* to define the function

The text file should also explain briefly how you used your Prolog code to find the operators (or prove their nonexistence) for each function.

The second deliverable is a text file containing your Prolog code.

Submitting
==========

Create a zip file with both deliverables described above and submit it to Marmoset as **assign05**:

> <https://cs.ycp.edu/marmoset/>
