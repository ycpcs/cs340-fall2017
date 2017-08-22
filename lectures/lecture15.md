---
layout: default
title: "Lecture 15: Prolog"
---

Prolog is a *declarative programming language* based on logical inference. It is the main example of a language in the *logic programming* paradigm.

Download **tuProlog**, a Prolog interpreter written in Java:

> [tuProlog.zip](../resources/tuProlog.zip)

Extract the contents of the zip file, and double-click the file **bin/2p.jar** to start the Prolog interpreter. Or, cd to the **bin** directory and run the command

    java -jar 2p.jar

in a shell window.

Atoms
=====

Symbols:

{% highlight prolog %}
homer
marge
bart
lisa
maggie
{% endhighlight %}

Note: names of symbols *must* be written in lower-case letters.

Numbers:

> 1 2 3

Relations
=========

A relation is a table of facts. Here is a **father** relation

> <table>
> <col width="13%" />
> <col width="15%" />
> <tbody>
> <tr class="odd">
> <td align="left">homer</td>
> <td align="left">bart</td>
> </tr>
> <tr class="even">
> <td align="left">homer</td>
> <td align="left">lisa</td>
> </tr>
> <tr class="odd">
> <td align="left">homer</td>
> <td align="left">maggie</td>
> </tr>
> <tr class="even">
> <td align="left">grandpa</td>
> <td align="left">homer</td>
> </tr>
> <tr class="odd">
> <td align="left">grandpa</td>
> <td align="left">herb</td>
> </tr>
> </tbody>
> </table>

The first item in each tuple of the relation represents a person who is a father. The second item is a person who is a child of that father.

Relations can thus be formed as a collection of explicit facts, called *ground truths*. Each fact is a tuple belonging to the relation. In Prolog, we specify ground truths as

> relation ( *list of atoms* )

Here are some ground truths:

{% highlight prolog %}
father(homer, bart).
father(homer, lisa).
father(homer, maggie).
father(grandpa, homer).
father(grandpa, herb).

mother(marge, bart).
mother(marge, lisa).
mother(marge, maggie).
mother(grandma, homer).
{% endhighlight %}

Inference rules
===============

An inference rule allows new facts to be inferred from existing facts.

General form:

> *conclusion* :- *hypothesis*.

Facts and inference rules may use *variables*. A variable is a name, in upper-case letters, which stands for some possible member of a tuple in a relation.

Example: X is Y's paternal grandfather if there exists Z such that X is Z's father, and Z is Y's father:

{% highlight prolog %}
paternal_grandfather(X, Y) :- father(X, Z), father(Z, Y).
{% endhighlight %}

Note that X, Y, and Z are all variables, and the comma means "and" in the sense of a logical conjunction.

We can describe a paternal grandmother in a similar way:

{% highlight prolog %}
paternal_grandmother(X, Y) :- mother(X, Z), father(Z, Y).
{% endhighlight %}

Here is a possible set of inference rules:

{% highlight prolog %}
samefather(X, Y) :- father(Q, X), father(Q, Y).
samemother(X, Y) :- mother(Q, X), mother(Q, Y).

siblings(X,Y) :- (samemother(X,Y); samefather(X,Y)), X \= Y.

paternal_grandfather(X, Y) :- father(X, Q), father(Q, Y).
paternal_grandmother(X, Y) :- mother(X, Z), father(Z, Y).
{% endhighlight %}

Note the definition of the inference rule defining the **siblings** relation:

{% highlight prolog %}
siblings(X,Y) :- (samemother(X,Y); samefather(X,Y)), X \= Y.
{% endhighlight %}

The semicolon means "or" in the sense of logical disjunction. That is because two people are siblings if *either* they share the same father or mother.  Also, the **\\=** operator means "not equals", preventing any person from being considered to be his or her own sibling.  (Prolog allows the same value to be bound to multiple variables.)

Queries
=======

We can type in a potential fact, and based on the ground truths and the available inference rules, Prolog will attempt to find a derivation that proves that the fact is true.

Example:

{% highlight prolog %}
father(homer, bart).
{% endhighlight %}

This query is true because a ground truth matching the query exists.

The query

{% highlight prolog %}
father(marge, bart).
{% endhighlight %}

is false because this fact cannot be derived using the available ground truths and inference rules.

In general, answering a query requires constructing a chain of inferences. For example, the query

{% highlight prolog %}
siblings(bart, lisa).
{% endhighlight %}

is true because the facts

{% highlight prolog %}
mother(marge, bart).
mother(marge, lisa).
{% endhighlight %}

are ground truths, enabling the query

{% highlight prolog %}
samemother(bart, lisa).
{% endhighlight %}

to be true if **marge** is substituted for the variable **Q** in the rule defining the **samemother** relation. This, in turn, is sufficient to deduce that

{% highlight prolog %}
siblings(bart, lisa).
{% endhighlight %}

is true.

A more interesting query

{% highlight prolog %}
siblings(homer, herb).
{% endhighlight %}

is true because

{% highlight prolog %}
father(grandpa, homer).
father(grandpa, herb).
{% endhighlight %}

implies

{% highlight prolog %}
samefather(homer, herb).
{% endhighlight %}

which is sufficient to deduce that

{% highlight prolog %}
siblings(homer, herb).
{% endhighlight %}

is true. Note that the query

{% highlight prolog %}
samemother(homer, herb).
{% endhighlight %}

is false, because there is no derivation for this fact.

Queries with unknowns
---------------------

The real power of Prolog can be seen when a query contains one or more variables, which represent unknowns: for each variable, Prolog will attempt to find a value which can be substituted for the variable in order to make the query true.

For example, the query:

{% highlight prolog %}
paternal_grandfather(X, bart).
{% endhighlight %}

yields the answer

    yes.
    X / grandpa
    Solution: paternal_grandfather(grandpa,bart)

showing that **grandpa** can be substituted for the variable **X** in order to make the query true.

Note that a query with variables could lead to multiple solutions.  For example, the query

{% highlight prolog %}
siblings(X, bart).
{% endhighlight %}

yields two solutions, one where **lisa** is substituted for **X**, and one where **maggie** is substituted for **X**.

Declarative programming
=======================

Prolog is a declarative programming language because we never specify *how* we want a computation to be performed. We simply use ground truths and inference rules to describe a problem, and allow the inference algorithm to deduce a solution.

Declarative programming is nice because it allows us to specify a problem at a higher level.

Other declarative programming languages:

-   [Makefiles](http://en.wikipedia.org/wiki/Makefile), a language for directing the compilation of software
-   [SQL](http://en.wikipedia.org/wiki/Sql), the database query language

The language for Makefiles is especially interesting because, like Prolog, it is a logic programming language.
