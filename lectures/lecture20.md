---
layout: default
title: "Lecture 20: Erlang"
---

Example source code: [series.erl](series.erl), [sort.erl](sort.erl)

Erlang
======

The origin of the name is both **Er**icsson **Lang**uage and the mathematician [Agner Erlang](http://en.wikipedia.org/wiki/Agner_Krarup_Erlang). It was invented by Joe Armstrong in 1986 for use in telephone exchange systems. The requirements of this application domain (fault-tolerance, concurrency) significantly influenced the language design.

Characteristics:

-   functional: a variable may be assigned a value only once
-   dynamically-typed
-   concurrency implemented by lightweight processes (like threads, but no shared memory) which communicate by exchanging messages

Processes in Erlang are similar to actors in Scala.

Syntax, Data types
==================

Erlang is a descendant of Prolog, and the syntax is very similar. Like Prolog, Erlang supports pattern matching to extract data values from composite data values such as lists and tuples.

The syntax is very similar to Prolog:

-   statements end with a period
-   variable names must begin with an upper-case letter

The built-in data types in Erlang are similar to those supported by Prolog. They include

-   symbols
-   numbers
-   lists
-   tuples (fixed-size records)
-   bit strings

Tuples
------

A tuple is a fixed-length series of values. Arbitrary record data structures can be created using tuples.

An important convention in Erlang is to use a symbol as the first member of a tuple as a tag to indicate the type of data the tuple contains.

For example:

{% highlight erlang %}
{ lineitem, {item, "Bananas"}, {quantity, 44} }
{% endhighlight %}

This is a tuple marked with the symbol (tag) **bananas**, with two nested tuples marked with the symbols **item** and **quantity**. This tuple might be part of a data structure used in an inventory-tracking system.

We could assign this tuple to a variable:

{% highlight erlang %}
Item = { lineitem, {item, "Bananas"}, {quantity, 44} }.
{% endhighlight %}

Things get interesting when we use pattern matching to extract information from the tuple:

{% highlight erlang %}
{lineitem, {item, "Bananas"}, {quantity, HowMany}} = Item.
{% endhighlight %}

This statement assigns the quantity associated with the **Item** tuple to the variable **HowMany**. This is the same idea as unification in Prolog: Erlang will try to make the left hand side equivalent to the right hand side.  It is also reminscent of vector destructuring in Clojure.  Constant values such as symbols and strings must be exactly equal for the match to succeed. Variables will match whatever value they correspond to on the other side.

Functions
=========

Functions in Erlang are specified in much the same way as inference rules in Prolog.

Annoying detail
---------------

Erlang has an interactive interpreter called **erl** in which you can enter Erlang statements and have them evaluated. However, you cannot define functions interactively. Instead, they must be defined in a separate source file (*module*) and compiled.

Example function
----------------

Because Erlang is a functional language, all computations involving repetition must be done recursively. Example: computing the nth Fibonacci number in Erlang.

Here is a module defined in a source file called **series.erl**:

{% highlight erlang %}
-module(series).
-export([fib/1]).

fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-2) + fib(N-1).
{% endhighlight %}

To iteractively compile this module and execute the **fib** function in **erl**:

<pre>
4> <b>c(series).</b>
{ok,fib}
5> <b>series:fib(6).</b>
13
</pre>

The built-in **c** function compiles a module whose name is specified as a symbol. Note that when an Erlang function is called, it must be prefixed with the name of the module in which it is defined. So, **series:fib** means to call a function called **fib** defined in a module called **series**.

More efficient version
----------------------

As you may recall from CS 201, the naive recursive implementation of **fib** has exponential running time. We can compute it more efficiently by avoiding revisiting the same recursive subproblem multiple times.

The idea is to use a tail-recursive implementation using an accumulator parameter. The base cases of the tail recursive helper function (**fibtailrecwork**) are the same as the original version. The recursive case's **Cur** parameter counts up from 2 to **N**, keeping track of the current Fibonacci number (**Accum**) and the previous Fibonacci number (**Prev**). Until **Cur** = **N**, recursive calls are made which compute the next Fibonacci number.

Note that in the base cases, we use the special variable name **\_** to indicate parameters that aren't used. You can think of this as the "don't care" variable name.

Here's the complete module:

{% highlight erlang %}
-module(series).
-export([fib/1, fibtailrec/1]).

fib(0) -> 1;
fib(1) -> 1;
fib(N) -> fib(N-2) + fib(N-1).

fibtailrec(N) -> fibtailrecwork(N, 2, 1, 2).

fibtailrecwork(0, _, _, _) -> 1;
fibtailrecwork(1, _, _, _) -> 1;
fibtailrecwork(N, N, _, Accum) -> Accum;
fibtailrecwork(N, Cur, Prev, Accum) -> fibtailrecwork(N, Cur+1, Accum, Prev+Accum).
{% endhighlight %}

Merge sort in Erlang
--------------------

Here is merge sort in Erlang. It is similar to [merge sort in Prolog](lecture15.html), although simpler because functions in Erlang return values rather than making logical assertions.

First, the **merge** function:

{% highlight erlang %}
merge([], Any) -> Any;
merge(Any, []) -> Any;
merge([X|RestL], [Y|RestR]) ->
  if
    X<Y  -> [X | merge(RestL, [Y|RestR])];
    true -> [Y | merge([X|RestL], RestR)]
  end.
{% endhighlight %}

The base cases state that merging an empty list with any other list results in the other list.

The recursive case uses an **if** expression to test which of the head elements of the two lists being merged is smaller, and then constructs a new list with the appropriate head element.

Note that this merge function could be improved: it is not tail recursive (the construction of the result list happens after the recursive call to **merge** completes).

Next, the **mergesort** function:

{% highlight erlang %}
mergesort([]) -> [];
mergesort([A]) -> [A];
mergesort(List) ->
  N = length(List),
  % Sublist containing the first N/2 elements.
  Left = lists:sublist(List, N div 2),
  % Sublist containing the remaining elements.
  % Note: list elements are indexed starting at 1, not 0.
  Right = lists:sublist(List, (N div 2) + 1, N - (N div 2)),
  % Recursively sort left and right sublists.
  LeftSorted = mergesort(Left),
  RightSorted = mergesort(Right),
  % Merge the results of sorting the left and right sublists.
  merge(LeftSorted, RightSorted).
{% endhighlight %}

Again, this function is quite a bit simpler than the equivalent version in Prolog because it returns a value instead of making a logical assertion. Note that we can use a series of expressions separated by commas to define the recursive case, and the result of the last expression is used as the result of the function.

Take a look at [sort.erl](sort.erl) to see the entire module.

Example of calling the **mergesort** function in the **erl** interpreter:

<pre>
40> <b>sort:mergesort([11, 86, 2, 69, 22, 39, 85, 57, 78, 76]).</b>
[2,11,22,39,57,69,76,78,85,86]
</pre>
