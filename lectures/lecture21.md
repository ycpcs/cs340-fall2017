---
layout: default
title: "Lecture 21: More Erlang"
---

Example code: [map.erl](map.erl), [filter.erl](filter.erl)

Anonymous Functions
===================

Many languages, including Clojure, Scala, and Ruby, have the ability to define "anonymous" blocks of code. An important use of such code blocks is to transform a series of values from a collection (e.g., passing an anonymous function to the Clojure **map** function.)

Erlang supports anonymous functions. Because Erlang is dynamically-typed, an anonymous function can be assigned to a variable (Clojure allows this as well). You can think of an anonymous function as being a value, in much the same way that numbers, lists, and tuples are values in Erlang. Like all other values, they can be passed to functions and returned from functions.

Example:

<pre>
1&gt; <b>Add1 = fun(N) -&gt; N+1 end.</b>
#Fun&lt;erl_eval.6.80247286&gt;
2&gt; <b>Add1(3).</b>
4
</pre>

Here, we defined a function of one parameter **N** that adds 1 to its parameter and returns the result.

More interesting example:

<pre>
3&gt; <b>AddN = fun(N) -&gt; fun(X) -&gt; N+X end end.</b>
#Fun&lt;erl_eval.6.80247286&gt;
4&gt; <b>Add12 = AddN(12).</b>
#Fun&lt;erl_eval.6.80247286&gt;
5&gt; <b>Add12(11).</b>
23
</pre>

In this example, the **AddN** function takes a parameter **N** and returns a function that adds **N** to its parameter **X**. By calling **AddN** with the argument value 12, we create a function that adds 12 to its parameter.

The concept of returning a function from a function is called *currying*.

Transforming Lists
==================

Anonymous functions can be applied to lists to select or transform the values in the list.

One way to transform a list is to *map* a function onto each element of the list, producing a list of transformed values as a result.

Here is a possible implementation of a map function:

{% highlight erlang %}
-module(map).
-export([map/2]).

map(_, []) -> [];
map(F, [First|Rest]) -> [F(First) | map(F, Rest)].
{% endhighlight %}

The implementation is quite simple. The base case is an empty list, where the result is simply the empty list. In the recursive case, the function *F* is applied to the first element of the list, and prepended onto the list that results from recursively applying *F* to the rest of the list.

Testing it on a list:

<pre>
8&gt; <b>map:map(fun(N) -&gt; N*2 end, [1, 2, 3]).</b>
[2,4,6]
</pre>

Note that the implementation of **map** above is not tail-recursive. Here is a tail-recursive version:

{% highlight erlang %}
-module(map).
-export([map/2]).

mapwork(_, [], Accum) -> lists:reverse(Accum);
mapwork(F, [First|Rest], Accum) -> mapwork(F, Rest, [F(First)|Accum]).

map(F, L) -> mapwork(F, L, []).
{% endhighlight %}

Because the accumulator builds the result list with the transformed elements in reverse order, we apply the built-in **lists:reverse** function before returning the final result.

Another useful list-transformation technique is filtering a list to retain or discard elements matching a specified predicate:

{% highlight erlang %}
-module(filter).
-export([retain/2, discard/2]).

filterwork(_, [], _, Accum) -> lists:reverse(Accum);
filterwork(F, [First|Rest], Keep, Accum) ->
  Test = F(First),
  if
    (Test and Keep) or (not Test and not Keep) ->
       filterwork(F, Rest, Keep, [First | Accum]);
    true -> filterwork(F, Rest, Keep, Accum)
  end.

retain(F, List) -> filterwork(F, List, true, []).

discard(F, List) -> filterwork(F, List, false, []).
{% endhighlight %}

Examples of using these functions:

<pre>
18&gt; <b>filter:retain(fun(N) -&gt; N &gt; 4 end, [1, 2, 3, 4, 5, 6, 7, 8]).</b>
[5,6,7,8]
19&gt; <b>filter:discard(fun(N) -&gt; N &gt; 4 end, [1, 2, 3, 4, 5, 6, 7, 8]).</b>
[1,2,3,4]
</pre>

Built-in versions
-----------------

It's interesting to build our own list-transformation functions, but in practice it's better to use the built-in implementations, which are **list::map**, **lists:takewhile**, and **lists:dropwhile**.

List Comprehensions
===================

*List comprehensions* are a concise syntax for describing transformations of one or more lists.

In the following examples, the variable **List** is defined as:

    List = [1, 2, 3, 4, 5, 6, 7, 8].

Example: double each element in a list:

<pre>
26&gt; <b>[N * 2 || N &lt;- List].</b>
[2,4,6,8,10,12,14,16]
</pre>

Read this as "select elements *N* from *List*, and generate a new list by adding elements of the form *N*\*2".

Example: get all elements greater than 4:

<pre>
27&gt; <b>[N || N &lt;- List, N &gt; 4].</b>
[5,6,7,8]
</pre>

Here, we've specified an additional clause *N*\>4 to restrict which elements of *List* are used to generate the result list.

Example: double all elements greater than 4:

<pre>
28&gt; <b>[N*2 || N &lt;- List, N &gt; 4].</b>
[10,12,14,16]
</pre>

In this example, we both selected and transformed the input list.
