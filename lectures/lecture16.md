---
layout: default
title: "Lecture 16: More Prolog"
---

Recursion
=========

Prolog inference rules can be recursive. For example:

{% highlight prolog %}
ancestor(X, Y) :- father(X, Y).
ancestor(X, Y) :- mother(X, Y).
ancestor(X, Y) :- father(X, Z), ancestor(Z, Y).
ancestor(X, Y) :- mother(X, Z), ancestor(Z, Y).
{% endhighlight %}

These rules establish that *X* is *Y*'s ancestor if

-   *X* is *Y*'s father or mother (first two rules, not recursive), or
-   there is some *Z* such that *X* is *Z*'s father and *Z* is *Y*'s ancestor, or
-   there is some *Z* such that *X* is *Z*'s mother and *Z* is *Y*'s ancestor

Example query (using the ground truths from the [previous lecture](lecture15.html):

<pre>
| ?- <b>ancestor(grandpa, bart).</b>

true ? 

yes
| ?- <b>ancestor(grandpa, homer).</b>

true ? 

yes
| ?- <b>ancestor(grandpa, marge).</b>

no
</pre>

Numbers
=======

A prolog variable can have a numeric value (as opposed to a symbol). The **is** keyword binds a variable to an expression with a numeric value.

Example:

<pre>
| ?- <b>A is 2 + 3.</b>

A = 5

yes
</pre>

Tuples and Lists
================

A tuple is a sequence with a fixed number of values. A list is a sequence with an arbitrary number (zero or more) of values.

A tuple:

{% highlight prolog %}
(a, b, c)
{% endhighlight %}

A list:

{% highlight prolog %}
[a, b, c]
{% endhighlight %}

Lists have an alternate syntax:

<pre>
[<i>FirstElement</i>|<i>RestOfList</i>]
</pre>

The alternate syntax is very useful for writing inference rules that operate on lists.

As an example: finding the smallest number in a list of numbers.

First, we can define a **min** rule which, for a pair of numbers, specifies which is the minimum:

{% highlight prolog %}
min(A, A, B) :- A =< B.
min(B, A, B) :- B =< A.
{% endhighlight %}

Read the first rule as "A is the minimum of A and B if A &lt;= B".

Examples:

<pre>
| ?- <b>min(What, 2, 3).</b>

What = 2 ? 

yes
| ?- <b>min(What, 3, 2).</b>

What = 2

yes
</pre>

We can define two rules for finding the smallest number in a list of numbers:

{% highlight prolog %}
smallest(A, [A|[]]).
smallest(Min, [A|B]) :- smallest(SB, B), min(Min, A, SB).
{% endhighlight %}

The first rule specifies that in a list where *A* is the only element, it is the smallest element. This serves as a base case.

The second rule specifies that for a list where *A* is the first element and *B* is a list containing an unspecified number of elements, the minimum value *Min* is the minimum of *A* and *SB*, where *SB* is the smallest value in *B*.

Example:

<pre>
| ?- <b>smallest(What, [11, 86, 2, 69, 22, 39, 85, 57, 78, 76]).</b>

What = 2 ? 

yes
</pre>

Example: Sorting
================

Now that we've defined how to find the smallest item in a list, we define how to sort a list.

The base case is that sorting a list with no elements is the empty list:

{% highlight prolog %}
sorted([], []).
{% endhighlight %}

The recursive case defines what it means to sort a list with at least one element:

{% highlight prolog %}
sorted([Min|RestSorted], List) :-
  smallest(Min, List),
  append(BeforeMin, [Min|AfterMin], List),
  append(BeforeMin, AfterMin, RestUnsorted),
  sorted(RestSorted, RestUnsorted).
{% endhighlight %}

This rule states that the sorted form of a list is a list containing at least one element is the list where the first element is the minimum value in the original list, and the subsequent values are the rest of the elements in the original list in sorted order.

To extract the minimum element from the list, we use the built-in **append** rule. The assertion

{% highlight prolog %}
append(A, B, C)
{% endhighlight %}

says that *C* is the result of concatenating the lists *A* and *B*. We use this rule twice. The first use,

{% highlight prolog %}
append(BeforeMin, [Min|AfterMin], List)
{% endhighlight %}

states that *List* is the result of concatenating two lists. The first list is *BeforeMin*. The second list has *Min* (the minimum element of the overall list) as its first element, and *AfterMin* as the remaining elements. This effectively gives us lists *BeforeMin* and *AfterMin*, which are lists with the elements that precede and succeed *Min*.

The second use,

{% highlight prolog %}
append(BeforeMin, AfterMin, RestUnsorted)
{% endhighlight %}

says that *RestUnsorted* is the list formed by concatenating *BeforeMin* and *AfterMin*. We use *RestUnsorted* to define *RestSorted*:

{% highlight prolog %}
sorted(RestSorted, RestUnsorted)
{% endhighlight %}

This is a recursive application of the **sorted** rule, which here says that *RestSorted* is the elements in *RestUnsorted* in sorted order.

Example use:

<pre>
| ?- <b>sorted(What, [11, 86, 2, 69, 22, 39, 85, 57, 78, 76]).</b>

What = [2,11,22,39,57,69,76,78,85,86] ? 

yes
</pre>

Is this an algorithm?
---------------------

In a declarative language, it becomes difficult to say what algorithm is used to compute a result. In our definition of the **sorted** rule, the definition resembles a selection sort, where we building a sorted result by repeatedly selecting the minimum element.

A difficulty with declarative programming is because the programmer does not directly specify an algorithm, it is difficult to reason about the efficiency with which the computation will be carried out.

Merge Sort
==========

Here is merge sort in Prolog.

Recall that merge sort is a recursive sorting algorithm based on *merging* sorted lists to produce a single sorted list that contains all of the elements from the two input lists.

Here is how we can define the merge operation in Prolog. First, the base cases:

{% highlight prolog %}
merge(List, List, []).
merge(List, [], List).
{% endhighlight %}

These rules state that the result of merging any sorted list with the empty list produces that list.

A pair of recursive rules define the more general case of merging two nonempty lists:

{% highlight prolog %}
merge([MinList1|RestMerged], [MinList1|RestList1], [MinList2|RestList2]) :-
  MinList1 =< MinList2,
  merge(RestMerged,RestList1,[MinList2|RestList2]).
merge([MinList2|RestMerged], [MinList1|RestList1], [MinList2|RestList2]) :-
  MinList2 =< MinList1,
  merge(RestMerged,[MinList1|RestList1],RestList2).
{% endhighlight %}

The first rule says that the if *MinList1*, the first element of the list [*MinList1* \| *RestList1*] is smaller than *MinList2* (the first element of the list [*MinList2* \| *RestList2* ]), then the result of merging the two lists is *MinList1*, followed by the result of merging the lists *RestList1* and [*FirstList2* \| *RestList2*]. The second rule handles the symmetric case (where *MinList2* is less than *MinList1*).

Testing the merge rule:

<pre>
| ?- <b>merge(What, [1, 3, 5, 6], [2, 4, 7]).</b>

What = [1,2,3,4,5,6,7] ? 
</pre>

Now that we have a merge operation, we can define the **mergeSort** rule. First, the two base cases:

{% highlight prolog %}
mergeSort([], []).
mergeSort([A], [A|[]]).
{% endhighlight %}

These rules state that sorting a list with no elements or exactly one element produces the same list as a result.

Next, the general (recursive) case:

{% highlight prolog %}
mergeSort(Sorted, List) :-
  length(List, N),
  FirstLength is div(N, 2),
  SecondLength is N - FirstLength,
  length(FirstUnsorted, FirstLength),
  length(SecondUnsorted, SecondLength),
  append(FirstUnsorted, SecondUnsorted, List),
  mergeSort(FirstSorted, FirstUnsorted),
  mergeSort(SecondSorted, SecondUnsorted),
  merge(Sorted, FirstSorted, SecondSorted).
{% endhighlight %}

A few things to note here:

-   The **length** predicate asserts that the length of the list called *List* is *N*
-   The **div** function does integer division
-   *FirstLength* and *SecondLength* are the lengths required to split the overall *List* into two equal parts: the **length** predicate is used to assert that *FirstUnsorted* and *SecondUnsorted* are lists with those lengths
-   **append(FirstUnsorted, SecondUnsorted, List)** asserts that *List* is the result of concatenating *FirstList* and *SecondList*
-   The recursive applications of **mergeSort** assert that *FirstSorted* and *SecondSorted* are the results of sorting *FirstUnsorted* and *SecondUnsorted*, respectively
-   The clause **merge(Sorted, FirstSorted, SecondSorted)** asserts that the overall result, *Sorted*, is the result of merging *FirstSorted* and *SecondUnsorted*

Testing merge sort:

<pre>
| ?- <b>mergeSort(What, [11, 86, 2, 69, 22, 39, 85, 57, 78, 76]).</b>

What = [2,11,22,39,57,69,76,78,85,86] ? 
</pre>

Is this an algorithm?
---------------------

Again, with a declarative language, it's hard to say.

It is worth noting that the definition of our rules is pretty close to how we might define merge sort in an imperative language.
