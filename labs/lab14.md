---
layout: default
title: "Lab 14: Recursive List Processing in Prolog"
---

# Your task

Define Prolog inference rules called **listSum** such that a query of the form

<pre>
<b>listSum</b>(<i>List</i>, <i>Sum</i>).
</pre>

asserts that the sum of the elements of *List* is *Sum*.  For example, the query

{% highlight prolog %}
listSum([8, 0, 18, 15, 5], X).
{% endhighlight %}

should result in the query succeeding with the binding of 46 to *X*.

## Hints

Think about an appropriate base case, and define an inference rule to handle the base case.

You can use the **is** keyword to make an assertion about numeric equality, e.g., the query

{% highlight prolog %}
X is 4 + 5.
{% endhighlight %}

succeeds with the binding of 9 to *X*.

You can use the [*First*\|*Rest*] syntax to break up a non-empty list into a first element and remaining elements list.

Note: Prolog is sensitive to the order in which the conjuncts in the hypothesis appear.  If your rule doesn't work, try reordering the clauses in the hypothesis of the recursive rule.
