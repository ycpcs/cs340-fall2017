---
layout: default
title: "Lab 20: Tail-recursive list merge in Erlang"
---

Getting Started
===============

Download [sort.erl](../lectures/sort.erl).

Start **erl** (Erlang interpreter) in the directory where you downloaded **sort.erl**. You can compile the code with the command

{% highlight erlang %}
c(sort).
{% endhighlight %}

You can test your **mergesort** function with the command

{% highlight erlang %}
sort:mergesort([11, 86, 2, 69, 22, 39, 85, 57, 78, 76]).
{% endhighlight %}

Your Task
=========

Reimplement the **merge** function, which merges two sorted lists, using *tail recursion*.

You will need to define a tail-recursive helper function with an accumulator parameter. For example, let's assume that your helper function will be called **mergehelp**. You could define your **merge** function this way:

{% highlight erlang %}
merge(Left, Right) -> mergehelp(Left, Right, []).
{% endhighlight %}

Because tail-recursive list processing builds a list starting with the *last* element, you will need to reverse the result of the merge before returning the completed result. You can use the built-in **lists:reverse** function to do this.

<!--
# Solution

Here is a solution: [Lab 20 solution](lab20soln.html)
-->
