---
layout: default
title: "Lab 10: Can I take your order?"
---

# Your task

## First part

Write a function called `tally-item` as follows.  The function should take two parameters, `prices` and `line-item`.

`line-item` is two-element vector representing an item on an invoice:

* the first member is a keyword value representing an item
* the second member is an integer quantity

 `prices` is a map of items (keyword values) to the price for a single quantity of that item.

The function should return the cost of the line item, meaning the price of the specified item multiplied by the quantity.

You can test your `tally-item` function using the following example prices map:

{% highlight clojure %}
(def fruit-prices
  {:apple 0.75,
   :orange 0.80,
   :pomegranate 2.50,
   :banana 0.50,
   :plum 1.20,
   :peach 1.00,
   :persimmon 1.75,
   :lime 0.60})
{% endhighlight %}

Here is part of a REPL session showing invocations of `tally-item` using the `fruit-prices` map.  Try these and make sure your function produces the same results:

    user=> (tally-item fruit-prices [:persimmon 2])
    3.5
    user=> (tally-item fruit-prices [:orange 3])
    2.4000000000000004
    user=> (tally-item fruit-prices [:peach 1])
    1.0
    user=> (tally-item fruit-prices [:banana 0])
    0.0

(Note that the strange result for three oranges is due to floating-point imprecision.  In production software, it's a bad idea to use floating point values to represent exact quantities such as money.)

## Second part

An "invoice" is a sequence of line items.  For example:

{% highlight clojure %}
(def yummy-fruit
  [[:persimmon 2]
   [:orange 3]
   [:peach 1]
   [:plum 10]
   [:pomegranate 5]])
{% endhighlight %}

It would be nice to have a way to apply your `tally-item` function to an invoice to compute a total price for all of the line items.

Here's how to do it!  Well, sort of.

The built-in `map` function applies a one-parameter function to each item in a sequence, returning a sequence with the results of each application.  (Note that there are more complicated ways to use `map`, but we won't get into that right now.)  We have a bit of a problem, though: `tally-item` expects two parameters, a prices map and a line item vector.

Not to worry: we can create an *anonymous* function that takes a single parameter (a line item vector) and applies `tally-item` to it, passing whichever prices map we want to use.  (You encountered anonymous functions in the [Intro to Functions](https://www.4clojure.com/problem/14) problem on [4clojure](https://www.4clojure.com/).)

Try this (which assumes you've evaluated the `yummy-fruit` invoice shown above):

{% highlight clojure %}
(map (fn [line-item] (tally-item fruit-prices line-item))
     yummy-fruit)
{% endhighlight %}

What happens when you do this?  (Ok, it's not really computing a total price, but perhaps it's doing something that might be useful for computing the total price?)

## Third part

Come up with a way to use the `map` function (as shown above) to compute a single price for an invoice.  Hint: research the built-in `reduce` function.

<!--
When you get this working, show me what you did.

# Solution

When you are done, you can compare your solution to mine:

> [lab10-solution.clj](lab10-solution.clj)
-->

<!-- vim:set wrap: ­-->
<!-- vim:set linebreak: -->
<!-- vim:set nolist: -->
