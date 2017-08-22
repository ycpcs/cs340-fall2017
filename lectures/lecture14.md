---
layout: default
title: "Lecture 14: Recursion and iteration"
---

Like most functional programming languages, Clojure emphasizes the use of recursion for iterated computation.

# Basic recursion on sequences

Here are some examples of recursion on sequences in Clojure:

> [Recursion on sequences](recursion-examples.html)

These illustrate how to build a sequence recursively, including how to build a sequence using tail recursion.

# The `cond` form

The `cond` form can be useful for handling a series of cases.  The syntax is

> (**cond** *cond1* *expr1* *cond2* *expr2* ... `:else` *else-expr*) 

The idea is that the conditions (*cond1*, *cond2*, etc.) are checked in order.  When an expression evaluates as "truthy" (any value other than `false` or `nil`), its corresponding expression is evaluated and its result is the overall result of the `cond` form.  This is somewhat similar to a `switch` statement in C or Java, but it is an expression which produces a value.

Note that `:else` is used by convention for the last expression (to serve as a "catch all" if none of the other expressions are true), but any truthy value would also work.

If there is no catch all expression, and if none of the expressions is true, the result is `nil`.

# Recursion in Clojure

[This section is a more in-depth illustration of recursion in Clojure.]

Example, merging two sorted lists:

{% highlight clojure %}
(defn my-merge [left right]
  (cond
    (empty? left) right
    (empty? right) left
    (< (first left) (first right)) (cons (first left) (my-merge (rest left) right))
    :else (cons (first right) (my-merge left (rest right)))))
{% endhighlight %}

(Note that the `cond` form evaluates a series of conditions and returns a specified value corresponding to the first condition evaluating as true.  It is similar to a chain of if/else statements in Java.)

Example run:

    user=> (def list1 '(1 5 9 33 50 97))
    #'user/list1
    user=> (def list2 '(3 6 28 44 67 68 99))
    #'user/list2
    user=> (my-merge list1 list2)
    (1 3 5 6 9 28 33 44 50 67 68 97 99)

Issue: each function call in Clojure creates a new stack frame.  So, a deep recursion could exhaust the stack (leading to a `StackOverflowError`).

Solution: use the `recur` form for recursive calls in tail position.  A call is in tail position if the result of the call is returned as the result of the overall function.  Note that `my-merge` does not have its recursive calls in tail position, because a call to `cons` is required after each recursive call to `my-merge` returns.  By adding a helper function with an  *accumulator parameter*, we rewrite the function using `recur`:

{% highlight clojure %}
(defn my-merge-work [left right accum]
  (cond
    (and (empty? left) (empty? right)) (reverse accum)
    (empty? left) (recur left (rest right) (cons (first right) accum))
    (empty? right) (recur right (rest left) (cons (first left) accum))
    (< (first left) (first right)) (recur (rest left) right (cons (first left) accum))
    :else (recur left (rest right) (cons (first right) accum))))

(defn my-merge [left right]
  (my-merge-work left right '()))
{% endhighlight %}

Note: because the `accum` parameter holds the partial result of the computation, the cases where one of the lists (`left` or `right`) has become empty are slightly more complicated.

**Question**: Why is it necessary to reverse the accumulator in the case when both the left and right lists are empty?

Clojure defines the `loop` form as a compact alternative to creating an explicit helper function.

{% highlight clojure %}
(defn my-merge [left right]
  (loop [ll left
         rr right
         accum '()]
    (cond
      (and (empty? ll) (empty? rr)) (reverse accum)
      (empty? ll) (recur ll (rest rr) (cons (first rr) accum))
      (empty? rr) (recur rr (rest ll) (cons (first ll) accum))
      (< (first ll) (first rr)) (recur (rest ll) rr (cons (first ll) accum))
      :else (recur ll (rest rr) (cons (first rr) accum)))))
{% endhighlight %}

`loop` is a bit like `let` since it defines some variables.  The important difference is that when `recur` is used, it *updates* the values of these variables and reevaluates the body.  It doesn't really change the values of the variables, since the reevaluation of the body is effectively a recursive call which introduces a new scope.  However, behind the scenes, the Clojure compiler actually does compile the `loop` form into an actual loop!

<!-- vim:set wrap: ­-->
<!-- vim:set linebreak: -->
<!-- vim:set nolist: -->
