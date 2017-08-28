---
layout: default
title: "Lab 15: Clojure macros"
---

# Your task

Create Clojure macros are described below.

## `applyn`

This macro should take three arguments: a function, a value, and a *n* (an integer count).  It should return a form which will apply the function to *n* copies of the value.

Example use:

    => (applyn str "HA" 5)
    HAHAHAHAHA
    => (defn splot [] (println "Splot!") 3)
    #'cs340-lab15.core/splot
    => (applyn + (splot) 3)
    Splot!
    Splot!
    Splot!
    9

Note that the way that `applyn` works is subtly different than how a function works.  If `applyn` were a function, each argument would be evaluated exactly once.  As you can see from the second example above, the *val* argument is evaluated *n* times.

Suggestion: you can use the built-in `repeat` function to generate a sequence with multiple copies of a specified value.  E.g.,

{% highlight clojure %}
(repeat n val)
{% endhighlight %}

produces a sequence with *n* copies of *val*.  Also note that using `conj` to add an element to a sequence returned by `repeat` does a prepend, as though the sequence were a list.

## `unless`

This macro is similar to the **if** special form.  Its syntax is

> (**unless** *cond* *if-false* *if-true*)

First, *cond* should be evaluated.  If *cond* yields a false value, *if-false* should be evaluated and returned.  Otherwise, *if-true* should be evaluated and returned.

Note that only one of *if-false* and *if-true* should be evaluated, not both.

Hint: generate an **if** form as a result of the macro.

Example use:

    => (unless (< 5 4) "yip" "yap")
    yip
    => (unless (< 4 5) "yip" "yap")
    yap
    => (defn ned [] (println "I'm Ned!") 44)
    #'cs340-lab15.core/ned
    => (defn ted [] (println "I'm Ted!") 55)
    #'cs340-lab15.core/ted
    => (unless (< 5 4) (ned) (ted))
    I'm Ned!
    44
    => (unless (< 4 5) (ned) (ted))
    I'm Ted!
    55
