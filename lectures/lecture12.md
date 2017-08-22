---
layout: default
title: "Lecture 12: Clojure functions"
---

# Dealing with data

When writing functions in Clojure, the first step is extracting the data you need in order to do the required computation.

Here are two techniques that are helpful.

## The `let` form

The `let` form can be used to define new variables.  This is extremely useful because it allows you to assign meaningful names to values used in the computation.

The `let` form is

> (**let** [*var1* *val1* *var2* *val2* ...] *body*)

Each value (*val1*, *val2*, etc.) is evaluated in order, and the results are assigned to the variables *var1*, *var2*, etc.  The result computed by the `let` form is the result of evaluating the *body* expression, which can refer to the variables.

For example, let's say that we want to write a function to compute the geometric distance between two points, where each point is represented by a two-element vector.  E.g.:

{% highlight clojure %}
(geom-dist [0 0] [3 4])
; => 5.0
{% endhighlight %}


Here's one possible implementation:

{% highlight clojure %}
(defn geom-dist [p1 p2]
  (Math/sqrt (+ (* (- (first p2) (first p1)) (- (first p2) (first p1)))
                (* (- (second p2) (second p1)) (- (second p2) (second p1))))))
{% endhighlight %}

This is pretty ugly because there is a lot of redundancy.  Here's a better version using the `let` form:

{% highlight clojure %}
(defn geom-dist [p1 p2]
  (let [x1 (first p1)
        y1 (second p1)
        x2 (first p2)
        y2 (second p2)
        xdiff (- x2 x1)
        ydiff (- y2 y1)]
    (Math/sqrt (+ (* xdiff xdiff) (* ydiff ydiff)))))
{% endhighlight %}

In addition to eliminating the redundancy, the variables make the code more readable by giving meaningful names to values used in the computation.

## Destructuring

*Destructuring* allows you to automatically extract data from a sequence or map argument.

For example, the `geom-dist` function defined earlier accesses elements of two parameter vectors using the built-in `first` and `second` functions.  However, since both of the parameter vectors are expected to have exactly two members, we can destructure them into their members directly:

{% highlight clojure %}
(defn geom-dist [[x1 y1] [x2 y2]]
  (let [xdiff (- x2 x1)
        ydiff (- y2 y1)]
    (Math/sqrt (+ (* xdiff xdiff) (* ydiff ydiff)))))
{% endhighlight %}

This is even simpler than the previous version, and arguably more readable as well.

The `let` form also supports destructuring:

{% highlight clojure %}
(defn geom-dist [p1 p2]
  (let [[x1 y1] p1
        [x2 y2] p2
        xdiff (- x2 x1)
        ydiff (- y2 y1)]
    (Math/sqrt (+ (* xdiff xdiff) (* ydiff ydiff)))))
{% endhighlight %}

This isn't quite as concise as the version above where the parameters are destructured directly, but it's still an improvement over making explicit calls to the `first` and `second` functions.

It is also possible to destructure maps.  For example:

{% highlight clojure %}
(def thing1 {:color "blue", :texture "fuzzy", :size 40})

(def thing2 {:color "yellow", :texture "scaly", :size 120})

(defn thingy-size [thingy]
  (let [{sz :size} thingy]
    (if (> sz 100)
      "large"
      "small")))

(thingy-size thing1)
;  => "small"

(thingy-size thing2)
;  => "large"
{% endhighlight %}

<!-- vim:set wrap: ­-->
<!-- vim:set linebreak: -->
<!-- vim:set nolist: -->
