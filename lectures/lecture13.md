---
layout: default
title: "Lecture 13: Map, filter, reduce, and higher-order functions"
---

# Intro

The true "zen" of functional programming is being able to compose functions in arbitrary ways in order to process collections of data.

The [map](http://clojuredocs.org/clojure.core/map), [mapv](http://clojuredocs.org/clojure.core/mapv), [filter](http://clojuredocs.org/clojure.core/filter), and [reduce](http://clojuredocs.org/clojure.core/reduce) functions are a powerful platform for using functions to process collections.

## Map (and mapv)

The `map` and `mapv` functions apply an arbitrary function to each value in a collection, returning the results of each application as a sequence.

Example:

    => (defn add1 [x] (+ x 1))
    
    => (map add1 [1 2 3])
    (2 3 4)
    
    => (mapv add1 [1 2 3])
    [2 3 4]

The main difference between `map` and `mapv` is that `map` returns a "lazy sequence", while `mapv` returns a vector.  A lazy sequence is more or less what it sounds like: a sequence where the values are not computed until they are needed.

You should consider using `map` or `mapv` any time you need to apply a transformation to each member of a collection and access the results as a sequence.

## Filter

The `filter` function applies a predicate (a function returning a boolean value) to each value in a collection, and returns a sequence containing just the values that matched the collection.

Example:

    => (defn is-multiple-of-3? [x] (= (mod x 3) 0))
    
    => (filter is-multiple-of-3? [1 2 3 4 5 6 7 8 9 10])
    (3 6 9)

You should consider using the `filter` function any time you need to select a subset of values from a collection.

## Reduce

The `reduce` function does a *reduction* on a collection of values.  There are two forms:

    (reduce f coll)
    
    (reduce f val coll)

The first form applies the function `f` to the first two values in `coll`, then applies `f` to that result and the third value in the collection, etc., until all values in the collection have been processed.

Example:

    => (reduce + [1 2 3 4 5])
    15

The second form is similar, but `f` is first applied to `val` and the first element of the collection, then `f` is applied to that result and the second element in the collection, etc.

Example:

    => (reduce + 0 [1 2 3 4 5])
    15

You should consider using `reduce` whenever you need to combine all of the values in a collection of data to produce a single result value.

## Higher-order functions

A *higher-order function* is a function which returns a function as a result.

One important use of higher-order functions is to produce a "family" of related functions on demand.  In the `filter` example above, we defined an `is-multiple-of-3?` function.  However, we might want to have predicate functions for other multiples.  Here is a `make-is-multiple-of` function that can generate any such predicate:

{% highlight clojure %}
(defn make-is-multiple-of [n]
  (fn [x]
    (= (mod x n) 0)))
{% endhighlight %}

Testing this function:

    => (filter (make-is-multiple-of 2) [1 2 3 4 5 6 7 8 9 10])
    (2 4 6 8 10)
    
    => (filter (make-is-multiple-of 3) [1 2 3 4 5 6 7 8 9 10])
    (3 6 9)

Note that in the example above, we didn't give a name to either of the functions returned by `make-is-multiple-of`.  However, if there is a particular variant that we want to refer to by name, we can give it one using `def`:

    => (def is-multiple-of-4? (make-is-multiple-of 4))
    #'lab10.core/is-multiple-of-4?
    
    => (filter is-multiple-of-4? [1 2 3 4 5 6 7 8 9 10])
    (4 8)

(Note that my namespace for this example was called `lab10.core`.)

Higher-order functions can be very powerful when used in conjunction with `map`, `filter`, and `reduce`.
