---
layout: default
title: "Lecture 11: Clojure Data Structures"
---

# Clojure data structures

Clojure has an extremely powerful set of built-in functional data structures, also called *collections*.

They are "functional" because they do not offer any operations that mutate an existing data structure.  Instead, the operations which "modify" the data structure create a new instance of the data structure, based on the original instance.

## Operations

There are many operations supported by all of the built-in data structures. Some examples:

`empty?`: determine whether a collection is empty.

`count`: get the number of elements in a collection.

`contains?`: check whether a collection contains a specified value.

In general, Google and [clojuredocs.org](https://clojuredocs.org/) are your friends when you have questions about how to use the Clojure collection types.

## Sequences

Lists and vectors are the two main sequence data structures.

Lists are sequential access (O(n) to access the nth element).  Vectors are random access (*sort of* O(1) access to arbitrary elements.)

`first` and `rest` are the standard functions to recursively process a sequence, yielding the first element and a sequence with all but the first element, respectively.

The `nth` function retrieves the element of a sequence at the specified index.  Note that this is O(n) for lists.

The `conj` function prepends (lists) or appends (vectors) a new element onto an existing sequence.

## Sets

A set is an unordered collection of elements, with no duplicates allowed.  `contains?` can be expected to be a fast operation.

There is a literal syntax for sets:

{% highlight clojure %}
; this is a set with three elements
#{ :a, :b, :c }
{% endhighlight %}

Note that the commas are optional in the literal syntax.

Note that `first` and `rest` can be used with sets, but you will see the values in an unpredictable order.

You can use `conj` to add an element to a set.

You can use `disj` to remove an element from a set.

## Maps

Clojure maps are similar to maps in Java, but they are functional.

Literal syntax:

{% highlight clojure %}
{ :a 1, :b 2, :c 3 }
{% endhighlight %}

As with sets, the commas are optional.

The `assoc` function updates a map by changing or adding a key/value association.

The `dissoc` function updates a map by removing a key/value association.

The `contains?` function can be used to check to see whether a map has a particular key.

The `get` function retrieves the value associated with a specified key.
