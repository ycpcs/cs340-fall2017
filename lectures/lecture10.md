---
layout: default
title: "Lecture 10: Why Clojure?"
---

# Clojure

[Clojure](http://clojure.org/) is a functional language that runs on the Java Virtual Machine.  It is a dialect of LISP.  It was designed and implemented by [Rich Hickey](https://twitter.com/richhickey).

Why learn Clojure?

* Functional programming: no side-effects, no assignments, no mutable data
* Concise syntax
* Powerful functional data structures: lists, vectors, sets, maps
* Anonymous functions (also called *closures*)
* Higher-order functions (functions can return functions)
* Powerful tools for *composing* functions: map, reduce, filter
* Interoperability with Java (any Java class/method can be used from Clojure code)
* Extremely rich literal syntax for structured data
* Homoiconicity: code is data
* Macros (code that generates or transforms code: a program add new syntactic forms to the language)
* Interactive, incremental development (the REPL, read-eval-print loop)

Recommended reading: [Beating the Averages](http://www.paulgraham.com/avg.html) by [Paul Graham](http://www.paulgraham.com/): talks about Common LISP, but everything he says about LISP is true of Clojure.

Data types:

* Integers, floating point numbers
* Keywords (**:a**, **:hello**): often used as opaque identifiers
* Strings (which are actually Java String objects)
* Lists (**'(1 2 3)**, **'(:a :b :c)**)
* Vectors (**[1 2 3]**, **[:a :b :c]**)
* Sequences (an abstract generalization of lists, vectors, and any other data structure that represents a sequence of values: many built-in functions operate on sequences such as **conj**, **first**, **rest**)
* Sets (**#{:a :b :c}**)
* Maps (**{:a 1, :b 2, :c 3}**)

## Forms

Because it is based on LISP, Clojure's syntax is very different from most programming languages.

There are only a small number of *syntactic forms* in Clojure.  All of them are a pair of parentheses with some stuff inside.

The most important syntactic form is the function application:

> (*function* *args...*)

This form applies a function to zero or more arguments, and yields a value (the result returned from the function.)

Unlike most programming languages, Clojure does not have infix operators.  All computations, including arithmetic, are the result of function applications.  For example, the Clojure code to compute the sum of 2 and 3 is

{% highlight clojure %}
(+ 2 3)
{% endhighlight %}

## The REPL

Interacting with Clojure is often done through the Clojure REPL: the Read/Eval/Print Loop.  The idea is simple: you enter Clojure forms, and the REPL (and the Clojure compiler and runtime) evaluate them and print the result.

There are lots of ways to use the REPL.  One of the easiest is the `lein repl` command.  Here is a transcript of me running it:

    [dhovemey@nutt]$ lein repl
    nREPL server started on port 39853 on host 127.0.0.1 - nrepl://127.0.0.1:39853
    REPL-y 0.3.7, nREPL 0.2.12
    Clojure 1.8.0
    Java HotSpot(TM) 64-Bit Server VM 1.8.0_91-b14
        Docs: (doc function-name-here)
              (find-doc "part-of-name-here")
      Source: (source function-name-here)
     Javadoc: (javadoc java-object-or-class-here)
        Exit: Control+D or (exit) or (quit)
     Results: Stored in vars *1, *2, *3, an exception in *e
    
    user=> (+ 2 3)
    5
    user=> 

At the `user=>` prompt you can type a Clojure form, and then see the result of the evaluation.

## Functions

The `defn` form defines a function:

> (**defn** *name* [*params*] *body*)

Example:

{% highlight clojure %}
(defn sum [a b]
  (+ a b))
{% endhighlight %}

Note that because Clojure is a dynamically-typed language, we don't need to declare data types for the parameters `a` and `b`.

You can create functions in a REPL and call them:

    [dhovemey@nutt]$ lein repl
    nREPL server started on port 44580 on host 127.0.0.1 - nrepl://127.0.0.1:44580
    REPL-y 0.3.7, nREPL 0.2.12
    Clojure 1.8.0
    Java HotSpot(TM) 64-Bit Server VM 1.8.0_91-b14
        Docs: (doc function-name-here)
              (find-doc "part-of-name-here")
      Source: (source function-name-here)
     Javadoc: (javadoc java-object-or-class-here)
        Exit: Control+D or (exit) or (quit)
     Results: Stored in vars *1, *2, *3, an exception in *e
    
    user=> (defn sum [a b] (+ a b))
    #'user/sum
    user=> (sum 2 3)
    5
    user=> 

This is a very nice way to develop programs in Clojure: create functions on the fly and test them interactively.
