---
layout: default
title: "Lecture 17: Clojure reader and evaluator"
---

# Code = data

You may have noticed that Clojure code and literal Clojure data structures look quite similar.  All function applications and special forms are sequences of items in parentheses, which look a lot like literal lists.  Parameter lists and variable definitions in the **let** and **loop** forms look a lot like vectors.  And so on.

This is not a coincedence.  The Clojure language has a property called *homoiconicity*, which simply means that code and data have a common representation.

## Why this is useful

Because Clojure code is represented using Clojure data structures, it is possible to transform and generate code at runtime.  What's more, Clojure can *evaluate* data structures as code.  This raises the possibility of implementing new language features by simply writing Clojure code.

First we will need to look at how Clojure code is read and evaluated.

# The reader

The *reader* is the component of Clojure responsible for translating textual representations of Clojure code and literal data structures (remember that due to homoiconicity, these are the same!) into actual (run-time) data structures.  In other words, it is the component which turns text such as

{% highlight clojure %}
(+ 2 3)
{% endhighlight %}

into a list where the symbol **+** is the first item, the number 2 is the second item, and the number 3 is the third item.

The built-in `read-string` function applies the reader to a string and returns the resulting Clojure data structure.  E.g.:

    user=> (read-string "(if 1 2 3)")
    (if 1 2 3)
    user=> (read-string "123")
    123
    user=> (read-string "[1 2 3]")
    [1 2 3]

# The evaluator

The evaluator takes a Clojure data structure representing a form, and evaluates it.  You can invoke the evaluator directly using the **eval** function.  E.g.:

    user=> (eval (read-string "(+ 1 2)"))
    3
    user=> (eval '(+ 1 2))
    3
    user=> (eval (list '+ 1 2))
    3

In each case, we are evaluating a list with three elements &mdash; the symbol **+**, the number 1, and the number 2 &mdash; and evaluating it.  Since this data structure is the representation of a function application, evaluating it

1. Looks up the function associated with the name "+" (the built-in addition function)
2. Applies the function to the evaluated form of the arguments
3. Returns the result of the function

## What does the evaluator do?

How the evaluator evaluates a form data structure depends on what kind it is.

Some forms self-evaluate.  For example:

* Numbers
* Strings
* Keyword values
* The empty list

A vector is evaluated by constructing a vector whose members are the results of evaluating the members of the original vector.  E.g.:

    user=> (eval ['(+ 1 2) '(* 3 5)])
    [3 15]

A non-empty list evaluates either as a special form or a function evaluation, depending on what the first member of the list is.  E.g.:

    user=> (eval '(if (> 3 4) "boo" "yah"))
    "yah"
    user=> (eval '(conj [:a :b] :c))
    [:a :b :c]

Symbols are one of the most interesting forms to evaluate: they represent a variable lookup.  E.g.:

    user=> (def lunch "beans and rice")
    #'user/lunch
    user=> (eval 'lunch)
    "beans and rice"

## Preventing evaluation

Sometimes it's important to *prevent* a form from being evaluated.  With self-evaluating forms such as numbers and keyword values, there's no need to prevent evaluation since the result of evaluation is the same as the original form.  However, it is useful to prevent the evaluation of lists and symbols.  A list is usually evaluated as a special form or a function application, so if you want a literal list, you need to prevent evaluation.  Similarly, a symbol is usually evaluated as a variable lookup, so if you want a literal symbol, again, you need to prevent evaluation.

The good news is that Clojure makes it really simple to prevent evaluation through *quoting*:

    user=> (+ 2 3)
    5
    user=> '(+ 2 3)
    (+ 2 3)
    user=> +
    #object[clojure.core$_PLUS_ 0x2bc19629 "clojure.core$_PLUS_@2bc19629"]
    user=> '+
    +

What we're seeing here is:

1. The text `(+ 2 3)`, which the reader transforms into a list, is evaluated as a function application
2. In the text `'(+ 2 3)`, the reader interprets the quote (`'`) character as meaning "prevent the evaluation of the next form", which produces a literal list as a result
3. The text `+`, which the reader transforms into a symbol, is evaluated as a variable lookup, and by default the name "`+`" refers to the built-in "plus" function
4. In the text `'+`, the quote prevents evaluation, yielding the literal symbol "`+`"

So, now you know how quoting works and why it's necessary.

Note that the single quote ("`'`") is a *reader macro* that converts the next expression *expr* into the special form

> (**quote** *expr*)

This special form tells the evaluator, "Hey evaluator, I know you would really like to evaluate *expr*, but don't do it, OK?  Just return it as a literal value."

You can use this more verbose way of quoting directly if you want to:

    user=> (quote (+ 1 2))
    (+ 1 2)
    user=> (quote +)
    +

# Macros

So, the way Clojure works is that the reader turns code into data, and the evaluator carries out the computation embodied by the data.

What if we could intervene in the process by changing the data produced by the reader before it goes on to the evaluator?  Then we could change the language itself.

*Macros* offer precisely this capability.  A macro is a function which transforms a "raw" form as produced by the reader.

## Example

Let's say we're having trouble dealing with the fact that Clojure uses prefix syntax for function applications, including applications of arithmetic operators.  We can write a macro to allow us to use infix notation!

{% highlight clojure %}
(defmacro infix [left op right]
  (list op left right))
{% endhighlight %}

Example use:

    user=> (infix 2 + 3)
    5

One issue is that this macro does not recursively translate subexpressions from infix form to prefix form.  Better version:

{% highlight clojure %}
(defn from-infix [e]
  (if (not (sequential? e))
    e
    (let [n (count e)]
      (case n
        1 (from-infix (first e))
        3 (let [[left op right] e]
            (list op (from-infix left) (from-infix right)))
        (throw (RuntimeException. "infix expression must have 1 or 3 members"))))))

(defmacro infix [& expr]
  (from-infix expr))
{% endhighlight %}

Example use:

    user=> (infix 2)
    2
    user=> (infix 2 + 3)
    5
    user=> (infix 2 * ((3) + 5))
    16

Some explanation:

* the `sequential?` function returns true if its argument is a sequence (such as list)
* the `case` form tests a value against a series of possibilities, returning a result expression on match: the `from-infix` function uses it to check whether the sequence containing an infix expression has 1 or 3 members
* in the `infix` macro, the syntax `[& expr]` allows the macro to take any number of arguments, causing `expr` to be a sequence containing the arguments

We just changed the language!  This example is somewhat frivolous, but macros can be tremendously powerful when applied thoughtfully.  With macros, you never need to wish that your programming language had a construct that would make your life easier.  You can just *add* the constructs you need.

<!-- vim:set wrap: ­-->
<!-- vim:set linebreak: -->
<!-- vim:set nolist: -->
