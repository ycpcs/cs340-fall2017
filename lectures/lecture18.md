---
layout: default
title: "Lecture 18: More Clojure macros"
---

# Quoting

A macro is essentially a function which translates a form into another form prior to evaluation.

We need to be careful how the result form is constructed.  In particular, we need to make sure that symbols (representing names of variables, functions, and syntactic forms) aren't evaluated.

Example: an "unless" macro, to be used as follows:

> (**unless** *cond* *if-false* *if-true*)

(Note that there is a built-in `unless` macro that works exactly this way.)

We might try to define it as

{% highlight clojure %}
(defmacro unless [cond if-false if-true]
  (list if cond if-true if-false))
{% endhighlight %}

This would translate **unless** forms into **if** forms.  Looks good, right?

If we try defining the macro this way, we get an error about the compiler being unable to resolve the symbol "if".  The problem is that the body of the macro is evaluated to produce the translated form, and the identifier "if" is treated as a variable reference.  To prevent the evaluation of "if" as a variable, we need to quote it:

{% highlight clojure %}
(defmacro unless [cond if-false if-true]
  (list 'if cond if-true if-false))
{% endhighlight %}

This works as intended:

    => (unless (< 4 3) "yep" "oh no")
    yep

A general observation about macros is that most of the time we have a specific idea about what we want the generated form to look like, and we just need to substitute in forms or values as necessary.  So, it would be nice to have the body of the macro be a "picture" of the generated form.  This capability is provided by *syntax quoting*.

Alternate version of unless:

{% highlight clojure %}
(defmacro unless [cond if-false if-true]
  `(if ~cond ~if-true ~if-false))
{% endhighlight %}

The idea is that the syntax quote ("<tt>\`</tt>") automatically quotes all symbols in its scope, and the "unquote" ("`~`") evaluates code to be substituted into the result.

If you have ever developed a web application using a template engine (such as Java Server Pages) to dynamically generate HTML, this is much the same idea, except to generate Clojure forms.
