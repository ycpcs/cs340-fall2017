---
layout: default
title: "Implementing Control Flow"
---

This document discusses how to implement control flow (**if** and **while** statements) as extra credit for [Assignment 5](assign08.html).

<div class="callout"><b>Important</b>: Don't work on the extra credit unless the standard features of your code generator are completely working.</div>

# Evaluating comparison operators

You may make the following simplifying assumptions about the comparison operators:

* The condition of an **if** or **while** statement is guaranteed to be an expression involving one of the comparison operators
* No comparison operator will ever be used to compute a value

So, you can assume that you will never see constructs such as the following:

<pre>
if (a + b) {
    <i>stuff</i>
}

x := c < d;
</pre>

In the first example above, the result of the + operator is used as a condition.  The the second example, the result of the &lt; operator is assigned to a variable.

These assumptions allow us to take a simple approach to generating code for the comparison operators: the generated code for a comparison operator should jump to a location in the generated code if the result of the comparison is true, and should fall through to the next instruction if the result of the comparison is false.

Your generated code for a comparison operator should look something like the following:

<pre>
<i>generated code for left-hand side of comparison</i>
<i>generated code for right-hand side of comparison</i>
cmp
if<i>cond</i> <i>label_if_true</i>
</pre>

In the code above, <i>cond</i> should be filled in appropriately based on which MiniVM conditional branch instruction implements the desired comparison, e.g., `lte` if the comparison operator is &lt;=.  <i>label\_if\_true</i> is a label specifying the location of the code to be executed if the result of the comparison is true.

## Generating code for control flow constructs

When your code generator needs to generate code for a control-flow construct (i.e., **if** or **while**), it should generate a label representing where in the generated code the condition should branch to if the comparison evaluates as true.

You can generate a label with the following function:

{% highlight clojure %}
(defn new-label []
  (str (gensym)))
{% endhighlight %}

Here is an example sketch of how your code generator might generate code for the **if** statement.

* Create a label
* Generate the code for the **if** statement's condition (which you can assume will be a comparison), having it jump to the generated label if the condition is false
* Generate the code for the body of the **if** statement (this code is reached if the condition evaluated as true)
* Generate the label (which the comparison will jump to if the result of the comparison is false, and which will be reached implicitly when the instructions generated for the body of the if statement complete)

One of the main issues you will need to consider is how the code generated for the condition will know where to jump to.  You should add a second parameter to the `generate-code` function, which is the label where code generated for a comparison should jump if the condition is true.  I.e., change

{% highlight clojure %}
(defn generate-code [aast]
  (case (:symbol aast)
    ...
{% endhighlight %}

to

{% highlight clojure %}
(defn generate-code [aast ontrue]
  (case (:symbol aast)
    ...
{% endhighlight %}

Note that the only time you will need an actual label as the value of the `ontrue` parameter is when you are generating code for a condition in an **if** or **while** statement.  For all other constructs, you can just pass an empty string.

Another interesting issue: you will notice that in the steps mentioned above for generating code for an **if** statement, it mentions jumping to a label if the condition is *false*.  This is not a typo: it is generally easier to generate code for an **if** statement if the branch is taken when the condition is false rather than true.  You may find it useful for "invert" a condition: for example, rather than jumping to a label if the left hand value is &lt;= the right hand value, jump to a label if the left hand value is &gt; the right hand value.  You can invert a comparison with the following `invert-comparison` function:

{% highlight clojure %}
(def inverted-comparison-ops
  {:op_je :op_jne,
   :op_jne :op_je,
   :op_lt :op_gte,
   :op_gte :op_lt,
   :op_lte :op_gt,
   :op_gt :op_lte})

(defn invert-comparison [aast]
  ;(println "; Inverting comparison " (:symbol aast))
  (if (not (contains? inverted-comparison-ops (:symbol aast)))
    (throw (RuntimeException. (str (:symbol aast) " expression is not a condition?")))
    (node/make-node-with-props (get inverted-comparison-ops (:symbol aast)) (:value aast) (:props aast))))
{% endhighlight %}

So, the idea is that you should generate code for the *inverted* condition rather than the actual condition, and pass it a label that the code should jump to if the inverted condition is true (meaning than the original condition was false).

# Testing, examples

Here are some example programs you can try.  Note that Clojure allows string constants to span multiple lines, e.g.:

{% highlight clojure %}
(def testprog
"var a;
a := 42;
if (a > 10) {
    a := a * 2;
}
a;")
{% endhighlight %}

Multiline strings will make your test programs a bit easier to write and read.

Example program:

    var a;
    var b;
    a := 4;
    b := 6;
    if (a < 10) {
        b := 7;
    }
    b;

This program should generate code similar to the following:

    main:
        enter 0, 2
        ldc_i 4
        dup
        stlocal 0
        pop
        ldc_i 6
        dup
        stlocal 1
        pop
        ldlocal 0
        ldc_i 10
        cmp
        jgte G__1914
        ldc_i 7
        dup
        stlocal 1
        pop
    G__1914:
        ldlocal 1
        syscall $println
        pop
        ldc_i 0
        ret

Another example program:

    var max;
    var count;
    var sum;
    max := 100;
    count := 1;
    sum := 0;
    while (count <= max) {
        var t;
        t := sum + count;
        sum := t;
        count := count + 1;
    }
    sum;

This program should generate code similar to the following:

    main:
        enter 0, 4
        ldc_i 100
        dup
        stlocal 0
        pop
        ldc_i 1
        dup
        stlocal 1
        pop
        ldc_i 0
        dup
        stlocal 2
        pop
    G__1941:
        ldlocal 1
        ldlocal 0
        cmp
        jgt G__1942
        ldlocal 2
        ldlocal 1
        add
        dup
        stlocal 3
        pop
        ldlocal 3
        dup
        stlocal 2
        pop
        ldlocal 1
        ldc_i 1
        add
        dup
        stlocal 1
        pop
        jmp G__1941
    G__1942:
        ldlocal 2
        syscall $println
        pop
        ldc_i 0
        ret

Note that this program computes the sum of the integers from 1 to 100: is that cool, or what?
