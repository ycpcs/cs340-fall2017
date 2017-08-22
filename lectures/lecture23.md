---
layout: default
title: "Lecture 23: Concurrency in Clojure"
---

Concurrency in Clojure
======================

Clojure supports several mechanisms for concurrency. The important thing to realize is that *none* of them involve

-   unsynchronized access to mutable shared state, or
-   explicit locking

Eliminating the possibility of an important class of errors such as data races and deadlocks. This doesn't mean that it is necessarily *easy* to express correct concurrent algorithms in Clojure, but there is more safety than in languages such as Java.

Because Clojure is a "mostly-functional" language, it is possible to write programs that use concurrency but do not use *any* mutable state. For example, many of the important built-in data structures (such as vectors, lists, and maps) are *immutable*: "modification" to such data structures produces a new data structure rather than destructively modifying the previous one. This can be very efficient, because most or all of the previous data structure can be re-used as-is rather than copying it.

Futures
-------

A *future* is one of the simplest forms of concurrency: it represents a "future result": one whose evaluation may be in progress, but which will be known at some future time.

A future is created using the **future** special form, which takes an expression and starts evaluating the expression concurrently. Simple example:

The **@** construct forces completion of the future: if the future's result is not available yet, it will wait for completion.

<pre>
user=&gt; <b>(def a (future (+ 2 3)))</b>
#'user/a
user=&gt; <b>a</b>
#&lt;core$future_call$reify__6267@4f230afa: 5&gt;
user=&gt; <b>@a</b>
5
</pre>

pcalls and pmap
---------------

The **pcalls** and **pmap** functions invoke functions in parallel and builds a list of results.

**pmap** invokes a single function (in parallel) on each element of a list (or other sequence):

<pre>
user=> <b>(pmap (fn [x] (* x 2)) '(1 2 3 4 5))</b>
(2 4 6 8 10)
</pre>

**pmap** works more or less the same as **map**, but if the computation being performed for each element of the sequence is expensive, could allow parallelism.

**pcalls** invokes an arbitrary series of 0-argument functions in parallel and builds a list containing the results. (We'll see a use of **pcalls** in the next section.)

Software Transactional Memory
-----------------------------

For some concurrent computations, you may want to use shared mutable state. Clojure makes it relatively easy to express these kinds of computations through *software transactional memory*. The idea is that the state that can change is expressed as *refs*. A ref is a "box" that holds a value, but the value in the box can be changed at any time.

The value of a ref can only be changed within a *transaction*. A transaction may read the values of refs as well as modify the values of refs. When the end of a transaction is reached, its results are *committed* only if the values of the refs have not been changed by another transaction. This means that the effects (modifications to shared data) of a transaction either take effect completely, or not at all. No explicit locking or synchronization by the program is required.

Example: map coloring. Given a map --- for example, a map of the US --- assign colors to the geographical regions (e.g. states) such that neighboring regions never have the same color.

A simple way to find a map coloring is to start by assigning all regions the same color, and then, for each region, looking at neighboring regions and attempting to find a color that is not used by any neighboring region.

Here is a program that uses the **pcalls** function to attempt to find a legal color for each US state, based on the [adjacency lists for each state](http://writeonly.wordpress.com/2009/03/20/adjacency-list-of-states-of-the-united-states-us/):

> [statecolors.clj](statecolors.clj)

The **find-state-colors** function uses **pcalls** to start a worker function for each state. Each worker repeatedly executes a transaction which

-   checks the colors of the neighbors
-   if possible, updates the state's color to be a color not used by any of its neighbors

The **state-colors** vector contains one ref for each state, where the value of each ref is initially set to red:

{% highlight clojure %}
(def state-colors
  (vec (repeatedly (count state-adjacency-list) (fn [] (ref 'red)))))
{% endhighlight %}

Here is the **worker** function, which attempts to find a color for a given state (the state is identified by an index number):

{% highlight clojure %}
; Worker function to try computing a color for a state
; by examining the colors of adjacent states and (if possible)
; picking a color that is not used by the neighboring states.
(defn worker [index count maxiters ok]
  (if (= count maxiters)
    ; Reached the end of the computation:
    ; return the final color, along with boolean indicating whether
    ; the final color is legal (as far as we can tell)
    (list (deref (nth state-colors index)) ok)
    ; In a transaction, attempt to find a color for the state.
    (do
      ; The found-legal-color variable will be set to the
      ; result of the transaction: true if we found a
      ; legal color for the state, false if not.
      ; This value is sent into the next recursive call
      ; so we always have an idea of whether or not this
      ; worker was able to find a legal color for its state.
      (let [found-legal-color
             ; Start a transaction.
             (dosync
               ; Find state's current color and the colors of
               ; of its neighbors.
               (let [my-color (deref (nth state-colors index))
                     neighbor-colors (get-neighbor-colors index)]
                 (if (contains? neighbor-colors my-color)
                   ; The state is using the same color as one of
                   ; its neighbors, so choose a new color
                   ; by setting the state's ref to a new color.
                   ; Evaluate to true or false depending on
                   ; whether the new color is different from
                   ; its neighbors' colors.
                   (let [new-color (choose-other-color neighbor-colors my-color)]
                     (do (ref-set (nth state-colors index) new-color)
                         (not (contains? neighbor-colors new-color))))
                   ; Current color is ok
                   true)))]
      ; Continue recursively.
      (recur index (+ count 1) maxiters found-legal-color)))))
{% endhighlight %}

The interesting part is the **dosync** which executes the examination of the neighbors' colors and updates the state's color in a transaction. The result of the overall call to the worker function is a list with two elements: the first is the state's final color, and the second is a boolean which indicates whether or not a legal color was found for the state.

The **find-state-colors** function invokes the worker function in parallel, once for each state:

{% highlight clojure %}
(defn find-state-colors [maxiters]
  (apply pcalls (map (fn [i] (fn [] (worker i 1 maxiters false)))
                     (range 0 (count state-adjacency-list)))))
{% endhighlight %}

Note that the result of the call to **map** is a list of functions, where each function will invoke the **worker** function with the specified index value and maximum number of iterations. The inidices are generated by the **range** function. The result of **pcalls** is a list containing the result of each parallel function.

Example run (user input in **bold**):

<pre>
user=> <b>(find-state-colors 1000)</b>
((red true) (blue true) (purple true) (yellow true) (green true)
 (yellow true) (blue true) (green true) (purple true) (green true)
 (purple true) (red true) (blue true) (blue true) (yellow true)
 (green true) (green true) (purple true) (green true) (yellow true)
 (blue true) (yellow true) (purple true) (yellow true) (red true)
 (yellow true) (yellow true) (blue true) (blue true) (purple true)
 (blue true) (green true) (red true) (red true) (purple true)
 (yellow true) (blue true) (yellow true) (red true) (red true)
 (red true) (green true) (green true) (yellow true) (purple true)
 (yellow true) (green true) (red true) (red true) (green true)
 (red true))
</pre>

The **check-state-colors** function checks the final **state-colors** vector to ensure that each state was assigned a color different from its neighbors:

{% highlight clojure %}
(defn check-state [i neighbors]
  (if (empty? neighbors)
      true
      (let [neighbor-index (state-to-index-map (first neighbors))]
        (if (= (deref (nth state-colors i)) (deref (nth state-colors neighbor-index)))
            false
            (recur i (rest neighbors))))))

(defn check-state-colors []
  (letfn [(work [i n]
            (if (= i n)
                true
                (if (not (check-state i (get-neighbors i)))
                    false
                    (recur (+ i 1) n))))]
    (work 0 (count state-colors))))
{% endhighlight %}

Calling **check-state-colors** to ensure that the computation was successful:

<pre>
user=> <b>(check-state-colors)</b>
true
</pre>

Agents
------

Agents are much like actors in Erlang and Scala: an agent is a sequential process that receives messages and processes them, and may send messages to other actors.

An interesting characteristic of agents in Clojure is that messages are functions that operate on two values:

-   The agent's current data
-   Message data (that is explicitly sent to the agent)

The result of a message function becomes the new "current data" of the agent.

Really simple example:

{% highlight clojure %}
(defn say [count msg]
  (do
    (println count)
    (println msg)
    (+ count 1)))
{% endhighlight %}

Dynamically creating an agent and sending it some messages:

<pre>
user=&gt; <b>(def my-agent (agent 1))</b>
#'user/my-agent
user=&gt; <b>(send my-agent say "Hello")</b>
1
Hello
#&lt;Agent@10e98462: 2&gt;
user=&gt; <b>(send my-agent say "World")</b>
2
World
#&lt;Agent@10e98462: 2&gt;
</pre>

In this example, the agent's data is a number that is incremented each time the **say** message is received.
