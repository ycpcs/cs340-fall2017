---
layout: default
title: "Lecture 24: Agents in Clojure"
---

Agent-based Mandelbrot computation in Clojure:

> [clojure-mandelbrot.zip](clojure-mandelbrot.zip)

`complex.clj` has functions implementing arithmetic on complex numbers.

`mandelbrot.clj` has functions implementing the core Mandelbrot set computation (iterating <i>Z</i> = <i>Z</i><sup>2</sup> + <i>C</i>).

`rowagent.clj` is the code for creating and running row agents.  Row agents respond to `compute-row` messages, and respond by computing iteration counts for the requested row.  The state data for a row agent is simply a reference to the mandelbrot agent (where results will be sent) and the message function to be used to send back row results to the mandelbrot agent.

`mandelbrotagent.clj` is the code for creating an running the main mandelbrot agent.  It responds to `start` and `row-result` messages.  The `start` message indicates the start of the computation, and causes the mandelbrot agent to create row agents and assign work to them.  The `row-result` message indicates that a row of iteration counts has been completed by a row agent.  The state data for a mandelbrot agent is a map of unique ids (representing computations) to a vector containing the received row results for the computation.

Example of creating a mandelbrot agent and starting a computation (this assumes the REPL is in the `clojure-mandelbrot.mandelbrotagent` namespace):

<pre>
=&gt; <b>(def m (create))</b>
#'clojure-mandelbrot.mandelbrotagent/m
=&gt; <b>(send m start [-2 -2 2 2 10 10 3])</b>
#&lt;Agent@73c36447: {}&gt;
Final data:
[[0 (1 1 1 1 1 2 1 1 1 1)]
 [3 (1 3 3 4 6 18 4 2 2 2)]
 [6 (1 3 7 7 1000 1000 9 3 2 2)]
 [9 (1 1 2 2 2 2 2 2 2 1)]
 [1 (1 1 2 2 2 2 2 2 2 1)]
 [4 (1 3 7 7 1000 1000 9 3 2 2)]
 [7 (1 3 3 4 6 18 4 2 2 2)]
 [2 (1 2 2 3 3 3 2 2 2 2)]
 [5 (1000 1000 1000 1000 1000 1000 7 3 2 2)]
 [8 (1 2 2 3 3 3 2 2 2 2)]]
</pre>

As with the [Mandelbrot computation using Erlang actors](lecture22.html#example-the-mandelbrot-set-using-erlang-processes), the row data is received in an unpredictable order.
