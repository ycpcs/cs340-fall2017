---
layout: default
title: "Lecture 22: Concurrency in Erlang"
---

Example code: [echo.erl](echo.erl), [mandelbrot.erl](mandelbrot.erl), [rowactor.erl](rowactor.erl), [mandelbrotactor.erl](mandelbrotactor.erl)

Concurrency in Erlang
=====================

Concurrency in Erlang is expressed using *processes*. A process is an independent thread of control that does not share memory with any other process: so, processes are isolated from each other. Processes communicate with each other by sending messages.

Processes in Erlang
-------------------

A process is started by calling a function that takes no parameters. Ths function should use the **receive** construct to wait for a message to arrive, process the message, and then call itself recursively. (As long as the recursive call is in tail position, this will not cause growth of the call stack.)

Here is a very simple actor: one that simply prints out any messages it receives:

{% highlight erlang %}
-module(echo).
-export([loop/0]).

loop() ->

  receive

    Any -> io:format("Echo: ~w~n", [Any]), loop()

  end.
{% endhighlight %}

The **receive** construct does pattern matching on received messages. In the echo actor above, the only pattern is the variable **Any**, which will match any value sent to the process.

Example of compiling and running this process:

<pre>
1> <b>c(echo).</b>
{ok,echo}
2> <b>EchoPid = spawn(fun echo:loop/0).</b>
<0.39.0>
3> <b>EchoPid ! "Hello".</b>
Echo: [72,101,108,108,111]
"Hello"
4> <b>EchoPid ! {hey, there}.</b>
Echo: {hey,there}
{hey,there}
</pre>

Erlang processes are created by the built-in **spawn** function, and identified by *process ids*. To send a message to a process, the syntax is

> *ProcessId* ! *message*

Example: The Mandelbrot Set using Erlang processes
--------------------------------------------------

As a complete example, let's do the Mandelbrot set computation using Erlang actors. First, we need some functions to do complex arithmetic and to compute iteration counts for a row of complex numbers:

{% highlight erlang %}
-module(mandelbrot).
-export([complexadd/2, complexmul/2, complexmagnitude/1,
         computeitercount/1, computerow/1]).

complexadd({A, B}, {C, D}) -> {A+C, B+D}.

complexmul({A, B}, {C, D}) -> {A*C - B*D, B*C + A*D}.

complexmagnitude({A, B}) -> math:sqrt(A*A + B*B).

computeitercountwork(C, Z, Count) ->
  MagnitudeOfZ = complexmagnitude(Z),
  if
  (Count >= 1000) or (MagnitudeOfZ > 2.0) -> Count;
  true -> computeitercountwork(C, complexadd(complexmul(Z, Z), C), Count + 1)
  end.

computeitercount(C) -> computeitercountwork(C, {0.0, 0.0}, 0).

computerowwork(RowNum, Y, XStart, XInc, CurCol, Accum) ->
  if
  (CurCol < 0) -> {rowresult, RowNum, Accum};
  true ->
    X = XStart + (CurCol * XInc),
    IterCount = computeitercount({X, Y}),
    computerowwork(RowNum, Y, XStart, XInc, CurCol - 1, [IterCount | Accum])
  end.

computerow({row, RowNum, Y, XStart, XInc, NumCols}) ->
  computerowwork(RowNum, Y, XStart, XInc, NumCols-1, []).
{% endhighlight %}

Next, an actor process which receives messages specifying rows of iteration counts to be computed, computes them, and sends the results back to a result collector process:

{% highlight erlang %}
-module(rowactor).
-export([loop/0]).

loopwork(ResultCollector) ->
  receive

    % The result collector process has sent us its pid.
    {resultcollectorpid, ResultCollectorPid} ->
      loopwork(ResultCollectorPid);

    % Received a row to compute: compute it and send result back to result collector.
    {row, RowNum, Y, XStart, XInc, NumCols} ->
      ResultCollector ! mandelbrot:computerow({row, RowNum, Y, XStart, XInc, NumCols}),
      loopwork(ResultCollector)

  end.

loop() -> loopwork(unknown).
{% endhighlight %}

Note one interesting detail: the first message the row actor process should receive is a tuple of the form

> {resultcollectorpid, *Pid*}

which specifies the process id of the process to which computed row results should be sent. Once this message is received, all subsequent calls to **loopwork** will have this process id available.

Finally, an actor which receives a message describing a region of the complex plane, creates row actors, sends work to the row actors, and waits for row results to be sent back:

{% highlight erlang %}
-module(mandelbrotactor).
-export([loop/0]).

% Send work to a specified row actor process.
sendwork(Pid, XMin, XMax, YMin, YMax, NumCols, NumRows, NumProcs, RowNum) ->
  if
  % We're done if there is no more work to send
  (RowNum >= NumRows) -> true;
  true ->
     % Send one row
     Pid ! {row, RowNum, YMin + (RowNum*((YMax-YMin)/NumRows)),
                 XMin, (XMax-XMin)/NumCols,
                 NumCols},
     % Send the rest of the rows
     sendwork(Pid, XMin, XMax, YMin, YMax, NumCols, NumRows,
              NumProcs, RowNum + NumProcs)
  end.

% Start row actor processes.
startprocs(_, _, _, _, _, _, N, N, Pids) -> Pids;
startprocs(XMin, XMax, YMin, YMax, NumCols, NumRows, NumProcs, CurProc, Pids) ->
  % Spawn a process to compute rows
  Pid = spawn(fun rowactor:loop/0),
  % Inform process where to send results (back to this process) 
  Pid ! {resultcollectorpid, self()},
  % Send the process the rows it should compute
  sendwork(Pid, XMin, XMax, YMin, YMax, NumCols, NumRows, NumProcs, CurProc),
  % Spawn the rest of the processes
  startprocs(XMin, XMax, YMin, YMax, NumCols, NumRows, NumProcs, CurProc + 1,
             [Pid | Pids]).

loop() ->
  receive

    {start, XMin, XMax, YMin, YMax, NumCols, NumRows, NumProcs} ->

      % Start processes
      startprocs(XMin, XMax, YMin, YMax, NumCols, NumRows,
                 NumProcs, 0, []),
      loop();

    {rowresult, RowNum, Data} ->

      % Just print out the received data
      io:format("~w: ~w~n", [RowNum, Data]), loop()

  end.
{% endhighlight %}

Note that completed row results are just printed out using **io:format**, in whatever order they arrive.

Example run:

<pre>
1> <b>c(mandelbrot).</b>
{ok,mandelbrot}
2> <b>c(rowactor).</b>
{ok,rowactor}
3> <b>c(mandelbrotactor).</b>
{ok,mandelbrotactor}
4> <b>Pid = spawn(fun mandelbrotactor:loop/0).</b>
<0.49.0>
5> <b>Pid ! {start, -2, 2, -2, 2, 10, 10, 3}.</b>
{start,-2,2,-2,2,10,10,3}
2: [1,2,2,3,3,3,2,2,2,2]
0: [1,1,1,1,1,2,1,1,1,1]
3: [1,3,3,4,6,18,4,2,2,2]
1: [1,1,2,2,2,2,2,2,2,1]
5: [1000,1000,1000,1000,1000,1000,7,3,2,2]
8: [1,2,2,3,3,3,2,2,2,2]
6: [1,3,7,7,1000,1000,9,3,2,2]
9: [1,1,2,2,2,2,2,2,2,1]
4: [1,3,7,7,1000,1000,9,3,2,2]
7: [1,3,3,4,6,18,4,2,2,2]
</pre>

In this example run, we used three row actor processes to compute 10 rows. Each row actor computes every *n*th row, where *n* is the number of processes.

As you can see, the row results do not come back in sorted order, so some additional work is needed to put them in order.
