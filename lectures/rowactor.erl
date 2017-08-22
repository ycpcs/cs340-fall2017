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
