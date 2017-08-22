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
