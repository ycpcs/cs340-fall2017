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
