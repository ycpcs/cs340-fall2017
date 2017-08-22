-module(map).
-export([map/2]).

mapwork(_, [], Accum) -> lists:reverse(Accum);
mapwork(F, [First|Rest], Accum) -> mapwork(F, Rest, [F(First)|Accum]).

map(F, L) -> mapwork(F, L, []).
