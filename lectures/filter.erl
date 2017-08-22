-module(filter).
-export([retain/2, discard/2]).

filterwork(_, [], _, Accum) -> lists:reverse(Accum);
filterwork(F, [First|Rest], Keep, Accum) ->
  Test = F(First),
  if
    (Test and Keep) or (not Test and not Keep) ->
        filterwork(F, Rest, Keep, [First | Accum]);
    true -> filterwork(F, Rest, Keep, Accum)
  end.

retain(F, List) -> filterwork(F, List, true, []).

discard(F, List) -> filterwork(F, List, false, []).
