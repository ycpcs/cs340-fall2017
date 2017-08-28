---
layout: default
title: "Lab 12 solution"
---

{% highlight erlang %}
-module(sort).
-export([mergesort/1, merge/2]).

% Original (non-tail-recursive) version.
%merge([], Any) -> Any;
%merge(Any, []) -> Any;
%merge([X|RestL], [Y|RestR]) ->
%  if
%    X<Y  -> [X | merge(RestL, [Y|RestR])];
%    true -> [Y | merge([X|RestL], RestR)]
%  end.

merge(Left, Right) -> mergehelp(Left, Right, []).

mergehelp([], [], Accum) -> lists:reverse(Accum);
mergehelp([], [Y|RestR], Accum) -> mergehelp([], RestR, [Y|Accum]);
mergehelp([X|RestL], [], Accum) -> mergehelp(RestL, [], [X|Accum]);
mergehelp([X|RestL], [Y|RestR], Accum) ->
  if
  X<Y     -> mergehelp(RestL, [Y|RestR], [X|Accum]);
  true    -> mergehelp([X|RestL], RestR, [Y|Accum])
  end.

mergesort([]) -> [];
mergesort([A]) -> [A];
mergesort(List) ->
  N = length(List),
  % Sublist containing the first N/2 elements.
  Left = lists:sublist(List, N div 2),
  % Sublist containing the remaining elements.
  % Note: list elements are indexed starting at 1, not 0.
  Right = lists:sublist(List, (N div 2) + 1, N - (N div 2)),
  % Recursively sort left and right sublists.
  LeftSorted = mergesort(Left),
  RightSorted = mergesort(Right),
  % Merge the results of sorting the left and right sublists.
  merge(LeftSorted, RightSorted).
{% endhighlight %}
