-module(echo).
-export([loop/0]).

loop() ->

  receive

    Any -> io:format("Echo: ~w~n", [Any]), loop()

  end.
