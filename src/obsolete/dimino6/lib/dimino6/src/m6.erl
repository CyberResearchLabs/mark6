-module(m6).
-export([main/1]).

main([A]) ->
	io:format("hello ~w = ~w~n",[A, A]),
	init:stop().

