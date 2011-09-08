%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%% Connects to X3C logger via TCP and is responsible for transmitting
%%% commands and receiving the response to those commands and passing
%%% them back.
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(client).
-author("dlapsley@haystack.mit.edu").
-include_lib("xmerl/include/xmerl.hrl").
-include("main.hrl").

%% API
-export([start/0]).

trim_whitespace(Input) ->
    LS = re:replace(Input, "^[ \\t\\n]*", "", [{return, list}]),
    RS = re:replace(LS, "[ \\t\\n]*$", "", [{return, list}]),
    RS.

start() ->
    io:format(?HLINE ++ "\n"),
    io:format("Mark6 CLI~n", []),
    io:format("Copyright 2011 MIT Haystack Observatory~n", []),
    io:format("del@haystack.mit.edu~n~n", []),
    io:format("Type 'quit;' to exit CLI...~n", []),
    io:format(?HLINE ++ "\n"),
    case gen_tcp:connect(?DEFAULT_HOST, ?DEFAULT_PORT, ?TCP_OPTIONS) of
	{ok, Sock} ->
	    io:format("Connected to server...~n"),
	    loop(Sock);
	{error, Reason} ->
	    io:format("Error connecting to server: ~w~n", [ Reason ]);
	_ ->
	    io:format("Unknown error connecting to server~n")
    end.

loop(Sock) ->
    case io:get_line("mark6>") of
	eof ->
	    io:format("Done");
	{error, Reason } ->
	    error_logger:info_msg("Error parsing command: ~w~n", [ Reason ]);
	Cmd ->
	    Cleaned_cmd = string:to_lower(trim_whitespace(Cmd)),
	    case string:equal(Cleaned_cmd, "quit;") of
		true ->
		    io:format("Quitting...");
		false ->
		    gen_tcp:send(Sock, Cleaned_cmd),
		    loop(Sock)
	    end	
    end.
