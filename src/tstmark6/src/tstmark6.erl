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
-module(tstmark6).
-author("dlapsley@haystack.mit.edu").
-include_lib("xmerl/include/xmerl.hrl").
-include("main.hrl").

%% API
-export([start_server/1, server/1]).

trim_whitespace(Input) ->
    LS = re:replace(Input, "^[ \\t\\n]*", "", [{return, list}]),
    RS = re:replace(LS, "[ \\t\\n]*$", "", [{return, list}]),
    RS.

start_server(Test) ->
    {ok, spawn(?MODULE, server, [Test])}.

server(Test) ->
    error_logger:info_msg("Server start."),
    io:format(?HLINE ++ "\n"),
    io:format("Mark6 CLI~n", []),
    io:format("Copyright 2011 MIT Haystack Observatory~n", []),
    io:format("del@haystack.mit.edu~n~n", []),
    io:format("Type 'quit;' to exit CLI...~n", []),
    io:format(?HLINE ++ "\n"),
    case gen_tcp:connect(?DEFAULT_HOST, ?DEFAULT_PORT,
			 ?TCP_OPTIONS) of
	{ok, Sock} ->
	    io:format("Connected to server...~n"),
	    loop(Sock, Test);
	{error, Reason} ->
	    error_logger:warn_msg("Error connecting to server: ~w~n",
				  [ Reason ]);
	_ ->
	    error_logger:warn_msg("Unknown error connecting to server~n")
    end.

loop(Sock, Test) ->
    Input = case Test of
		true ->
		    receive
			{_, Msg} ->
			    error_logger:info_msg(Msg),
			    Msg
		    end;
		_ ->
		    error_logger:info_msg("Here"),
		    io:get_line("mark6>")
	    end,
    case Input of
	eof ->
	    error_logger:info_msg("Done");
	{error, Reason } ->
	    error_logger:info_msg("Error parsing command: ~w~n",
				  [ Reason ]);
	Cmd ->
	    Cleaned_cmd = string:to_lower(trim_whitespace(Cmd)),
	    case string:equal(Cleaned_cmd, "quit;") of
		true ->
		    error_logger:info_msg("Quitting...");
		false ->
		    gen_tcp:send(Sock, Cleaned_cmd),
		    Response = get_response(Sock, []),
		    io:format(Response),
		    loop(Sock, Test)
	    end	
    end.

get_response(Sock, List) ->
    case gen_tcp:recv(Sock, 1) of
	{ok, ";"} ->
	    lists:reverse(List) ++ ";";
	{ok, Char} ->
	    get_response(Sock, [Char|List]);
	Error ->
	    error_logger:info_msg("R: ~p~n", [Error])
    end.
