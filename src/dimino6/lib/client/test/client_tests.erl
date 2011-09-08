%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%%
%%% @end
%%% Created : 8 Sep 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(client_tests).
-author("dlapsley@haystack.mit.edu").
-include_lib("eunit/include/eunit.hrl").
-include("main.hrl").

my_test_() ->
    [
     {timeout, ?TEST_TIMEOUT_MS, ?_test(basic())}
    ].

start_server(Responses) ->
    {ok, spawn(?MODULE, server, [?DEFAULT_LISTEN_PORT, Responses])}.

server(Listen_port, Responses) ->
    {ok, Listen_socket} = gen_tcp:listen(Listen_port, ?TCP_OPTIONS),
    wait_connect(Listen_socket, Responses).

wait_connect(Listen_socket, List) ->
    {ok, Socket} = gen_tcp:accept(Listen_socket),
    handle_request(Socket, List).

handle_request(Socket, [H|T]) ->
    case gen_tcp:recv(Socket, 0) of
	{ok, _Command} ->
	    io:format("R: ~p~n", [H]),
	    Xml ="ok",
	    ok = gen_tcp:send(Socket, Xml),
	    handle_request(Socket, T);
	Done ->
	    error_logger:info_msg("recv done: ~p~n", [Done])
    end;
handle_request(Socket, []) ->
    ok.

% Start of tests...
basic() ->
    error_logger:info_msg("client_tests:basic()~n"),
    process_flag(trap_exit, true),
    {ok, Pid} = start_server([]),
    client:start(),
    exit(Pid, "Finished test."),
    ok.

