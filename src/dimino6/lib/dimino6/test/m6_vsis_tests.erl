%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(m6_vsis_tests).
-author("dlapsley@haystack.mit.edu").
-include_lib("eunit/include/eunit.hrl").
-include("main.hrl").

-define(TEST_TIMEOUT_MS, 20000).
-define(INTER_COMMAND_DELAY_MS, 1000).
-define(SETTLING_DELAY_MS, 500).

my_test_() ->
    [
     {timeout, ?TEST_TIMEOUT_MS, ?_test(init())}
     ].

init() ->
    error_logger:info_msg("m6_vsis_test:init_test()~n"),
    process_flag(trap_exit, true),
    N=2,
    {Commands, Responses} = dummy_commands:gen_test(N),
    {ok, Server_pid} = x3c_dummy_server:start(Responses),
    {ok, X3C_Server_pid} = x3c_server:start_link(),
    lists:map(fun(Command) ->
		      Xml = m6_vsis:handle(Command),
		      error_logger:info_msg("response==~p~n", [ Xml ])
	      end,
	      Commands),
    exit(X3C_Server_pid, "Finished"),
    exit(Server_pid, "Finished").
