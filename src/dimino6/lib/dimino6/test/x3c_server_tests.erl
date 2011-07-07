%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(x3c_server_tests).
-author("dlapsley@haystack.mit.edu").
-include_lib("eunit/include/eunit.hrl").
-include("main.hrl").

-define(TEST_TIMEOUT_MS, 20000).
-define(INTER_COMMAND_DELAY_MS, 1000).
-define(SETTLING_DELAY_MS, 500).

start_server() ->
    error_logger:info_msg("x3c_server_test:start_server()~n"),
    x3c_server:start_link().

my_test_() ->
    [
     {timeout, ?TEST_TIMEOUT_MS, ?_test(init())}
     ].

init() ->
    error_logger:info_msg("x3c_server_test:init_test()~n"),
    process_flag(trap_exit, true),
    {ok, Pid} = start_server(),
    exit(Pid, "Finished test."),
    ok.
