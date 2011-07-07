%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(x3c_logger_tests).
-author("dlapsley@haystack.mit.edu").
-include_lib("eunit/include/eunit.hrl").
-include("main.hrl").

-define(TEST_TIMEOUT_MS, 10000).
-define(INTER_COMMAND_DELAY_MS, 500).
-define(SETTLING_DELAY_MS, 500).

%% Current commands.
%% X3C: get_volume_state, set_volume_state

%% vsis_mod_init() ->
%%     [
%%      "mod_init=A:ABC0001:8;",
%%      "vol_cmd=unprotect:B;",
%%      "vol_cmd=erase:B;"
%%     ].

%% vsis_record_start() ->
%%     [
%%      "input_stream=add:RDBE1:vdir:eth0:192.162.1.38;",
%%      "input_stream=add:RDBE1:vdir:eth0:192.162.1.40;",
%%      "record=on:076-1233:exp123:wf;"
%%     ].

%% vsis_record_stop() ->
%%     [
%%      "record=off"
%%     ].

my_tests() ->
    ok.

%% my_test_() ->
%%     [
%%      {timeout, ?TEST_TIMEOUT_MS, ?_test(send())}
%%      ].


%% first() ->
%%     [
%%      "disk_state=Volume1:dmsState;",
%%      "disk_state?Volume1;"
%%     ].


%% start_server() ->
%%     error_logger:info_msg("m6_server_test:start_server()~n"),
%%     m6_server:start_link().


%% connect() ->
%%     error_logger:info_msg("x3c_logger_tests:connect_test()~n"),
%%     process_flag(trap_exit, true),
%%     {ok, Pid} = start_server(),
%%     {ok, _Sock} = gen_tcp:connect("localhost", ?DEFAULT_LISTEN_PORT,
%% 				 ?TCP_OPTIONS),
%%     error_logger:info_msg("x3c_logger_tests:connect_test() sleeping~n"),
%%     timer:sleep(1000),
%%     exit(Pid, "Finished test."),
%%     ok.

%% send() ->
%%     error_logger:info_msg("x3c_logger_tests:send_test()~n"),
%%     process_flag(trap_exit, true),
%%     {ok, Pid} = start_server(),
%%     {ok, X3C_Server_pid } = x3c_server:start_link(),
%%     send_command_list(first()),
%%     timer:sleep(?SETTLING_DELAY_MS),
%%     exit(Pid, "Finished test."),
%%     exit(X3C_Server_pid, "Finished test."),
%%     ok.

%% send_command_list([H|T]) ->
%%     io:format("send_command_list ~p~n", [H]),
%%     {ok, Sock} = gen_tcp:connect("localhost", ?DEFAULT_LISTEN_PORT,
%% 				?TCP_OPTIONS),
%%     gen_tcp:send(Sock, H),
%%     gen_tcp:close(Sock),
%%     timer:sleep(?INTER_COMMAND_DELAY_MS),
%%     send_command_list(T);
%% send_command_list([]) ->
%%     ok.
