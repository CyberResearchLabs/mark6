%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(m6_server_tests).
-author("dlapsley@haystack.mit.edu").
-include_lib("eunit/include/eunit.hrl").
-include("main.hrl").

-define(TEST_TIMEOUT_MS, 20000).
-define(INTER_COMMAND_DELAY_MS, 500).
-define(SETTLING_DELAY_MS, 500).

%% Current commands.
%% X3C: get_volume_state, set_volume_state

vsis_use_case_1() ->
    [
     "vol_stack?;",
     "vol_stack?;",
     "mod_init=A:HAYS0001:8;",
     "vol_stack?;",
     "vol_stack?;",
     "vol_stack?;",
     "vol_cmd=unprotect:B;",
     "vol_cmd=erase:B;",
     "vol_stack?;"
    ].

vsis_use_case_2() ->
    [
     "input_stream=add:RDBE1:vdif:eth0:192.162.1.38;",
     "input_stream=add:RDBE2:vdif:eth0:192.162.1.40;",
     "record=on:076-1233:exp123:wf;",
     "vol_stack?;",
     "record=off",
     "scan_info?;",
     "scan_check?;"
    ].

vsis_use_case_3() ->
    [
     "vol_stack?;",
     "record=on:076-1330:exp123:wf:200;",
     "scan_info?;",
     "vol_stack?;",
     "vol_cmd=remove:B;",
     "vol_stack?;"
    ].

vsis_use_case_4() ->
    [
     "vol_stack?;",
     "vol_cmd=inactive:A:B;",
     "vol_cmd=unprotect:A:B;",
     "vol_cmd=erase:A:B;",
     "vol_stack?;",
     "vol_cmd=bond:A:B;",
     "vol_stack?;",
     "record=on:076-1233:exp123:wf;",
     "vol_stack?;"
     ].

vsis_use_case_5() ->
    [
     "vol_stack?;",
     "vol_stack?;"
    ].

vsis_use_case_6() ->
    [
     "vol_stack?;",
     "vol_cmd=force:A;",
     "vol_stack?;"
     ].

vsis_use_case_7() ->
    [
     "vol_stack?;",
     "disk_info?usage;",
     "disk_info?serial;"
    ].

vsis_use_cases() ->
    [
     vsis_use_case_1(),
     vsis_use_case_2(),
     vsis_use_case_3(),
     vsis_use_case_4(),
     vsis_use_case_5(),
     vsis_use_case_6(),
     vsis_use_case_7()
    ].

first_test() ->
    [
     "disk_state=Volume1:dmsState;",
     "disk_state?Volume1;"
    ].


start_server() ->
    error_logger:info_msg("m6_server_test:start_server()~n"),
    m6_server:start_link().


my_test_() ->
    [
     %% {timeout, ?TEST_TIMEOUT_MS, ?_test(connect())},
     %% {timeout, ?TEST_TIMEOUT_MS, ?_test(send())},
     {timeout, ?TEST_TIMEOUT_MS, ?_test(send_use_cases())}
     ].

connect() ->
    error_logger:info_msg("m6_server_test:connect_test()~n"),
    process_flag(trap_exit, true),
    {ok, Pid} = start_server(),
    {ok, _Sock} = gen_tcp:connect("localhost", ?DEFAULT_LISTEN_PORT,
				 ?TCP_OPTIONS),
    error_logger:info_msg("m6_server_test:connect_test() sleeping~n"),
    timer:sleep(1000),
    exit(Pid, "Finished test."),
    ok.

send_use_cases() ->
    error_logger:info_msg("m6_server_test:send_test()~n"),
    process_flag(trap_exit, true),
    {ok, Pid} = start_server(),
    {ok, Server_pid} = x3c_dummy_server:start(dummy_commands:m6_x3c_responses()),
    lists:foreach(fun(X) -> send_command_list(X) end, vsis_use_cases()),
    timer:sleep(?SETTLING_DELAY_MS),
    exit(Pid, "Finished test."),
    % exit(X3C_Server_pid, "Finished test."),
    exit(Server_pid, "Finished test."),
    ok.

send_first() ->
    error_logger:info_msg("x3c_logger_tests:send_test()~n"),
    process_flag(trap_exit, true),
    {ok, Pid} = start_server(),
    {ok, X3C_Server_pid } = x3c_server:start_link(),
    send_command_list(first_test()),
    timer:sleep(?SETTLING_DELAY_MS),
    exit(Pid, "Finished test."),
    exit(X3C_Server_pid, "Finished test."),
    ok.

send_command_list([H|T]) ->
    io:format("send_command_list ~p~n", [H]),
    {ok, Sock} = gen_tcp:connect("localhost", ?DEFAULT_LISTEN_PORT,
				?TCP_OPTIONS),
    gen_tcp:send(Sock, H),
    gen_tcp:close(Sock),
    timer:sleep(?INTER_COMMAND_DELAY_MS),
    send_command_list(T);
send_command_list([]) ->
    ok.
