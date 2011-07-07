%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(m6_x3c_tests).
-author("dlapsley@haystack.mit.edu").
-include_lib("eunit/include/eunit.hrl").
-include("main.hrl").

-define(TEST_TIMEOUT_MS, 20000).
-define(INTER_COMMAND_DELAY_MS, 1000).
-define(SETTLING_DELAY_MS, 500).

my_test_() ->
    [
     {timeout, ?TEST_TIMEOUT_MS, ?_test(init())}%,
     %%  {timeout, ?TEST_TIMEOUT_MS, ?_test(parse())}
     ].



init() ->
    errologger:info_msg("m6_x3c_test:init_test()~n"),
    process_flag(trap_exit, true),
    {ok, X3C_Server_pid } = x3c_server:start_link(),
    {ok, Server_pid} = x3c_dummy_server:start(dummy_commands:m6_x3c_responses()),
    lists:map(fun(X) ->
		      {Command, Param_list} = X,
		      io:format("Command ~p ~p~n", [Command, Param_list]),
		      Xml = apply(m6_x3c, send_x3c_cmd, [Command | tuple_to_list(Param_list)]),
		      error_logger:info_msg("init() xml==~p~n", [ Xml ])
	      end,
	      dummy_commands:m6_x3c_commands()),
    exit(X3C_Server_pid, "Finished"),
    exit(Server_pid, "Finished").

parse() ->
    Data = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><x3c_cmd><name><check_data/></name><retval>ok</retval><reason>cool</reason><params><param><name>volumeName</name><type>string</type><value>myvolumename</value></param><param><name>projectName</name><type>string</type><value>myprojectname</value></param><param><name>interfaceName</name><type>string</type><value>myinterfacename</value></param></params></x3c_cmd>",

    {ok, {Name, Retval, Reason, Param_records}} = m6_x3c:xml_extract(Data),

    error_logger:info_msg("Command name: ~p~n", [Name]),   
    error_logger:info_msg("Command retval: ~p~n", [Retval]),   
    error_logger:info_msg("Command reason: ~p~n", [Reason]),   
    error_logger:info_msg("Param_records: ~p~n", [lists:map(fun(X) -> X#param.name end, Param_records)]). 

