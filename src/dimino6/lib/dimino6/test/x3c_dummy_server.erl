%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(x3c_dummy_server).
-author("dlapsley@haystack.mit.edu").
-include_lib("eunit/include/eunit.hrl").
-include("main.hrl").
-export([start/1, server/2, handle_request/2]).


start(Responses) ->
    {ok, spawn(?MODULE, server, [?DEFAULT_XCUBE_PORT, Responses])}.

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
	    Xml = rsp_xml(H#x3c_resp.name, H#x3c_resp.retval, H#x3c_resp.reason,
			  H#x3c_resp.params),
	    ok = gen_tcp:send(Socket, Xml),
	    handle_request(Socket, T);
	Done ->
	    error_logger:info_msg("recv done: ~p~n", [Done])
    end;
handle_request(Socket, []) ->
    ok.




rsp_xml(Name, Retval, Reason, Param_list) ->
    Content = {x3c_resp, [{name, [Name]},
			 {retval, [Retval]},
			 {reason, [Reason]},
			 {params, rsp_xml(Param_list)}]},
    lists:flatten(xmerl:export_simple([ Content  ], xmerl_xml,
				      [{prolog, ?XML_PROLOG}])).
    
rsp_xml(Param_list) ->
    lists:map(fun(X) ->
		      {param, [{name, [X#param.name]},
			       {type, [X#param.type]},
			       {value, [X#param.value]}]}
	      end, Param_list).
