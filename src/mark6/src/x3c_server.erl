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
-module(x3c_server).
-behaviour(gen_server).
-author("dlapsley@haystack.mit.edu").
-include("main.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% API
-export([start_link/0, send_x3c_cmd/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {xcube_host, xcube_port, xcube_socket}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% @doc
%% Sends a command to the server.
%%
%% @spec send_x3c_cmd(Type, Args) -> {ok, Response} | {error, Error}
%% @end
%%--------------------------------------------------------------------
send_x3c_cmd(Type, Args) ->
    gen_server:call(?SERVER, {send_x3c_cmd, Type, Args}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    error_logger:info_msg("x3c_server:init()"),
    Xcube_host = case application:get_env(xcube_host) of
		     {ok, Host} -> Host;
		     undefined -> ?DEFAULT_HOST
		 end,
    Xcube_port = case application:get_env(xcube_port) of
		     {ok, Port} -> Port;
		     undefined -> ?DEFAULT_PORT
		 end,
    {ok, Xcube_socket} = gen_tcp:connect(Xcube_host, Xcube_port,
					 ?TCP_OPTIONS),
    {ok, #state{xcube_host=Xcube_host, xcube_port=Xcube_port,
		xcube_socket=Xcube_socket}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({send_x3c_cmd, Type, Args}, _From, State) ->
    error_logger:info_msg("handle_call:send_x3c_cmd ~p~n", [ lists:append([Type], Args) ]),
    Xml_string = apply(m6_x3c, send_x3c_cmd, lists:append([Type], Args)),
    Packet = handle_send(Xml_string, State#state.xcube_socket,
			 ?DEFAULT_CONNECT_RETRIES),
    {reply, Packet, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_send(Xml_string, Sock, Tries) when Tries>0 ->
    error_logger:info_msg("x3c_server:handle_send command xml_string == ~p~n", [Xml_string]),
    ok = gen_tcp:send(Sock, Xml_string),
    case gen_tcp:recv(Sock, 0, ?DEFAULT_RCV_TO) of
	{ok, Packet} ->
	    error_logger:info_msg("x3c_server:handle_call response ~p~n",
				  [Packet]),
	    Packet;
	{error, Reason} ->
	    error_logger:info_msg("x3c_server:handle_call error ~p~n",
				  [atom_to_list(Reason)]),
	    Reason;
	_ ->
	    handle_send(Xml_string, Sock, Tries - 1)
    end;
handle_send(Xml_string, Socket, 0) ->
    "Send retries exceeded".



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Extract the Name, Retval, Reason and Param records from an X3C
%% command response.
%%
%% @spec xml_extract(Data) -> {ok, {Name, Retval, Reason, Param_records}}
%%                            | {error, Error}.
%% @end
%%--------------------------------------------------------------------
xml_extract(Data) ->
    {Root, _Misc} = xmerl_scan:string(Data),
    x3c_cmd = Root#xmlElement.name,
    Content = Root#xmlElement.content,
    {ok, {Name, Retval, Reason}} = xml_extract_top(Content),
    {ok, Param_records} = xml_extract_params(Content),
    {ok, {Name, Retval, Reason, Param_records}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Extract the Name, Retval, and Reason from an X3C command response.
%%
%% @spec xml_extract_top(Content) -> {ok, {Name, Retval, Reason}}
%%                                   | {error, Error}.
%% @end
%%--------------------------------------------------------------------
xml_extract_top(Content) ->
    {ok, {lists:filter(fun(X) -> X#xmlElement.name == name end, Content),
	  lists:filter(fun(X) -> X#xmlElement.name == retval end, Content),
	  lists:filter(fun(X) -> X#xmlElement.name == reason end, Content)}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Extract the Parameters from an X3C command response. Convert and 
%% return them as param records.
%%
%% @spec xml_extract_params(Content) -> {ok, Param_records}
%%                                      | {error, Error}.
%% @end
%%--------------------------------------------------------------------
xml_extract_params(Content) ->
    Ps = lists:filter(fun(X) -> X#xmlElement.name == params end, Content),
    Ps_content = lists:map(fun(X) -> X#xmlElement.content end, Ps),
    P_list = lists:filter(fun(X) -> X#xmlElement.name == param end,
			  lists:flatten(Ps_content)),
    % Returns the value of a pname, type, value node.
    Proc_text = fun(Y) ->
			lists:nth(1,
				  lists:map(fun(Z) ->
						    Z#xmlText.value
					    end, Y#xmlElement.content)) 
		end,
    % Returns [ {pname, value}, {type, value}, {value, value} ].
    Proc_param = fun(X) ->
			 lists:map(fun(Y) ->
					   {Y#xmlElement.name,
					    Proc_text(Y)}
				   end, X#xmlElement.content)
		 end,
    P_raw = lists:map(Proc_param, P_list),
    {ok, lists:map(fun(X) -> param_tuple_to_record(X) end, P_raw)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert a tuple of key value pairs into a param record.
%%
%% @spec xml_extract_params(KeyValuePairs) -> {ok, Param_records}
%%                                            | {error, Error}.
%% @end
%%--------------------------------------------------------------------
param_tuple_to_record(Keyvalpairs) ->
    #param{name=proplists:get_value(name, Keyvalpairs), 
	   type=proplists:get_value(type, Keyvalpairs),
	   value=proplists:get_value(value, Keyvalpairs)}.
