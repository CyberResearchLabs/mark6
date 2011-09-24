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
-module(vdif).
-author("dlapsley@haystack.mit.edu").
-include_lib("xmerl/include/xmerl.hrl").
-include("main.hrl").

%% API
-export([start_server/1, server/1]).
-define(FRAME_SIZE, 8224).
-define(HEADER_SIZE, 32).
-define(PAYLOAD_SIZE, 8192).

trim_whitespace(Input) ->
    LS = re:replace(Input, "^[ \\t\\n]*", "", [{return, list}]),
    RS = re:replace(LS, "[ \\t\\n]*$", "", [{return, list}]),
    RS.

is_header(Bin) ->
    try decode_header(binary:part(Bin, {0, 32}))
    catch
	_:_ -> error
    end.

scan_file(Fd_data, Fd_in, Fd_out) ->
    {ok, Binary} = file:read(Fd_in, ?HEADER_SIZE),
    scan_file(Fd_data, Fd_in, Fd_out, start, Binary, 0).

scan_file(Fd_data, Fd_in, Fd_out, start, Header, _Frame) ->
    case is_header(Header) of
	error ->
	    [H|T] = binary:bin_to_list(Header),
	    case file:read(Fd_in, 1) of
		{ok, Bin} ->
		    New_header = binary:list_to_bin(lists:append(T, [Bin])),
		    scan_file(Fd_data, Fd_in, Fd_out, start, New_header, 0);
		{error, Reason} -> scan_file(Fd_data, Fd_in, Fd_out, error, Reason, 0);
		eof -> scan_file(Fd_data, Fd_in, Fd_out, eof, "EOF", 0)
	    end;
	{ok, Data_frame, Thread_id, Station_id, Epoch_seconds, Length} ->
	    io:format(Fd_out, "~p,~p,~p,~p,~p~n", [ Data_frame, Thread_id, Station_id, Epoch_seconds, Length ]),
	    case file:read(Fd_in, ?PAYLOAD_SIZE) of
		{ok, Data} ->
		    case file:read(Fd_in, ?HEADER_SIZE) of
			{ok, New_header} -> scan_file(Fd_data, Fd_in, Fd_out, sync, New_header, Data_frame);
			{error, Reason} -> scan_file(Fd_data, Fd_in, Fd_out, error, Reason, Data_frame);
			eof -> scan_file(Fd_data, Fd_in, Fd_out, eof, "", Data_frame)
		    end;
		{error, Reason} -> scan_file(Fd_data, Fd_in, Fd_out, error, Reason, 0);
		eof -> scan_file(Fd_data, Fd_in, Fd_out, eof, "EOF", 0)
	    end
    end;
scan_file(Fd_data, Fd_in, Fd_out, sync, Header, Frame) ->
    case is_header(Header) of
	error ->
	    [H|T] = binary:bin_to_list(Header),
	    case file:read(Fd_in, 1) of
		{ok, Bin} ->
		    New_header = binary:list_to_bin(lists:append(T, [Bin])),
		    scan_file(Fd_data, Fd_in, Fd_out, start, New_header, 0);
		{error, Reason} -> scan_file(Fd_data, Fd_in, Fd_out, error, Reason, 0);
		eof -> scan_file(Fd_data, Fd_in, Fd_out, eof, "", 0)
	    end;
	{ok, Data_frame, Thread_id, Station_id, Epoch_seconds, Length} ->
	    % io:format(Fd_out, "~p,~p,~p,~p,~p~n", [ Data_frame, Thread_id, Station_id, Epoch_seconds, Length ]),
	    if
		Data_frame /= Frame + 1, Data_frame /= Frame ->
		    io:format("Frame skip: ~p ~p ~p~n", [ Data_frame, Frame, Data_frame - Frame ]);
		true ->
		    0
	    end,
	    case file:read(Fd_in, ?PAYLOAD_SIZE) of
		{ok, Bin} ->
		    file:write(Fd_data, Header),
		    file:write(Fd_data, Bin),
		    case file:read(Fd_in, ?HEADER_SIZE) of
			{ok, New_header} -> scan_file(Fd_data, Fd_in, Fd_out, sync, New_header, Data_frame);
			{error, Reason} -> scan_file(Fd_data, Fd_in, Fd_out, error, Reason, Data_frame);
			eof -> scan_file(Fd_data, Fd_in, Fd_out, eof, "", Data_frame)
		    end;
		{error, Reason} -> scan_file(Fd_data, Fd_in, Fd_out, error, Reason, Data_frame);
		eof -> scan_file(Fd_data, Fd_in, Fd_out, eof, "", Data_frame)
	    end
    end;
scan_file(Fd_data, Fd_in, Fd_out, error, Reason, _Frame) ->
    file:close(Fd_in),
    file:close(Fd_out),
    error_logger:error_msg("scan_file: error ~p~n", [ Reason ]);
scan_file(Fd_data, Fd_in, Fd_out, eof, _, _Frame) ->
    file:close(Fd_in),
    file:close(Fd_out),
    error_logger:error_msg("scan_file: eof~n").


start_server([File_name_data, File_name_in, File_name_out]) ->
    {ok, spawn(?MODULE, server, [ [File_name_data, File_name_in, File_name_out] ])}.

server([File_name_data, File_name_in, File_name_out]) ->
    error_logger:info_msg("Server start."),
    error_logger:info_msg("Filename: ~p ~p~n", [ File_name_in, File_name_out ]),
    {ok, Fd_in} = file:open(File_name_in, [ read, binary ]),
    {ok, Fd_out} = file:open(File_name_out, [ write ]),
    {ok, Fd_data} = file:open(File_name_data, [ write, binary, raw ]),
    FRAME_SIZE = 8224,
    scan_file(Fd_data, Fd_in, Fd_out).
    
decode_header(<<
		W0B0:8, W0B1:8, W0B2:8, W0B3:8, 
		W1B0:8, W1B1:8, W1B2:8, W1B3:8, 
		W2B0:8, W2B1:8, W2B2:8, W2B3:8, 
		W3B0:8, W3B1:8, W3B2:8, W3B3:8, 
		W4B0:8, W4B1:8, W4B2:8, W4B3:8, 
		W5B0:8, W5B1:8, W5B2:8, W5B3:8, 
		W6B0:8, W6B1:8, W6B2:8, W6B3:8, 
		W7B0:8, 16#DE:8, 16#AD:8, 16#AB:8
	      >>) ->    
    <<I:1, L:1, Seconds_a:6>> = <<W0B3:8>>,
    <<Epoch_seconds:32>> = <<Seconds_a, W0B2, W0B1, W0B0>>,
    <<_:2,Reference_epoch:6>> = << W1B3 >>,
    <<Data_frame:32>> = <<2#0:8, W1B2, W1B1, W1B0>>,
    <<V:3, Log_channels:5>> = <<W2B3>>,
    <<Length:32>> = <<2#0:8, W2B2, W2B1, W2B0>>,
    <<C:1, Bits:5,Thread_id_a:2>> = <<W3B3>>,
    <<Thread_id:32>> = <<2#0:22, Thread_id_a:2, W3B2:8>>,
    <<Station_id:32>> = <<2#0:16, W3B1:8, W3B1:8>>,
    <<EDV:8>> = <<W4B3>>,

     % io:format("~p ~p~n", [ Data_frame, Thread_id ]),
     {ok, Data_frame, Thread_id, Station_id, Epoch_seconds, Length}.

    %io:format("MW          : ~.16B~.16B~.16B~.16B~n", [ 0, 0, 0, W7B0 ]),
     %io:format("Data_frame  : ~p~n~n", [ Data_frame ]),
    % io:format("MW            : ~p ~p~n", [ MW1, MW2]),
    % io:format("Data_frame  : ~.16B~.16B~.16B~n", [ W1B2, W1B1, W1B0 ]),
    % io:format("Frame_length: ~p~n", [ Length ]),
    % io:format("Epoch       : ~p~n", [ Reference_epoch ]),
    % io:format("Epoch_seconds: ~.16B~.16B~.16B~.16B~n", [ W0B3, W0B2, W0B1, W0B0 ]),
    % io:format("Epoch_seconds: ~p~n", [ Epoch_seconds ]).

		
