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

scan_file(Fd) ->
    {ok, Binary} = file:read(Fd, ?HEADER_SIZE),
    scan_file(Fd, start, Binary).

scan_file(Fd, start, Header) ->
    case is_header(Header) of
	error ->
	    [H|T] = binary:bin_to_list(Header),
	    case file:read(Fd, 1) of
		{ok, Bin} ->
		    New_header = binary:list_to_bin(lists:append(T, [Bin])),
		    scan_file(Fd, start, New_header);
		{error, Reason} -> scan_file(Fd, error, Reason);
		eof -> scan_file(Fd, eof, "EOF")
	    end;
	ok ->
	    case file:read(Fd, ?PAYLOAD_SIZE) of
		{ok, Data} ->
		    case file:read(Fd, ?HEADER_SIZE) of
			{ok, New_header} -> scan_file(Fd, sync, New_header);
			{error, Reason} -> scan_file(Fd, error, Reason);
			eof -> scan_file(Fd, eof, "")
		    end;
		{error, Reason} -> scan_file(Fd, error, Reason);
		eof -> scan_file(Fd, eof, "EOF")
	    end
    end;
scan_file(Fd, sync, Header) ->
    case is_header(Header) of
	error ->
	    [H|T] = binary:bin_to_list(Header),
	    case file:read(Fd, 1) of
		{ok, Bin} ->
		    New_header = binary:list_to_bin(lists:append(T, [Bin])),
		    scan_file(Fd, start, New_header);
		{error, Reason} -> scan_file(Fd, error, Reason);
		eof -> scan_file(Fd, eof, "")
	    end;
	ok ->
	    case file:read(Fd, ?PAYLOAD_SIZE) of
		{ok, Bin} ->
		    case file:read(Fd, ?HEADER_SIZE) of
			{ok, New_header} -> scan_file(Fd, sync, New_header);
			{error, Reason} -> scan_file(Fd, error, Reason);
			eof -> scan_file(Fd, eof, "")
		    end;
		{error, Reason} -> scan_file(Fd, error, Reason);
		eof -> scan_file(Fd, eof, "")
	    end
    end;
scan_file(Fd, error, Reason) ->
    file:close(Fd),
    error_logger:error_msg("scan_file: error ~p~n", [ Reason ]);
scan_file(Fd, eof, _) ->
    file:close(Fd),
    error_logger:error_msg("scan_file: eof~n").


start_server(File_name) ->
    {ok, spawn(?MODULE, server, [File_name])}.

server(File_name) ->
    error_logger:info_msg("Server start."),
    error_logger:info_msg("Filename: ~p~n", [ File_name ]),
    {ok, Fd} = file:open(File_name, [ read, binary ]),
    FRAME_SIZE = 8224,
    scan_file(Fd).
    
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


    io:format("MW          : ~.16B~.16B~.16B~.16B~n", [ 0, 0, 0, W7B0 ]),
    io:format("Data_frame  : ~p~n~n", [ Data_frame ]),
    ok.

    % io:format("MW            : ~p ~p~n", [ MW1, MW2]),
    % io:format("Data_frame  : ~.16B~.16B~.16B~n", [ W1B2, W1B1, W1B0 ]),
    % io:format("Frame_length: ~p~n", [ Length ]),
    % io:format("Epoch       : ~p~n", [ Reference_epoch ]),
    % io:format("Epoch_seconds: ~.16B~.16B~.16B~.16B~n", [ W0B3, W0B2, W0B1, W0B0 ]),
    % io:format("Epoch_seconds: ~p~n", [ Epoch_seconds ]).

		
