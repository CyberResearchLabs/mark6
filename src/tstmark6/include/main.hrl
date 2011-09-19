%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%%
%%% @end
%%% Created : 8 Sep 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------

%%%-------------------------------------------------------------------
%% Defines
%%%-------------------------------------------------------------------

%% M6 Server.
-define(DEFAULT_BLOCK_SIZE, 1024).

%% Xcube connectivity.
-define(DEFAULT_HOST, "127.0.0.1").
-define(DEFAULT_PORT, 14242).
-define(DEFAULT_RCV_TO, 1000).
-define(DEFAULT_CONNECT_RETRIES, 5).

-define(TCP_OPTIONS,[list,{packet, 0},{active, false},{reuseaddr, true}]).

-define(XML_PROLOG, "").
-define(XML_PRETTY_PRINTING, true).

%% Enable/disable eunit tests.
-define(NOTEST, false).

%% Eunit commands.
-define(TEST_TIMEOUT_MS, 20000).
-define(INTER_COMMAND_DELAY_MS, 500).
-define(SETTLING_DELAY_MS, 500).

%% Useful constants.
-define(HLINE, "------------------------------------------------------------------------------").

%%%-------------------------------------------------------------------
%% Records
%%%-------------------------------------------------------------------

