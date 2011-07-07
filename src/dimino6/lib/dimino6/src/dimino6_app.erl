%%%-------------------------------------------------------------------
%%% @author David <dlapsley@lib1-lt.haystack.mit.edu>
%%% @copyright (C) 2011, MIT Haystack Observatory
%%% @doc
%%%
%%% @end
%%% Created : 29 Mar 2011 by David <dlapsley@lib1-lt.haystack.mit.edu>
%%%-------------------------------------------------------------------
-module(dimino6_app).
-author("dlapsley@haystack.mit.edu").
-behavior(application).
-export([start/2, stop/1]).

start(_Type, _Arguments) ->
    error_logger:error_msg("Starting dimino6 application~n"),
    dimino6_sup:start_link().

stop(_State) ->
	ok.

