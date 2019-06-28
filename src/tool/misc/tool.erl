%%%-------------------------------------------------------------------
%%% @doc
%%% module tool
%%% @end
%%%-------------------------------------------------------------------
-module(tool).
-export([default/2]).

%% @doc value default
default([], Term) ->
	Term;
default(Term, _) ->
	Term.
