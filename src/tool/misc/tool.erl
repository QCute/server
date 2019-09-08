%%%-------------------------------------------------------------------
%%% @doc
%%% module tool
%%% @end
%%%-------------------------------------------------------------------
-module(tool).
%% API
-export([default/2, default/3]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc value default
default(undefined, Term) ->
	Term;
default([], Term) ->
	Term;
default(<<>>, Term) ->
	Term;
default(Term, _) ->
	Term.

default(undefined, _, Term) ->
	Term;
default([], _, Term) ->
	Term;
default(<<>>, _, Term) ->
	Term;
default(_, Term, _) ->
	Term.
