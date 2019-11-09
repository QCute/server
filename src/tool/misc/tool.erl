%%%------------------------------------------------------------------
%%% @doc
%%% module tool
%%% @end
%%%------------------------------------------------------------------
-module(tool).
%% API
-export([default/2]).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc value default
-spec default(term(), term()) -> term().
default(undefined, Term) ->
    Term;
default([], Term) ->
    Term;
default(<<>>, Term) ->
    Term;
default(Term, _) ->
    Term.
