%%%------------------------------------------------------------------
%%% @doc
%%% module tool
%%% @end
%%%------------------------------------------------------------------
-module(tool).
%% API
-export([default/2, what/3]).
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc value default
-spec default(term(), term()) -> term().
default(false, Default) ->
    Default;
default(undefined, Default) ->
    Default;
default([], Default) ->
    Default;
default(<<>>, Default) ->
    Default;
default(Other, _) ->
    Other.

%% @doc ternary expression
-spec what(boolean(), term(), term()) -> term().
what(true, True, _) ->
    True;
what(false, _, False) ->
    False.


