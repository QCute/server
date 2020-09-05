%%%-------------------------------------------------------------------
%%% @doc
%%% common tool
%%% @end
%%%-------------------------------------------------------------------
-module(tool).
%% API
-export([default/2, what/3]).
-export([text/1]).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc value default
-spec default(term(), term()) -> term().
default(undefined, Default) ->
    Default;
default(false, Default) ->
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

%% @doc get translate text
-spec text(Text :: atom()) -> binary().
text(Text) ->
    type:to_binary(text_data:(parameter_data:get(language))(Text)).
