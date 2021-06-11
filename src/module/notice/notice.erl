%%%-------------------------------------------------------------------
%%% @doc
%%% notice
%%% @end
%%%-------------------------------------------------------------------
-module(notice).
%% API
-export([broadcast/4]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("role.hrl").
-include("notice.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc broadcast
-spec broadcast(Scope :: non_neg_integer(), Type :: non_neg_integer(), Text :: atom() | binary(), Content :: [term()]) -> ok.
broadcast(Scope, Type, Text, Args) when is_atom(Text) ->
    Content = parser:format(tool:text(Text), Args),
    {ok, Binary} = user_router:write(?PROTOCOL_NOTICE_BROADCAST, [Scope, Type, <<>>, Content]),
    user_manager:broadcast(Binary);
broadcast(Scope, Type, Title, Content) when is_binary(Title) ->
    {ok, Binary} = user_router:write(?PROTOCOL_NOTICE_BROADCAST, [Scope, Type, Title, Content]),
    user_manager:broadcast(Binary).

%%%===================================================================
%%% Internal functions
%%%===================================================================
