%%%-------------------------------------------------------------------
%%% @doc
%%% module map handle
%%% @end
%%%-------------------------------------------------------------------
-module(map_handler).
%% API
-export([handle/3]).
%% Includes
-include("user.hrl").
-include("map.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
handle(?PROTOCOL_MAP_FIGHTER, User, [X, Y]) ->
    map_server:move(User, X, Y);

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.
