%%%-------------------------------------------------------------------
%%% @doc
%%% module player handle
%%% @end
%%%-------------------------------------------------------------------
-module(player_handle).
%% API
-export([handle/3]).
%% Includes
-include("user.hrl").
-include("player.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% player
handle(?CMD_PLAYER, #user{player = Player}, []) ->
    {reply, [Player]};

%% assets
handle(?CMD_PLAYER_ASSETS, #user{assets = Assets}, []) ->
    {reply, [Assets]};

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.