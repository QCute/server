%%%-------------------------------------------------------------------
%%% @doc
%%% module player handle
%%% @end
%%%-------------------------------------------------------------------
-module(player_handle).
%% export API functions
-export([handle/3]).
-include("player.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
handle(?CMD_PLAYER, #user{player = Player}, []) ->
    {reply, Player};

handle(?CMD_PLAYER_ASSETS, #user{assets = Assets}, []) ->
    {reply, Assets};

handle(?CMD_SERVER_TIME, _, []) ->
    {reply, time:ts()};

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.