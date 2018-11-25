%%----------------------------------------------------
%% @doc
%% module player
%% @end
%%----------------------------------------------------
-module(player).
-include("player.hrl").
-export([load/1, save/1]).
-export([save_timed_first/1, save_timed_second/1]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load data
load(User = #user{id = Id}) ->
    Data = player_sql:select(Id),
    [Player] = data_tool:load(Data, player),
    User#user{player = Player}.

%% @doc save data
save(User = #user{player = Player}) ->
    player_sql:update(Player),
    User.

%% @doc save data timed
save_timed_first(User) ->
	player_logout:save_loop(#user.player, #user.assess, User).

%% @doc save data timed
save_timed_second(User) ->
    player_logout:save_loop(#user.quest, #user.shop, User).
%%%===================================================================
%%% Internal functions
%%%===================================================================