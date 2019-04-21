%%%-------------------------------------------------------------------
%%% @doc
%%% module player
%%% @end
%%%-------------------------------------------------------------------
-module(player).
%% API
-export([load/1, save/1]).
-export([reset/3]).
-export([save_timed_first/1, save_timed_second/1]).
%% includes
-include("player.hrl").
-include("assets.hrl").
-include("vip.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load data
load(User = #user{id = Id}) ->
    Data = player_sql:select(Id),
    F = fun(Player = #player{focus = Focus}) -> Player#player{focus = data_tool:string_to_term(Focus)} end,
    [Player] = data_tool:load(Data, player, F),
    User#user{player = Player}.

%% @doc save data
save(User = #user{player = Player}) ->
    player_sql:update(Player),
    User.

%% @doc save data timed
save_timed_first(User) ->
    player_logout:save_loop(#user.player, #user.assets, User).

%% @doc save data timed
save_timed_second(User) ->
    player_logout:save_loop(#user.quest, #user.shop, User).

%% @doc daily reset
reset(User, login, 0) ->
    User;
reset(User, login, 5) ->
    User;
reset(User, cross, 0) ->
    User;
reset(User, cross, 5) ->
    User.
%%%===================================================================
%%% Internal functions
%%%===================================================================