%%%-------------------------------------------------------------------
%%% @doc
%%% module user login
%%% @end
%%%-------------------------------------------------------------------
-module(player_login).
-include("player.hrl").
%% API
-export([login/1]).
-export([load_loop/3]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load data on player login
login(User) ->
    Size = tuple_size(User),
    load_loop(2, Size, User).

%% @doc load loop
load_loop(Size, Size, User) ->
    load(Size, User);
load_loop(Position, Size, User) ->
    NewUser = load(Position, User),
    load_loop(Position + 1, Size, NewUser).
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% load per player's data
load(#user.player, User) ->
    player:load(User);
load(#user.item, User) ->
    item:load(User);
load(_, User) ->
    User.