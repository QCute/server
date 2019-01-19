%%%-------------------------------------------------------------------
%%% @doc
%%% module user logout
%%% @end
%%%-------------------------------------------------------------------
-module(player_logout).
-include("player.hrl").
%% API
-export([logout/1]).
-export([save_loop/3]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc save data on player logout
logout(User) ->
    Size = tuple_size(User),
    save_loop(2, Size, User).

%% @doc save loop
save_loop(Size, Size, User) ->
    save(Size, User);
save_loop(Position, Size, User) ->
    NewUser = save(Position, User),
    save_loop(Position + 1, Size, NewUser).
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% save per player's data
save(#user.player, User) ->
    player:save(User);
save(#user.item, User) ->
    item:save(User);
save(#user.quest, User) ->
    quest:save(User);
save(_, User) ->
    User.