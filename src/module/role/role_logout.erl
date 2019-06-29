%%%-------------------------------------------------------------------
%%% @doc
%%% module user logout
%%% @end
%%%-------------------------------------------------------------------
-module(role_logout).
%% API
-export([logout/1]).
-export([save_loop/3]).
%% Includes
-include("user.hrl").
-include("role.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc save data on role logout
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
%% save per role's data
save(#user.role, User) ->
    role:save(User);
save(#user.assets, User) ->
    role_assets:save(User);
save(#user.item, User) ->
    item:save(User);
save(#user.quest, User) ->
    quest:save(User);
save(#user.shop, User) ->
    shop:save(User);
save(_, User) ->
    User.