%%%-------------------------------------------------------------------
%%% @doc
%%% module user login
%%% @end
%%%-------------------------------------------------------------------
-module(role_login).
%% API
-export([login/1]).
-export([load_loop/3]).
%% Includes
-include("user.hrl").
-include("role.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load data on role login
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
%% load per role's data
load(#user.role, User) ->
    role:load(User);
load(#user.assets, User) ->
    role_assets:load(User);
load(#user.item, User) ->
    item:load(User);
load(#user.quest, User) ->
    quest:load(User);
load(#user.mail, User) ->
    mail:load(User);
load(#user.shop, User) ->
    shop:load(User);
load(_, User) ->
    User.