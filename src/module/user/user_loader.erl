%%%-------------------------------------------------------------------
%%% @doc
%%% module user data loader
%%% @end
%%%-------------------------------------------------------------------
-module(user_loader).
%% API
-export([load/1]).
-export([load_loop/3]).
%% Includes
-include("user.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load data on role login
load(User) ->
    Size = tuple_size(User),
    load_loop(2, Size, User).

%% @doc load loop
load_loop(Size, Size, User) ->
    do_load(Size, User);
load_loop(Position, Size, User) ->
    NewUser = do_load(Position, User),
    load_loop(Position + 1, Size, NewUser).
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% load per role's data
do_load(#user.role, User) ->
    role:load(User);
do_load(#user.asset, User) ->
    asset:load(User);
do_load(#user.vip, User) ->
    vip:load(User);
do_load(#user.item, User) ->
    item:load(User);
do_load(#user.quest, User) ->
    quest:load(User);
do_load(#user.mail, User) ->
    mail:load(User);
do_load(#user.friend, User) ->
    friend:load(User);
do_load(#user.shop, User) ->
    shop:load(User);
do_load(#user.buff, User) ->
    buff:load(User);
do_load(#user.skill, User) ->
    skill:load(User);
do_load(_, User) ->
    User.
