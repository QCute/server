%%%-------------------------------------------------------------------
%%% @doc
%%% module user clear
%%% @end
%%%-------------------------------------------------------------------
-module(role_clear).
%% API
-export([clear/1]).
-export([clear_loop/3]).
%% Includes
-include("user.hrl").
-include("role.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc save data on role logout
clear(User) ->
    Size = tuple_size(User),
    clear_loop(2, Size, User).

%% @doc save loop
clear_loop(Size, Size, User) ->
    clear(Size, User);
clear_loop(Position, Size, User) ->
    NewUser = clear(Position, User),
    clear_loop(Position + 1, Size, NewUser).
%%%===================================================================
%%% Internal functions
%%%===================================================================
%% clear per role's data

clear(_, User) ->
    User.