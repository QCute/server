%%%-------------------------------------------------------------------
%%% @doc
%%% module role
%%% @end
%%%-------------------------------------------------------------------
-module(role).
%% API
-export([load/1, save/1]).
-export([push/1]).
%% Includes
-include("user.hrl").
-include("role.hrl").
-include("asset.hrl").
-include("vip.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load data
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    [Role] = parser:convert(role_sql:select(RoleId), ?MODULE),
    User#user{role = Role}.

%% @doc save data
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{role = Role}) ->
    role_sql:update(Role),
    User.

%% @doc push
-spec push(User :: #user{}) -> {reply, list()}.
push(#user{role = Role}) ->
    {reply, [Role]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
