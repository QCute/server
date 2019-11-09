%%%------------------------------------------------------------------
%%% @doc
%%% module role vip
%%% @end
%%%------------------------------------------------------------------
-module(vip).
%% API
-export([load/1, save/1]).
-export([query/1]).
%% Includes
-include("user.hrl").
-include("vip.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    [Vip] = tool:default(parser:convert(vip_sql:select(RoleId), ?MODULE), [#vip{}]),
    User#user{vip = Vip}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{vip = #vip{role_id = 0}}) ->
    %% do not save it
    User;
save(User = #user{vip = Vip}) ->
    vip_sql:update(Vip),
    User.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{vip = Vip}) ->
    {ok, [Vip]}.

%%%==================================================================
%%% Internal functions
%%%==================================================================