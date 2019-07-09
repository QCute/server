%%%-------------------------------------------------------------------
%%% @doc
%%% module role vip
%%% @end
%%%-------------------------------------------------------------------
-module(vip).
%% API
-export([load/1, save/1]).
%% Includes
-include("user.hrl").
-include("role.hrl").
-include("vip.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Data =  vip_sql:select(RoleId),
    case parser:convert(Data, vip) of
        [] ->
            %% new data
            Vip = #vip{role_id = RoleId},
            vip_sql:insert(Vip);
        [Vip] ->
            Vip
    end,
    User#user{vip = Vip}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{vip = Vip}) ->
    vip_sql:update(Vip),
    User.
%%%===================================================================
%%% Internal functions
%%%===================================================================