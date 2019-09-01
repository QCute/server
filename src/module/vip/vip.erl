%%%-------------------------------------------------------------------
%%% @doc
%%% module role vip
%%% @end
%%%-------------------------------------------------------------------
-module(vip).
%% API
-export([load/1, save/1]).
-export([query/1]).
%% Includes
-include("user.hrl").
-include("vip.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    case parser:convert(vip_sql:select(RoleId), ?MODULE) of
        [] ->
            %% new data
            Vip = #vip{role_id = RoleId};
        [Vip] ->
            Vip
    end,
    User#user{vip = Vip}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{vip = Vip}) ->
    vip_sql:update(Vip),
    User.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{vip = Vip}) ->
    {ok, [Vip]}.

%%%===================================================================
%%% Internal functions
%%%===================================================================