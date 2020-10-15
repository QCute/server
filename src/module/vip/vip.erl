%%%-------------------------------------------------------------------
%%% @doc
%%% vip
%%% @end
%%%-------------------------------------------------------------------
-module(vip).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([handle_recharge/2]).
%% Includes
-include("event.hrl").
-include("user.hrl").
-include("recharge.hrl").
-include("vip.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    case vip_sql:select(RoleId) of
        [Vip] ->
            Vip;
        [] ->
            Vip = #vip{}
    end,
    user_event:add_trigger(User#user{vip = Vip}, #trigger{name = event_recharge, module = ?MODULE, function = handle_recharge}).

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
    {ok, Vip}.

%% @doc handle recharge
-spec handle_recharge(User :: #user{}, Event :: #event{}) -> ok().
handle_recharge(User = #user{role_id = RoleId, vip = Vip = #vip{exp = Exp}}, #event{target = RechargeId}) ->
    #recharge_data{exp = AddExp} = recharge_data:get(RechargeId),
    NewExp = Exp + AddExp,
    Level = vip_data:level(NewExp),
    {ok, User#user{vip = Vip#vip{role_id = RoleId, vip_level = Level, exp = NewExp}}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================