%%%-------------------------------------------------------------------
%%% @doc
%%% vip
%%% @end
%%%-------------------------------------------------------------------
-module(vip).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([handle_event_charge/2]).
%% Includes
-include("event.hrl").
-include("user.hrl").
-include("charge.hrl").
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
    {ok, Vip}.

%% @doc upgrade level after charge
-spec handle_event_charge(User :: #user{}, Event :: #event{}) -> #user{}.
handle_event_charge(User = #user{role_id = RoleId, vip = Vip = #vip{vip_level = VipLevel, exp = Exp}}, #event{target = ChargeId}) ->
    #charge_data{exp = AddExp} = charge_data:get(ChargeId),
    NewExp = Exp + AddExp,
    NewLevel = vip_data:level(NewExp),
    NewVip = Vip#vip{role_id = RoleId, vip_level = NewLevel, exp = NewExp},
    NewUser = User#user{vip = NewVip},
    case VipLevel < NewLevel of
        true ->
            user_event:trigger(NewUser, #event{name = event_vip_upgrade, target = NewLevel});
        false ->
            NewUser
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================