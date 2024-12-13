%%%-------------------------------------------------------------------
%%% @doc
%%% event dispatcher
%%% @end
%%%-------------------------------------------------------------------
-module(dispatcher).
%% API
-export([trigger/2]).
%% Includes
-include("event.hrl").
-include("user.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc trigger static event
-spec trigger(User :: #user{}, Event :: #event{}) -> NewUser :: #user{}.
%% trigger static event @here
%% auto generate, do not edit this code

trigger(User, Event = #event{name = charge}) ->
    CountUser = count:on_charge(User, Event),
    vip:on_charge(CountUser, Event);
trigger(User, _Event = #event{name = exp_add}) ->
    role:on_exp_add(User);
trigger(User, Event = #event{name = shop_buy}) ->
    count:on_shop_buy(User, Event);
trigger(User, Event = #event{name = gold_cost}) ->
    count:on_gold_cost(User, Event);
trigger(User, _) ->
    User.

%%%===================================================================
%%% Internal functions
%%%===================================================================
