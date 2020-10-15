%%%-------------------------------------------------------------------
%%% @doc
%%% recharge
%%% @end
%%%-------------------------------------------------------------------
-module(recharge).
%% API
-export([recharge/2]).
%% Includes
-include("common.hrl").
-include("event.hrl").
-include("user.hrl").
-include("recharge.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc recharge
-spec recharge(User :: #user{}, RechargeNo :: non_neg_integer()) -> ok() | error().
recharge(User, RechargeNo) ->
    case recharge_sql:select(RechargeNo) of
        [#recharge{recharge_id = RechargeId, status = 0}] ->
            case recharge_data:get(RechargeId) of
                #recharge_data{gold = Gold} ->
                    recharge_sql:update_status(1, RechargeNo),
                    {ok, NewUser} = asset:add(User, [{gold, Gold}], ?MODULE),
                    {ok, user_event:trigger(NewUser, #event{name = event_recharge, target = RechargeId})};
                _ ->
                    {error, configure_not_found}
            end;
        [#recharge{status = 1}] ->
            {error, gold_already_receive};
        _ ->
            {error, no_such_id}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
