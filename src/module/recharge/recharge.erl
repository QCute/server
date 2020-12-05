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
-include("role.hrl").
-include("recharge.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc recharge
-spec recharge(User :: #user{}, RechargeNo :: non_neg_integer()) -> ok() | error().
recharge(User, RechargeNo) ->
    case recharge_sql:select(RechargeNo) of
        [#recharge{recharge_id = RechargeId, status = 0}] ->
            add_gold(User, RechargeNo, RechargeId);
        [#recharge{status = 1}] ->
            {error, gold_already_receive};
        _ ->
            {error, no_such_id}
    end.

add_gold(User, RechargeNo, RechargeId) ->
    case recharge_data:get(RechargeId) of
        #recharge_data{gold = Gold, gift_gold = GiftGold, now_price = NowPrice} ->
            %% update receive status
            recharge_sql:update_status(1, RechargeNo),
            %% add asset gold
            {ok, NewUser} = asset:add(User, [{gold, Gold + GiftGold}], ?MODULE),
            update_statistics(NewUser, RechargeId, NowPrice);
        _ ->
            {error, configure_not_found}
    end.

update_statistics(User = #user{role = Role = #role{first_recharge_time = 0, recharge_total = RechargeTotal}}, RechargeId, Price) ->
    Now = time:now(),
    NewUser = User#user{role = Role#role{first_recharge_time = Now, last_recharge_time = Now, recharge_total = RechargeTotal + Price}},
    %% trigger recharge event
    {ok, user_event:trigger(NewUser, #event{name = event_recharge, target = RechargeId})};
update_statistics(User = #user{role = Role = #role{recharge_total = RechargeTotal}}, RechargeId, Price) ->
    Now = time:now(),
    NewUser = User#user{role = Role#role{last_recharge_time = Now, recharge_total = RechargeTotal + Price}},
    %% trigger recharge event
    {ok, user_event:trigger(NewUser, #event{name = event_recharge, target = RechargeId})}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
