%%%-------------------------------------------------------------------
%%% @doc
%%% charge
%%% @end
%%%-------------------------------------------------------------------
-module(charge).
%% API
-export([charge/2]).
%% Includes
-include("common.hrl").
-include("event.hrl").
-include("user.hrl").
-include("role.hrl").
-include("charge.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc charge
-spec charge(User :: #user{}, ChargeNo :: non_neg_integer()) -> ok() | error().
charge(User, ChargeNo) ->
    case charge_sql:select(ChargeNo) of
        [#charge{charge_id = ChargeId, status = ?FALSE}] ->
            add_gold(User, ChargeNo, ChargeId);
        [#charge{status = ?TRUE}] ->
            {error, gold_already_receive};
        _ ->
            {error, no_such_id}
    end.

add_gold(User, ChargeNo, ChargeId) ->
    case charge_data:get(ChargeId) of
        #charge_data{gold = Gold, gift_gold = GiftGold, now_price = NowPrice} ->
            %% update receive status
            charge_sql:update_status(?TRUE, ChargeNo),
            %% add asset gold
            {ok, NewUser} = asset:add(User, [{gold, Gold + GiftGold}], ?MODULE),
            update_statistics(NewUser, ChargeId, NowPrice);
        _ ->
            {error, configure_not_found}
    end.

update_statistics(User = #user{role = Role = #role{first_charge_time = 0, charge_total = ChargeTotal}}, ChargeId, Price) ->
    Now = time:now(),
    NewUser = User#user{role = Role#role{first_charge_time = Now, last_charge_time = Now, charge_total = ChargeTotal + Price}},
    %% trigger charge event
    {ok, user_event:trigger(NewUser, #event{name = event_charge, target = ChargeId})};
update_statistics(User = #user{role = Role = #role{charge_total = ChargeTotal}}, ChargeId, Price) ->
    Now = time:now(),
    NewUser = User#user{role = Role#role{last_charge_time = Now, charge_total = ChargeTotal + Price}},
    %% trigger charge event
    {ok, user_event:trigger(NewUser, #event{name = event_charge, target = ChargeId})}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
