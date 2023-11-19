%%%-------------------------------------------------------------------
%%% @doc
%%% charge
%%% @end
%%%-------------------------------------------------------------------
-module(charge).
%% API
-export([on_load/1, on_save/1, on_reset/1]).
-export([callback/2]).
%% Includes
-include("common.hrl").
-include("event.hrl").
-include("user.hrl").
-include("charge.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc on load
-spec on_load(User :: #user{}) -> NewUser :: #user{}.
on_load(User = #user{role_id = RoleId}) ->
    case charge_sql:select(RoleId) of
        [Charge] ->
            Charge;
        [] ->
            Charge = #charge{}
    end,
    User#user{charge = Charge}.

%% @doc on save
-spec on_save(User :: #user{}) -> NewUser :: #user{}.
on_save(User = #user{role_id = RoleId, charge = Charge = #charge{role_id = 0}}) ->
    NewCharge = Charge#charge{role_id = RoleId},
    %% insert new
    charge_sql:insert(NewCharge),
    User#user{charge = NewCharge};
on_save(User = #user{charge = Charge}) ->
    charge_sql:update(Charge),
    User.

%% @doc on reset
-spec on_reset(User :: #user{}) -> NewUser :: #user{}.
on_reset(User = #user{charge = Charge}) ->
    case time:weekday() of
        1 ->
            WeeklyTotal = 0;
        _ ->
            WeeklyTotal = Charge#charge.weekly_total
    end,
    case time:date() of
        1 ->
            MonthlyTotal = 0;
        _ ->
            MonthlyTotal = Charge#charge.monthly_total
    end,
    NewCharge = Charge#charge{daily_total = 0, weekly_total = WeeklyTotal, monthly_total = MonthlyTotal},
    User#user{charge = NewCharge}.

%% @doc charge callback
-spec callback(User :: #user{}, ChargeNo :: non_neg_integer()) -> ok() | error().
callback(User, ChargeNo) ->
    case charge_order_sql:select(ChargeNo) of
        [ChargeOrder = #charge_order{charge_id = ChargeId, status = ?FALSE}] ->
            add_gold(User, ChargeOrder, ChargeId);
        [#charge_order{status = ?TRUE}] ->
            {error, gold_already_receive};
        _ ->
            {error, no_such_id}
    end.

add_gold(User, ChargeOrder, ChargeId) ->
    case charge_data:get(ChargeId) of
        #charge_data{gold = Gold, gift_gold = GiftGold, now_price = NowPrice} ->
            %% update receive status
            NewChargeChargeOrder = ChargeOrder#charge_order{status = ?TRUE},
            charge_order_sql:update_status(NewChargeChargeOrder),
            %% add asset gold
            {ok, NewUser} = asset:add(User, [{gold, Gold + GiftGold}], ?MODULE),
            update_statistics(NewUser, ChargeId, NowPrice);
        _ ->
            {error, configure_not_found}
    end.

update_statistics(User = #user{charge = Charge = #charge{first_charge_time = 0, daily_total = DailyTotal, weekly_total = WeeklyTotal, monthly_total = MonthlyTotal, charge_total = ChargeTotal}}, ChargeId, Price) ->
    Now = time:now(),
    NewCharge = Charge#charge{
        first_charge_time = Now,
        last_charge_time = Now,
        daily_total = DailyTotal + Price,
        weekly_total = WeeklyTotal + Price,
        monthly_total = MonthlyTotal + Price,
        charge_total = ChargeTotal + Price
    },
    NewUser = User#user{charge = NewCharge},
    %% trigger charge event
    {ok, user_event:trigger(NewUser, #event{name = charge, target = ChargeId})};
update_statistics(User = #user{charge = Charge = #charge{daily_total = DailyTotal, weekly_total = WeeklyTotal, monthly_total = MonthlyTotal, charge_total = ChargeTotal}}, ChargeId, Price) ->
    Now = time:now(),
    NewCharge = Charge#charge{
        last_charge_time = Now,
        daily_total = DailyTotal + Price,
        weekly_total = WeeklyTotal + Price,
        monthly_total = MonthlyTotal + Price,
        charge_total = ChargeTotal + Price
    },
    NewUser = User#user{charge = NewCharge},
    %% trigger charge event
    {ok, user_event:trigger(NewUser, #event{name = charge, target = ChargeId})}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
