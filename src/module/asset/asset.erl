%%%-------------------------------------------------------------------
%%% @doc
%%% number type asset, add/check/cost
%%% @end
%%%-------------------------------------------------------------------
-module(asset).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([push/1]).
-export([convert/1]).
-export([add/3, check/3, cost/3]).
%% field exports
-export([gold/1, silver/1, copper/1, coin/1, exp/1]).
%% Includes
-include("protocol.hrl").
-include("event.hrl").
-include("user.hrl").
-include("asset.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    case asset_sql:select(RoleId) of
        [Asset] ->
            Asset;
        [] ->
            Asset = #asset{}
    end,
    User#user{asset = Asset}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{role_id = RoleId, asset = Asset = #asset{role_id = 0}}) ->
    NewAsset = Asset#asset{role_id = RoleId},
    %% insert new
    asset_sql:insert(NewAsset),
    User#user{asset = NewAsset};
save(User = #user{asset = Asset}) ->
    asset_sql:update(Asset),
    User.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{asset = Asset}) ->
    {ok, Asset}.

%% @doc push
-spec push(User :: #user{}) -> ok.
push(User = #user{asset = Asset}) ->
    user_sender:send(User, ?PROTOCOL_ROLE_ASSET_QUERY, Asset).

%% @doc convert asset type to item type
-spec convert(AssetList :: [{Asset :: atom(), Number :: non_neg_integer()}]) -> [{non_neg_integer(), non_neg_integer()}].
convert(AssetList) ->
    %% asset must be to configure and number is a great then zero integer
    [{asset_data:get(Asset), Number} || {Asset, Number} <- AssetList, asset_data:get(Asset) =/= 0 andalso is_integer(Number) andalso Number > 0].

%% @doc only add asset
-spec add(User :: #user{}, Add :: [{Asset :: atom(), Number :: non_neg_integer()}], From :: term()) -> {ok, NewUser :: #user{}} | {error, Asset :: atom()}.
add(User, [], _) ->
    %% push after add
    push(User),
    {ok, User};
add(User = #user{asset = Asset = #asset{gold = Gold}}, [{gold, Number} | T], From) ->
    {NewUser, NewNumber} = user_effect:calculate(User, add, asset, gold, Number, From),
    FinalUser = user_event:trigger(NewUser, #event{name = event_gold_add, target = gold, number = NewNumber}),
    add(FinalUser#user{asset = Asset#asset{gold = Gold + NewNumber}}, T, From);
add(User = #user{asset = Asset = #asset{silver = Silver}}, [{silver, Number} | T], From) ->
    {NewUser, NewNumber} = user_effect:calculate(User, add, asset, silver, Number, From),
    FinalUser = user_event:trigger(NewUser, #event{name = event_silver_add, target = silver, number = NewNumber}),
    add(FinalUser#user{asset = Asset#asset{silver = Silver + NewNumber}}, T, From);
add(User = #user{asset = Asset = #asset{copper = Copper}}, [{copper, Number} | T], From) ->
    {NewUser, NewNumber} = user_effect:calculate(User, add, asset, copper, Number, From),
    FinalUser = user_event:trigger(NewUser, #event{name = event_copper_add, target = copper, number = NewNumber}),
    add(FinalUser#user{asset = Asset#asset{copper = Copper + NewNumber}}, T, From);
add(User = #user{asset = Asset = #asset{coin = Coin}}, [{coin, Number} | T], From) ->
    {NewUser, NewNumber} = user_effect:calculate(User, add, asset, coin, Number, From),
    FinalUser = user_event:trigger(NewUser, #event{name = event_coin_add, target = coin, number = NewNumber}),
    add(FinalUser#user{asset = Asset#asset{coin = Coin + NewNumber}}, T, From);
add(User = #user{asset = Asset = #asset{exp = Exp}}, [{exp, Number} | T], From) ->
    {NewUser, NewNumber} = user_effect:calculate(User, add, asset, exp, Number, From),
    FinalUser = user_event:trigger(NewUser, #event{name = event_exp_add, target = exp, number = NewNumber}),
    add(FinalUser#user{asset = Asset#asset{exp = Exp + NewNumber}}, T, From);
add(_, [{Type, _} | _], _) ->
    {error, Type}.

%% @doc only check asset
-spec check(User :: #user{}, Check :: [{Asset :: atom(), Number :: non_neg_integer()}], From :: term()) -> ok | {error, Asset :: atom()}.
check(_, [], _) ->
    ok;
check(User = #user{asset = Asset = #asset{gold = Gold}}, [{gold, Number} | T], From) ->
    case Number =< Gold of
        true ->
            check(User#user{asset = Asset#asset{gold = Gold - Number}}, T, From);
        _ ->
            {error, gold}
    end;
check(User = #user{asset = Asset = #asset{silver = Silver}}, [{silver, Number} | T], From) ->
    case Number =< Silver of
        true ->
            check(User#user{asset = Asset#asset{silver = Silver - Number}}, T, From);
        _ ->
            {error, silver}
    end;
check(User = #user{asset = Asset = #asset{copper = Copper}}, [{copper, Number} | T], From) ->
    case Number =< Copper of
        true ->
            check(User#user{asset = Asset#asset{copper = Copper - Number}}, T, From);
        _ ->
            {error, copper}
    end;
check(User = #user{asset = Asset = #asset{coin = Coin}}, [{coin, Number} | T], From) ->
    case Number =< Coin of
        true ->
            check(User#user{asset = Asset#asset{coin = Coin - Number}}, T, From);
        _ ->
            {error, coin}
    end;
check(User = #user{asset = Asset = #asset{exp = Exp}}, [{exp, Number} | T], From) ->
    case Number =< Exp of
        true ->
            check(User#user{asset = Asset#asset{exp = Exp - Number}}, T, From);
        _ ->
            {error, exp}
    end;
check(_, [{Type, _} | _], _) ->
    {error, Type}.

%% @doc only cost asset
-spec cost(User :: #user{}, Cost :: [{Asset :: atom(), Number :: non_neg_integer()}], From :: term()) -> {ok, NewUser :: #user{}} | {error, Asset :: atom()}.
cost(User, [], _) ->
    %% push after cost
    push(User),
    {ok, User};
cost(User = #user{asset = Asset = #asset{gold = Gold}}, [{gold, Number} | T], From) ->
    case user_effect:calculate(User, reduce, asset, gold, Number, From) of
        {NewUser, NewNumber} when NewNumber =< Gold ->
            FinalUser = user_event:trigger(NewUser, #event{name = event_gold_cost, target = gold, number = NewNumber}),
            cost(FinalUser#user{asset = Asset#asset{gold = Gold - NewNumber}}, T, From);
        _ ->
            {error, gold}
    end;
cost(User = #user{asset = Asset = #asset{silver = Silver}}, [{silver, Number} | T], From) ->
    case user_effect:calculate(User, reduce, asset, silver, Number, From) of
        {NewUser, NewNumber} when NewNumber =< Silver ->
            FinalUser = user_event:trigger(NewUser, #event{name = event_silver_cost, target = silver, number = NewNumber}),
            cost(FinalUser#user{asset = Asset#asset{silver = Silver - NewNumber}}, T, From);
        _ ->
            {error, silver}
    end;
cost(User = #user{asset = Asset = #asset{copper = Copper}}, [{copper, Number} | T], From) ->
    case user_effect:calculate(User, reduce, asset, copper, Number, From) of
        {NewUser, NewNumber} when NewNumber =< Copper ->
            FinalUser = user_event:trigger(NewUser, #event{name = event_copper_cost, target = copper, number = NewNumber}),
            cost(FinalUser#user{asset = Asset#asset{copper = Copper - NewNumber}}, T, From);
        _ ->
            {error, copper}
    end;
cost(User = #user{asset = Asset = #asset{coin = Coin}}, [{coin, Number} | T], From) ->
    case user_effect:calculate(User, reduce, asset, coin, Number, From) of
        {NewUser, NewNumber} when NewNumber =< Coin ->
            FinalUser = user_event:trigger(NewUser, #event{name = event_coin_cost, target = coin, number = NewNumber}),
            cost(FinalUser#user{asset = Asset#asset{coin = Coin - NewNumber}}, T, From);
        _ ->
            {error, coin}
    end;
cost(User = #user{asset = Asset = #asset{exp = Exp}}, [{exp, Number} | T], From) ->
    case user_effect:calculate(User, reduce, asset, exp, Number, From) of
        {NewUser, NewNumber} when NewNumber =< Exp ->
            FinalUser = user_event:trigger(NewUser, #event{name = event_exp_cost, target = exp, number = NewNumber}),
            cost(FinalUser#user{asset = Asset#asset{exp = Exp - NewNumber}}, T, From);
        _ ->
            {error, exp}
    end;
cost(_, [{Type, _} | _], _) ->
    {error, Type}.

%%%===================================================================
%%% field functions
%%%===================================================================
%% @doc gold
-spec gold(#user{} | #asset{}) -> non_neg_integer().
gold(#user{asset = #asset{gold = Gold}}) ->
    Gold;
gold(#asset{gold = Gold}) ->
    Gold.

%% @doc silver
-spec silver(#user{} | #asset{}) -> non_neg_integer().
silver(#user{asset = #asset{silver = Silver}}) ->
    Silver;
silver(#asset{silver = Silver}) ->
    Silver.

%% @doc copper
-spec copper(#user{} | #asset{}) -> non_neg_integer().
copper(#user{asset = #asset{copper = Copper}}) ->
    Copper;
copper(#asset{copper = Copper}) ->
    Copper.

%% @doc coin
-spec coin(#user{} | #asset{}) -> non_neg_integer().
coin(#user{asset = #asset{coin = Coin}}) ->
    Coin;
coin(#asset{coin = Coin}) ->
    Coin.

%% @doc exp
-spec exp(#user{} | #asset{}) -> non_neg_integer().
exp(#user{asset = #asset{exp = Exp}}) ->
    Exp;
exp(#asset{exp = Exp}) ->
    Exp.

%%%===================================================================
%%% Internal functions
%%%===================================================================
