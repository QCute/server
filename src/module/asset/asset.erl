%%%-------------------------------------------------------------------
%%% @doc
%%% module asset
%%% @end
%%%-------------------------------------------------------------------
-module(asset).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([push/1]).
-export([add/3, cost/3]).
-export([check/3]).
-export([convert/1]).
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
    [Asset] = tool:default(asset_sql:select(RoleId), [#asset{}]),
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
    user_sender:send(User, ?PROTOCOL_ASSET, Asset).

%% @doc convert asset type to item type
-spec convert(AssetList :: [{Asset :: atom(), Number :: non_neg_integer()}]) -> [{non_neg_integer(), non_neg_integer()}].
convert(AssetList) ->
    %% asset must be to configure and number is a great then zero integer
    [{asset_data:get(Asset), Number} || {Asset, Number} <- AssetList, asset_data:get(Asset) =/= 0 andalso is_integer(Number) andalso Number > 0].

%% @doc only add assess
-spec add(User :: #user{}, Add :: [{Asset :: atom(), Number :: non_neg_integer()}], From :: term()) -> {ok, NewUser :: #user{}} | {error, Asset :: atom()}.
add(User, [], _) ->
    {ok, User};
add(User = #user{asset = Asset = #asset{gold = Gold}}, [{gold, Number} | T], From) ->
    {NewUser, NewNumber} = user_effect:calculate(User, add, asset, gold, Number, From),
    FinalUser = user_event:handle(NewUser, #event{name = add_gold, target = gold, number = NewNumber}),
    add(FinalUser#user{asset = Asset#asset{gold = Gold + NewNumber}}, T, From);
add(User = #user{asset = Asset = #asset{silver = Silver}}, [{silver, Number} | T], From) ->
    {NewUser, NewNumber} = user_effect:calculate(User, add, asset, silver, Number, From),
    FinalUser = user_event:handle(NewUser, #event{name = add_silver, target = silver, number = NewNumber}),
    add(FinalUser#user{asset = Asset#asset{silver = Silver + NewNumber}}, T, From);
add(User = #user{asset = Asset = #asset{copper = Copper}}, [{copper, Number} | T], From) ->
    {NewUser, NewNumber} = user_effect:calculate(User, add, asset, copper, Number, From),
    FinalUser = user_event:handle(NewUser, #event{name = add_copper, target = copper, number = NewNumber}),
    add(FinalUser#user{asset = Asset#asset{copper = Copper + NewNumber}}, T, From);
add(User = #user{asset = Asset = #asset{coin = Coin}}, [{coin, Number} | T], From) ->
    {NewUser, NewNumber} = user_effect:calculate(User, add, asset, coin, Number, From),
    FinalUser = user_event:handle(NewUser, #event{name = add_coin, target = coin, number = NewNumber}),
    add(FinalUser#user{asset = Asset#asset{coin = Coin + NewNumber}}, T, From);
add(User = #user{asset = Asset = #asset{exp = Exp}}, [{exp, Number} | T], From) ->
    {NewUser, NewNumber} = user_effect:calculate(User, add, asset, exp, Number, From),
    FinalUser = user_event:handle(NewUser, #event{name = add_exp, target = exp, number = NewNumber}),
    add(FinalUser#user{asset = Asset#asset{exp = Exp + NewNumber}}, T, From);
add(_, [{Type, _} | _], _) ->
    {error, Type}.

%% @doc only check assess
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


%% @doc only cost assess
-spec cost(User :: #user{}, Cost :: [{Asset :: atom(), Number :: non_neg_integer()}], From :: term()) -> {ok, NewUser :: #user{}} | {error, Asset :: atom()}.
cost(User, [], _) ->
    {ok, User};
cost(User = #user{asset = Asset = #asset{gold = Gold}}, [{gold, Number} | T], From) ->
    case user_effect:calculate(User, reduce, asset, gold, Number, From) of
        {NewUser, NewNumber} when NewNumber =< Gold ->
            FinalUser = user_event:handle(NewUser, #event{name = cost_gold, target = gold, number = NewNumber}),
            cost(FinalUser#user{asset = Asset#asset{gold = Gold - NewNumber}}, T, From);
        _ ->
            {error, gold}
    end;
cost(User = #user{asset = Asset = #asset{silver = Silver}}, [{silver, Number} | T], From) ->
    case user_effect:calculate(User, reduce, asset, silver, Number, From) of
        {NewUser, NewNumber} when NewNumber =< Silver ->
            FinalUser = user_event:handle(NewUser, #event{name = cost_silver, target = silver, number = NewNumber}),
            cost(FinalUser#user{asset = Asset#asset{silver = Silver - NewNumber}}, T, From);
        _ ->
            {error, silver}
    end;
cost(User = #user{asset = Asset = #asset{copper = Copper}}, [{copper, Number} | T], From) ->
    case user_effect:calculate(User, reduce, asset, copper, Number, From) of
        {NewUser, NewNumber} when NewNumber =< Copper ->
            FinalUser = user_event:handle(NewUser, #event{name = cost_copper, target = copper, number = NewNumber}),
            cost(FinalUser#user{asset = Asset#asset{copper = Copper - NewNumber}}, T, From);
        _ ->
            {error, copper}
    end;
cost(User = #user{asset = Asset = #asset{coin = Coin}}, [{coin, Number} | T], From) ->
    case user_effect:calculate(User, reduce, asset, coin, Number, From) of
        {NewUser, NewNumber} when NewNumber =< Coin ->
            FinalUser = user_event:handle(NewUser, #event{name = cost_coin, target = coin, number = NewNumber}),
            cost(FinalUser#user{asset = Asset#asset{coin = Coin - NewNumber}}, T, From);
        _ ->
            {error, coin}
    end;
cost(User = #user{asset = Asset = #asset{exp = Exp}}, [{exp, Number} | T], From) ->
    case user_effect:calculate(User, reduce, asset, exp, Number, From) of
        {NewUser, NewNumber} when NewNumber =< Exp ->
            FinalUser = user_event:handle(NewUser, #event{name = cost_exp, target = exp, number = NewNumber}),
            cost(FinalUser#user{asset = Asset#asset{exp = Exp - NewNumber}}, T, From);
        _ ->
            {error, exp}
    end;
cost(_, [{Type, _} | _], _) ->
    {error, Type}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
