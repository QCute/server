%%%------------------------------------------------------------------
%%% @doc
%%% module role asset
%%% @end
%%%------------------------------------------------------------------
-module(asset).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([push/1]).
-export([add/2, cost/2]).
-export([check/2]).
-export([convert/1]).
%% Includes
-include("user.hrl").
-include("asset.hrl").
-include("protocol.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    case parser:convert(asset_sql:select(RoleId), ?MODULE) of
        [Asset] ->
            User#user{asset = Asset};
        [] ->
            %% new asset
            Asset = #asset{role_id = RoleId},
            asset_sql:insert(Asset),
            User#user{asset = Asset}
    end.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{asset = Asset}) ->
    asset_sql:update(Asset),
    User.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{asset = Asset}) ->
    {ok, Asset}.

%% @doc push
-spec push(User :: #user{}) -> ok().
push(User = #user{asset = Asset}) ->
    user_sender:send(User, ?PROTOCOL_ASSET, Asset).

%% @doc convert asset type to item type
-spec convert(AssetList :: [{Asset :: atom(), Number :: non_neg_integer()}]) -> [{non_neg_integer(), non_neg_integer()}].
convert(AssetList) ->
    %% asset must be to configure and number is a great then zero integer
    [{asset_data:get(Asset), Number} || {Asset, Number} <- AssetList, asset_data:get(Asset) =/= 0 andalso is_integer(Number) andalso Number > 0].

%% @doc only add assess
-spec add(User :: #user{}, Add :: [{Asset :: atom(), Number :: non_neg_integer()}]) -> {ok, NewUser :: #user{}} | {error, Asset :: atom()}.
add(User, []) ->
    {ok, User};
add(User = #user{asset = Asset = #asset{gold = Gold}}, [{gold, Number} | T]) ->
    {NewUser, NewNumber} = user_effect:act(User, add, asset, gold, Number),
    add(NewUser#user{asset = Asset#asset{gold = Gold + NewNumber}}, T);
add(User = #user{asset = Asset = #asset{silver = Silver}}, [{silver, Number} | T]) ->
    {NewUser, NewNumber} = user_effect:act(User, add, asset, silver, Number),
    add(NewUser#user{asset = Asset#asset{silver = Silver + NewNumber}}, T);
add(User = #user{asset = Asset = #asset{copper = Copper}}, [{copper, Number} | T]) ->
    {NewUser, NewNumber} = user_effect:act(User, add, asset, copper, Number),
    add(NewUser#user{asset = Asset#asset{copper = Copper + NewNumber}}, T);
add(User = #user{asset = Asset = #asset{coin = Coin}}, [{coin, Number} | T]) ->
    {NewUser, NewNumber} = user_effect:act(User, add, asset, coin, Number),
    add(NewUser#user{asset = Asset#asset{coin = Coin + NewNumber}}, T);
add(User = #user{asset = Asset = #asset{exp = Exp}}, [{exp, Number} | T]) ->
    {NewUser, NewNumber} = user_effect:act(User, add, asset, exp, Number),
    add(NewUser#user{asset = Asset#asset{exp = Exp + NewNumber}}, T);
add(_, [{Type, _} | _]) ->
    {error, Type}.

%% @doc only check assess
-spec check(User :: #user{}, Check :: [{Asset :: atom(), Number :: non_neg_integer()}]) -> ok | {error, Asset :: atom()}.
check(_, []) ->
    ok;
check(User = #user{asset = Asset = #asset{gold = Gold}}, [{gold, Number} | T]) ->
    case user_effect:act(User, reduce, asset, gold, Number) of
        {NewUser, NewNumber} when NewNumber =< Gold ->
            check(NewUser#user{asset = Asset#asset{gold = Gold - NewNumber}}, T);
        _ ->
            {error, gold}
    end;
check(User = #user{asset = Asset = #asset{silver = Silver}}, [{silver, Number} | T]) ->
    case user_effect:act(User, reduce, asset, silver, Number) of
        {NewUser, NewNumber} when NewNumber =< Silver ->
            check(NewUser#user{asset = Asset#asset{silver = Silver - NewNumber}}, T);
        _ ->
            {error, silver}
    end;
check(User = #user{asset = Asset = #asset{copper = Copper}}, [{copper, Number} | T]) when Number =< Copper ->
    case user_effect:act(User, reduce, asset, silver, Number) of
        {NewUser, NewNumber} when NewNumber =< Copper ->
            check(NewUser#user{asset = Asset#asset{copper = Copper - NewNumber}}, T);
        _ ->
            {error, copper}
    end;
check(User = #user{asset = Asset = #asset{coin = Coin}}, [{coin, Number} | T]) ->
    case user_effect:act(User, reduce, asset, silver, Number) of
        {NewUser, NewNumber} when NewNumber =< Coin ->
            check(NewUser#user{asset = Asset#asset{coin = Coin - NewNumber}}, T);
        _ ->
            {error, coin}
    end;
check(User = #user{asset = Asset = #asset{exp = Exp}}, [{exp, Number} | T]) ->
    case user_effect:act(User, reduce, asset, silver, Number) of
        {NewUser, NewNumber} when NewNumber =< Exp ->
            check(NewUser#user{asset = Asset#asset{exp = Exp - NewNumber}}, T);
        _ ->
            {error, exp}
    end;
check(_, [{Type, _} | _]) ->
    {error, Type}.


%% @doc only cost assess
-spec cost(User :: #user{}, Cost :: [{Asset :: atom(), Number :: non_neg_integer()}]) -> {ok, NewUser :: #user{}} | {error, Asset :: atom()}.
cost(User, []) ->
    {ok, User};
cost(User = #user{asset = Asset = #asset{gold = Gold}}, [{gold, Number} | T]) ->
    case user_effect:act(User, reduce, asset, gold, Number) of
        {NewUser, NewNumber} when NewNumber =< Gold ->
            cost(NewUser#user{asset = Asset#asset{gold = Gold - NewNumber}}, T);
        _ ->
            {error, gold}
    end;
cost(User = #user{asset = Asset = #asset{silver = Silver}}, [{silver, Number} | T]) ->
    case user_effect:act(User, reduce, asset, silver, Number) of
        {NewUser, NewNumber} when NewNumber =< Silver ->
            cost(NewUser#user{asset = Asset#asset{silver = Silver - NewNumber}}, T);
        _ ->
            {error, silver}
    end;
cost(User = #user{asset = Asset = #asset{copper = Copper}}, [{copper, Number} | T]) when Number =< Copper ->
    case user_effect:act(User, reduce, asset, silver, Number) of
        {NewUser, NewNumber} when NewNumber =< Copper ->
            cost(NewUser#user{asset = Asset#asset{copper = Copper - NewNumber}}, T);
        _ ->
            {error, copper}
    end;
cost(User = #user{asset = Asset = #asset{coin = Coin}}, [{coin, Number} | T]) ->
    case user_effect:act(User, reduce, asset, silver, Number) of
        {NewUser, NewNumber} when NewNumber =< Coin ->
            cost(NewUser#user{asset = Asset#asset{coin = Coin - NewNumber}}, T);
        _ ->
            {error, coin}
    end;
cost(User = #user{asset = Asset = #asset{exp = Exp}}, [{exp, Number} | T]) ->
    case user_effect:act(User, reduce, asset, silver, Number) of
        {NewUser, NewNumber} when NewNumber =< Exp ->
            cost(NewUser#user{asset = Asset#asset{exp = Exp - NewNumber}}, T);
        _ ->
            {error, exp}
    end;
cost(_, [{Type, _} | _]) ->
    {error, Type}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
