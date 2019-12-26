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
    user_sender:send(User, ?PROTOCOL_ASSET, [Asset]).

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
    add(User#user{asset = Asset#asset{gold = Gold + Number}}, T);
add(User = #user{asset = Asset = #asset{silver = Silver}}, [{silver, Number} | T]) ->
    add(User#user{asset = Asset#asset{silver = Silver + Number}}, T);
add(User = #user{asset = Asset = #asset{copper = Copper}}, [{copper, Number} | T]) ->
    add(User#user{asset = Asset#asset{copper = Copper + Number}}, T);
add(User = #user{asset = Asset = #asset{exp = Exp}}, [{exp, Number} | T]) ->
    add(User#user{asset = Asset#asset{exp = Exp + Number}}, T);
add(User = #user{asset = Asset = #asset{coin = Coin}}, [{coin, Number} | T]) ->
    add(User#user{asset = Asset#asset{coin = Coin + Number}}, T);
add(_, [{Type, _} | _]) ->
    {error, Type}.

%% @doc only check assess
-spec check(User :: #user{}, Check :: [{Asset :: atom(), Number :: non_neg_integer()}]) -> {ok, NewUser :: #user{}} | {error, Asset :: atom()}.
check(_, []) ->
    ok;
check(User = #user{asset = #asset{gold = Gold}}, [{gold, Number} | T]) when Number =< Gold ->
    check(User, T);
check(User = #user{asset = #asset{silver = Silver}}, [{silver, Number} | T]) when Number =< Silver ->
    check(User, T);
check(User = #user{asset = #asset{copper = Copper}}, [{copper, Number} | T]) when Number =< Copper ->
    check(User, T);
check(User = #user{asset = #asset{exp = Exp}}, [{exp, Number} | T]) when Number =< Exp ->
    check(User, T);
check(User = #user{asset = #asset{coin = Coin}}, [{coin, Number} | T]) when Number =< Coin ->
    check(User, T);
check(_, [{Type, _} | _]) ->
    {error, Type}.

%% @doc only cost assess
-spec cost(User :: #user{}, Cost :: [{Asset :: atom(), Number :: non_neg_integer()}]) -> {ok, NewUser :: #user{}} | {error, Asset :: atom()}.
cost(User, []) ->
    {ok, User};
cost(User = #user{asset = Asset = #asset{gold = Gold}}, [{gold, Number} | T]) when Number =< Gold ->
    cost(User#user{asset = Asset#asset{gold = Gold - Number}}, T);
cost(User = #user{asset = Asset = #asset{silver = Silver}}, [{silver, Number} | T]) when Number =< Silver ->
    cost(User#user{asset = Asset#asset{silver = Silver - Number}}, T);
cost(User = #user{asset = Asset = #asset{copper = Copper}}, [{copper, Number} | T]) when Number =< Copper ->
    cost(User#user{asset = Asset#asset{copper = Copper - Number}}, T);
cost(User = #user{asset = Asset = #asset{exp = Exp}}, [{exp, Number} | T]) when Number =< Exp ->
    cost(User#user{asset = Asset#asset{exp = Exp - Number}}, T);
cost(User = #user{asset = Asset = #asset{coin = Coin}}, [{coin, Number} | T]) when Number =< Coin ->
    cost(User#user{asset = Asset#asset{coin = Coin - Number}}, T);
cost(_, [{Type, _} | _]) ->
    {error, Type}.

%%%==================================================================
%%% Internal functions
%%%==================================================================