%%%-------------------------------------------------------------------
%%% @doc
%%% module role asset
%%% @end
%%%-------------------------------------------------------------------
-module(asset).
%% API
-export([load/1, save/1]).
-export([query/1]).
-export([add/2, cost/2]).
-export([check/2]).
-export([convert/1]).
%% Includes
-include("user.hrl").
-include("asset.hrl").
%%%===================================================================
%%% API
%%%===================================================================
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
    {ok, [Asset]}.

%% @doc convert asset type to item type
-spec convert(AssetList :: [{Asset :: atom(), Amount :: non_neg_integer()}]) -> [{non_neg_integer(), non_neg_integer()}].
convert(AssetList) ->
    %% asset must be to configure and amount is a great then zero integer
    [{asset_data:get(Asset), Amount} || {Asset, Amount} <- AssetList, asset_data:get(Asset) =/= 0 andalso is_integer(Amount) andalso Amount > 0].

%% @doc only add assess
-spec add(User :: #user{}, Add :: [{Asset :: atom(), Amount :: non_neg_integer()}]) -> {ok, NewUser :: #user{}} | {error, Asset :: atom()}.
add(User, []) ->
    {ok, User};
add(User = #user{asset = Asset = #asset{gold = Gold}}, [{gold, Amount} | T]) ->
    add(User#user{asset = Asset#asset{gold = Gold + Amount}}, T);
add(User = #user{asset = Asset = #asset{silver = Silver}}, [{silver, Amount} | T]) ->
    add(User#user{asset = Asset#asset{silver = Silver + Amount}}, T);
add(User = #user{asset = Asset = #asset{copper = Copper}}, [{copper, Amount} | T]) ->
    add(User#user{asset = Asset#asset{copper = Copper + Amount}}, T);
add(User = #user{asset = Asset = #asset{exp = Exp}}, [{exp, Amount} | T]) ->
    add(User#user{asset = Asset#asset{exp = Exp + Amount}}, T);
add(User = #user{asset = Asset = #asset{coin = Coin}}, [{coin, Amount} | T]) ->
    add(User#user{asset = Asset#asset{coin = Coin + Amount}}, T);
add(_, [{Type, _} | _]) ->
    {error, Type}.

%% @doc only check assess
-spec check(User :: #user{}, Check :: [{Asset :: atom(), Amount :: non_neg_integer()}]) -> {ok, NewUser :: #user{}} | {error, Asset :: atom()}.
check(User = #user{asset = #asset{gold = Gold}}, [{gold, Amount} | T]) when Amount =< Gold ->
    check(User, T);
check(User = #user{asset = #asset{silver = Silver}}, [{silver, Amount} | T]) when Amount =< Silver ->
    check(User, T);
check(User = #user{asset = #asset{copper = Copper}}, [{copper, Amount} | T]) when Amount =< Copper ->
    check(User, T);
check(User = #user{asset = #asset{exp = Exp}}, [{exp, Amount} | T]) when Amount =< Exp ->
    check(User, T);
check(User = #user{asset = #asset{coin = Coin}}, [{coin, Amount} | T]) when Amount =< Coin ->
    check(User, T);
check(_, [{Type, _} | _]) ->
    {error, Type}.

%% @doc only cost assess
-spec cost(User :: #user{}, Cost :: [{Asset :: atom(), Amount :: non_neg_integer()}]) -> {ok, NewUser :: #user{}} | {error, Asset :: atom()}.
cost(User, []) ->
    {ok, User};
cost(User = #user{asset = Asset = #asset{gold = Gold}}, [{gold, Amount} | T]) when Amount =< Gold ->
    cost(User#user{asset = Asset#asset{gold = Gold - Amount}}, T);
cost(User = #user{asset = Asset = #asset{silver = Silver}}, [{silver, Amount} | T]) when Amount =< Silver ->
    cost(User#user{asset = Asset#asset{silver = Silver - Amount}}, T);
cost(User = #user{asset = Asset = #asset{copper = Copper}}, [{copper, Amount} | T]) when Amount =< Copper ->
    cost(User#user{asset = Asset#asset{copper = Copper - Amount}}, T);
cost(User = #user{asset = Asset = #asset{exp = Exp}}, [{exp, Amount} | T]) ->
    cost(User#user{asset = Asset#asset{exp = Exp - Amount}}, T);
cost(User = #user{asset = Asset = #asset{coin = Coin}}, [{coin, Amount} | T]) ->
    cost(User#user{asset = Asset#asset{coin = Coin - Amount}}, T);
cost(_, [{Type, _} | _]) ->
    {error, Type}.

%%%===================================================================
%%% Internal functions
%%%===================================================================