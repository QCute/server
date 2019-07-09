%%%-------------------------------------------------------------------
%%% @doc
%%% module role asset
%%% @end
%%%-------------------------------------------------------------------
-module(asset).
%% API
-export([load/1, save/1]).
-export([add/2, cost/2]).
%% Includes
-include("user.hrl").
-include("role.hrl").
-include("asset.hrl").
-include("vip.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Data =  asset_sql:select(RoleId),
    case parser:convert(Data, asset) of
        [] ->
            %% new data
            Asset = #asset{role_id = RoleId},
            asset_sql:insert(Asset);
        [Asset] ->
            Asset
    end,
    User#user{asset = Asset}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{asset = Asset}) ->
    asset_sql:update(Asset),
    User.

%% @doc only add assess
-spec add(User :: #user{}, CostList :: list()) -> {ok, NewUser :: #user{}} | {error, non_neg_integer()}.
add(User, []) ->
    {ok, User};
add(User = #user{asset = Asset = #asset{gold = Gold}}, [{gold, Add, _} | T]) ->
    add(User#user{asset = Asset#asset{gold = Gold + Add}}, T);
add(User = #user{asset = Asset = #asset{silver = Silver}}, [{silver, Add, _} | T]) ->
    add(User#user{asset = Asset#asset{silver = Silver + Add}}, T);
add(User = #user{asset = Asset = #asset{copper = Copper}}, [{copper, Add, _} | T]) ->
    add(User#user{asset = Asset#asset{copper = Copper + Add}}, T);

add(_, [_ | _]) ->
    {error, 0}.

%% @doc only cost assess
-spec cost(User :: #user{}, CostList :: list()) -> {ok, NewUser :: #user{}} | {error, non_neg_integer()}.
cost(User, []) ->
    {ok, User};
cost(User = #user{asset = Asset = #asset{gold = Gold}}, [{gold, Cost} | T]) when Cost =< Gold ->
    cost(User#user{asset = Asset#asset{gold = Gold - Cost}}, T);
cost(User = #user{asset = Asset = #asset{silver = Silver}}, [{silver, Cost} | T]) when Cost =< Silver ->
    cost(User#user{asset = Asset#asset{silver = Silver - Cost}}, T);
cost(User = #user{asset = Asset = #asset{copper = Copper}}, [{copper, Cost} | T]) when Cost =< Copper ->
    cost(User#user{asset = Asset#asset{copper = Copper - Cost}}, T);

cost(User = #user{asset = Asset = #asset{gold = Gold}}, [{gold, Cost, _} | T]) when Cost =< Gold ->
    cost(User#user{asset = Asset#asset{gold = Gold - Cost}}, T);
cost(User = #user{asset = Asset = #asset{silver = Silver}}, [{silver, Cost, _} | T]) when Cost =< Silver ->
    cost(User#user{asset = Asset#asset{silver = Silver - Cost}}, T);
cost(User = #user{asset = Asset = #asset{copper = Copper}}, [{copper, Cost, _} | T]) when Cost =< Copper ->
    cost(User#user{asset = Asset#asset{copper = Copper - Cost}}, T);

cost(_, [{_, _, Code} | _]) ->
    {error, Code};
cost(_, [_ | _]) ->
    {error, 0}.
%%%===================================================================
%%% Internal functions
%%%===================================================================