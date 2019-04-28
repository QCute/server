%%%-------------------------------------------------------------------
%%% @doc
%%% module player assets
%%% @end
%%%-------------------------------------------------------------------
-module(player_assets).
%% API
-export([load/1, save/1]).
-export([add/2, cost/2]).
%% Includes
-include("user.hrl").
-include("player.hrl").
-include("assets.hrl").
-include("vip.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load user items
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{id = UserId}) ->
    Data =  player_assets_sql:select(UserId),
    case parser:convert(Data, assets) of
        [] ->
            %% new data
            Assets = #assets{player_id = UserId},
            player_assets_sql:insert(Assets);
        [Assets] ->
            Assets
    end,
    User#user{assets = Assets}.

%% @doc save user items
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{assets = Assets}) ->
    player_assets_sql:update(Assets),
    User.

%% @doc only add assess
-spec add(User :: #user{}, CostList :: list()) -> {ok, NewUser :: #user{}} | {error, non_neg_integer()}.
add(User, []) ->
    {ok, User};
add(User = #user{assets = Assets = #assets{gold = Gold}}, [{gold, Add, _} | T]) ->
    add(User#user{assets = Assets#assets{gold = Gold + Add}}, T);
add(User = #user{assets = Assets = #assets{silver = Silver}}, [{silver, Add, _} | T]) ->
    add(User#user{assets = Assets#assets{silver = Silver + Add}}, T);
add(User = #user{assets = Assets = #assets{copper = Copper}}, [{copper, Add, _} | T]) ->
    add(User#user{assets = Assets#assets{copper = Copper + Add}}, T);

add(_, [_ | _]) ->
    {error, 0}.

%% @doc only cost assess
-spec cost(User :: #user{}, CostList :: list()) -> {ok, NewUser :: #user{}} | {error, non_neg_integer()}.
cost(User, []) ->
    {ok, User};
cost(User = #user{assets = Assets = #assets{gold = Gold}}, [{gold, Cost} | T]) when Cost =< Gold ->
    cost(User#user{assets = Assets#assets{gold = Gold - Cost}}, T);
cost(User = #user{assets = Assets = #assets{silver = Silver}}, [{silver, Cost} | T]) when Cost =< Silver ->
    cost(User#user{assets = Assets#assets{silver = Silver - Cost}}, T);
cost(User = #user{assets = Assets = #assets{copper = Copper}}, [{copper, Cost} | T]) when Cost =< Copper ->
    cost(User#user{assets = Assets#assets{copper = Copper - Cost}}, T);

cost(User = #user{assets = Assets = #assets{gold = Gold}}, [{gold, Cost, _} | T]) when Cost =< Gold ->
    cost(User#user{assets = Assets#assets{gold = Gold - Cost}}, T);
cost(User = #user{assets = Assets = #assets{silver = Silver}}, [{silver, Cost, _} | T]) when Cost =< Silver ->
    cost(User#user{assets = Assets#assets{silver = Silver - Cost}}, T);
cost(User = #user{assets = Assets = #assets{copper = Copper}}, [{copper, Cost, _} | T]) when Cost =< Copper ->
    cost(User#user{assets = Assets#assets{copper = Copper - Cost}}, T);

cost(_, [{_, _, Code} | _]) ->
    {error, Code};
cost(_, [_ | _]) ->
    {error, 0}.
%%%===================================================================
%%% Internal functions
%%%===================================================================