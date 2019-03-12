%%%-------------------------------------------------------------------
%%% @doc
%%% module player assets
%%% @end
%%%-------------------------------------------------------------------
-module(player_assets).
%% API
-export([add/2, cost/2]).
%% includes
-include("player.hrl").
-include("assets.hrl").
-include("vip.hrl").
%%%===================================================================
%%% API
%%%===================================================================
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