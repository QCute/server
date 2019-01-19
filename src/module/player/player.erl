%%----------------------------------------------------
%% @doc
%% module player
%% @end
%%----------------------------------------------------
-module(player).
-include("player.hrl").
-include("assets.hrl").
-include("vip.hrl").
-export([load/1, save/1]).
-export([save_timed_first/1, save_timed_second/1]).
-export([cost/2]).
%%%===================================================================
%%% API
%%%===================================================================
%% @doc load data
load(User = #user{id = Id}) ->
    Data = player_sql:select(Id),
    [Player] = data_tool:load(Data, player),
    User#user{player = Player}.

%% @doc save data
save(User = #user{player = Player}) ->
    player_sql:update(Player),
    User.

%% @doc save data timed
save_timed_first(User) ->
    player_logout:save_loop(#user.player, #user.assets, User).

%% @doc save data timed
save_timed_second(User) ->
    player_logout:save_loop(#user.quest, #user.shop, User).

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