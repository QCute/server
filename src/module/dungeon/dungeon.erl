%%%------------------------------------------------------------------
%%% @doc
%%% module dungeon
%%% @end
%%%------------------------------------------------------------------
-module(dungeon).
%% API
-export([load/1, save/1, reset/1]).
-export([query/1]).
-export([check_quest/2]).
-export([enter/2]).
-export([passed/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("vip.hrl").
-include("map.hrl").
-include("event.hrl").
-include("dungeon.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc load
-spec load(User :: #user{}) -> NewUser :: #user{}.
load(User = #user{role_id = RoleId}) ->
    Dungeon = dungeon_sql:select(RoleId),
    User#user{dungeon = Dungeon}.

%% @doc save
-spec save(User :: #user{}) -> NewUser :: #user{}.
save(User = #user{dungeon = Dungeon}) ->
    NewDungeon = dungeon_sql:insert_update(Dungeon),
    User#user{dungeon = NewDungeon}.

%% @doc clean
-spec reset(User :: #user{}) -> NewUser :: #user{}.
reset(User = #user{dungeon = DungeonList}) ->
    NewDungeonList = [Dungeon#dungeon{today_number = 0, flag = 1} || Dungeon <- DungeonList],
    User#user{dungeon = NewDungeonList}.

%% @doc query
-spec query(User :: #user{}) -> ok().
query(#user{dungeon = Dungeon}) ->
    {ok, Dungeon}.

%% @doc check quest
-spec check_quest(User :: #user{}, atom()) -> #event_checker{}.
check_quest(#user{dungeon = Dungeon}, event_dungeon_passed) ->
    #event_checker{data = Dungeon, key = #dungeon.type, value = #dungeon.dungeon_id}.

%% @doc enter
-spec enter(User :: #user{}, DungeonId :: non_neg_integer()) -> ok() | error().
enter(User, DungeonId) ->
    case dungeon_data:get(DungeonId) of
        DungeonData = #dungeon_data{} ->
            check_condition(User, DungeonData);
        _ ->
            {error, configure_not_found}
    end.

check_condition(User, DungeonData = #dungeon_data{condition = Condition}) ->
    case user_checker:check(User, Condition) of
        ok ->
            check_limit(User, DungeonData);
        _ ->
            {error, condition_not_met}
    end.

check_limit(User = #user{role_id = RoleId, vip = #vip{vip_level = VipLevel}, dungeon = DungeonList}, DungeonData = #dungeon_data{type = Type, day_number = DayNumberList, buy_number = BuyNumberList}) ->
    %% every day free number
    {_, DayNumber} = listing:key_find(VipLevel, 1, DayNumberList, {VipLevel, 0}),
    %% every day can buy number
    {_, BuyNumber, Gold} = listing:key_find(VipLevel, 1, BuyNumberList, {VipLevel, 0, 0}),
    %% enter or after cost gold
    case listing:key_find(Type, #dungeon.type, DungeonList, #dungeon{role_id = RoleId, type = Type}) of
        Dungeon = #dungeon{today_number = TodayNumber} when TodayNumber < DayNumber ->
            NewDungeon = Dungeon#dungeon{today_number = TodayNumber + 1, flag = 1},
            enter_map(User, NewDungeon, DungeonData);
        Dungeon = #dungeon{today_number = TodayNumber} when TodayNumber < DayNumber + BuyNumber ->
            NewDungeon = Dungeon#dungeon{today_number = TodayNumber + 1, flag = 1},
            cost(User, NewDungeon, DungeonData, Gold);
        _ ->
            {error, today_number_limit}
    end.

cost(User, Dungeon, DungeonData = #dungeon_data{cost = Cost}, Gold) ->
    case item:cost(User, [{gold, Gold} | Cost], ?MODULE) of
        {ok, NewUser} ->
            enter_map(NewUser, Dungeon, DungeonData);
        _ ->
            {error, item_not_enough}
    end.

enter_map(User = #user{role_id = RoleId, dungeon = DungeonList}, Dungeon, #dungeon_data{dungeon_id = DungeonId, module = Module, function = Function, map_id = MapId}) ->
    %% save dungeon
    NewDungeonList = lists:keystore(DungeonId, #dungeon.type, DungeonList, Dungeon),
    NewUser = User#user{dungeon = NewDungeonList},
    %% handle enter dungeon event
    NewestUser = user_event:handle(NewUser, #event{name = event_dungeon_enter, target = DungeonId}),
    %% start map
    Map = #map{pid = Pid} = map_server:start(MapId),
    map_server:apply_cast(Pid, Module, Function, [RoleId, DungeonId]),
    %% enter and return
    {ok, ok, map_server:enter(NewestUser, Map)}.

%% @doc dungeon passed
-spec passed(User :: #user{}, DungeonId :: non_neg_integer()) -> ok() | error().
passed(User, DungeonId) ->
    case dungeon_data:get(DungeonId) of
        DungeonData = #dungeon_data{} ->
            update_dungeon(User, DungeonData);
        _ ->
            {error, configure_not_found}
    end.

update_dungeon(User = #user{dungeon = DungeonList}, DungeonData = #dungeon_data{dungeon_id = DungeonId, type = Type, award = Award}) ->
    case lists:keyfind(Type, #dungeon.type, DungeonList) of
        Dungeon = #dungeon{} ->
            %% update dungeon id
            NewDungeonList = lists:keystore(Type, #dungeon.type, DungeonList, Dungeon#dungeon{dungeon_id = DungeonId, flag = 1}),
            %% give award
            {ok, NewUser} = item:add(User, Award, ?MODULE),
            %% handle pass dungeon event
            handle_event(NewUser#user{dungeon = NewDungeonList}, DungeonData);
        _ ->
            {error, no_such_dungeon}
    end.

handle_event(User, #dungeon_data{dungeon_id = DungeonId, event = Event}) ->
    %% handle pass dungeon event
    {ok, user_event:handle(User, [#event{name = event_dungeon_passed, target = DungeonId}, #event{name = Event, target = DungeonId}])}.

%%%==================================================================
%%% Internal functions
%%%==================================================================
