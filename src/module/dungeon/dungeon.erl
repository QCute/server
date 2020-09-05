%%%-------------------------------------------------------------------
%%% @doc
%%% dungeon
%%% @end
%%%-------------------------------------------------------------------
-module(dungeon).
%% API
-export([load/1, save/1, reset/1]).
-export([query/1]).
-export([get_number/1]).
-export([get_current/2]).
-export([enter/2]).
-export([passed/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("vip.hrl").
-include("map.hrl").
-include("event.hrl").
-include("dungeon.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
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

%% @doc get number
-spec get_number(User :: #user{}) -> non_neg_integer().
get_number(#user{dungeon = Dungeon}) ->
    length(Dungeon).

%% @doc get current
-spec get_current(User :: #user{}, Type :: non_neg_integer()) -> non_neg_integer().
get_current(#user{dungeon = Dungeon}, Type) ->
    #dungeon{dungeon_id = DungeonId} = listing:key_find(Type, #dungeon.type, Dungeon, #dungeon{}),
    DungeonId.

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
        Dungeon = #dungeon{dungeon_id = DungeonId, today_number = TodayNumber} when TodayNumber < DayNumber ->
            %% update dungeon
            NewDungeon = Dungeon#dungeon{today_number = TodayNumber + 1, flag = 1},
            NewDungeonList = lists:keystore(DungeonId, #dungeon.type, DungeonList, NewDungeon),
            enter_map(User#user{dungeon = NewDungeonList}, DungeonData);
        Dungeon = #dungeon{dungeon_id = DungeonId, today_number = TodayNumber} when TodayNumber < DayNumber + BuyNumber ->
            %% update dungeon
            NewDungeon = Dungeon#dungeon{today_number = TodayNumber + 1, flag = 1},
            NewDungeonList = lists:keystore(DungeonId, #dungeon.type, DungeonList, NewDungeon),
            cost(User#user{dungeon = NewDungeonList}, DungeonData, Gold);
        _ ->
            {error, today_number_limit}
    end.

cost(User, DungeonData = #dungeon_data{cost = Cost}, Gold) ->
    case item:cost(User, [{gold, Gold} | Cost], ?MODULE) of
        {ok, NewUser} ->
            enter_map(NewUser, DungeonData);
        _ ->
            {error, item_not_enough}
    end.

enter_map(User, DungeonData = #dungeon_data{map_id = MapId}) ->
    %% start map
    Map = map_server:start(MapId),
    %% enter
    NewUser = map_server:enter(User, Map),
    %% handle event and return
    start(NewUser, DungeonData, Map).

%% start dungeon map callback and handle enter event @here if the default handle not satisfy


start(User = #user{role_id = RoleId}, #dungeon_data{dungeon_id = DungeonId}, #map{pid = Pid}) ->
    %% dungeon map start callback
    map_server:apply_cast(Pid, dungeon_map, start, [RoleId, DungeonId]),
    %% handle enter dungeon event
    {ok, ok, user_event:handle(User, [#event{name = event_dungeon_enter, target = DungeonId}])}.

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
            handle_passed_event(NewUser#user{dungeon = NewDungeonList}, DungeonData);
        _ ->
            {error, no_such_dungeon}
    end.

%% handle dungeon passed event @here if the default handle not satisfy


handle_passed_event(User, #dungeon_data{dungeon_id = DungeonId}) ->
    %% handle passed dungeon event
    {ok, user_event:handle(User, [#event{name = event_dungeon_passed, target = DungeonId}])}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
