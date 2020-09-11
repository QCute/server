%%%-------------------------------------------------------------------
%%% @doc
%%% dungeon map
%%% @end
%%%-------------------------------------------------------------------
-module(dungeon_map).
%% API
-export([start/3]).
-export([monster_dead/2, role_dead/2, refresh_monster/1]).
-export([role_leave/2]).
-export([inspire/1]).
-export([over/1]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("event.hrl").
-include("user.hrl").
-include("role.hrl").
-include("map.hrl").
-include("monster.hrl").
-include("dungeon.hrl").
%% Records
-record(dungeon_map_data, {
    role_id = 0,                                      %% 角色Id
    dungeon_id = 0,                                   %% 副本Id
    monster_number = 0,                               %% 当前怪物数量
    monster_list = [],                                %% 待刷怪物列表
    state,                                            %% 当前状态
    inspire = 0                                       %% 鼓舞
}).
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc start
-spec start(State :: #map_state{}, RoleId :: non_neg_integer(), DungeonId :: non_neg_integer()) -> {ok, #map_state{}}.
start(State = #map_state{pid = Pid, fighter = FighterList}, RoleId, DungeonId) ->
    #dungeon_data{monsters = [Monster | MonsterList], time = Time} = dungeon_data:get(DungeonId),
    Monsters = monster:create([Monster]),
    DungeonMapData = #dungeon_map_data{role_id = RoleId, dungeon_id = DungeonId, monster_number = length(Monsters), monster_list = MonsterList, state = start},
    NewState = State#map_state{fighter = listing:merge(FighterList, Monsters), data = DungeonMapData},
    %% start notify
    user_sender:send(RoleId, ?PROTOCOL_DUNGEON_START, ok),
    %% over apply
    map_server:apply_delay_cast(Pid, ?MODULE, over, [], ?MILLISECONDS(Time)),
    %% add monster dead event
    Events = [#trigger{name = event_battle_monster_dead, module = ?MODULE, function = monster_dead}, #trigger{name = event_battle_role_dead, module = ?MODULE, function = role_dead}, #trigger{name = event_role_leave, module = ?MODULE, function = role_leave}],
    {ok, battle_event:add(NewState, Events)}.

%% @doc handle monster dead event
-spec monster_dead(State :: #map_state{}, #battle_event{}) -> ok.
monster_dead(State = #map_state{pid = Pid, data = DungeonMapData = #dungeon_map_data{role_id = RoleId, monster_number = MonsterAmount, monster_list = MonsterList}}, #battle_event{target = #fighter{monster_id = MonsterId}}) ->
    %% monster dead give award
    #monster_data{award = Award} = monster_data:get(MonsterId),
    %% award
    user_server:apply_cast(RoleId, item, add, [Award, ?MODULE]),
    %% refresh monster
    case {MonsterAmount - 1 =< 0, MonsterList} of
        {true, []} ->
            %% all last wave monster dead, over
            over(State);
        {true, _} ->
            %% refresh next wave monster 10 seconds after
            map_server:apply_delay_cast(Pid, ?MODULE, refresh_monster, [], ?MILLISECONDS(10)),
            ok;
        {false, _} ->
            %% only update monster amount
            {ok, State#map_state{data = DungeonMapData#dungeon_map_data{monster_number = MonsterAmount - 1}}}
    end.

%% @doc refresh monster
-spec refresh_monster(State :: #map_state{}) -> {ok, #map_state{}}.
refresh_monster(State = #map_state{pid = Pid, data = DungeonMapData = #dungeon_map_data{role_id = RoleId, dungeon_id = DungeonId, monster_list = []}}) ->
    %% pass dungeon
    user_server:apply_cast(RoleId, dungeon, passed, [DungeonId]),
    %% over notify
    user_sender:send(RoleId, ?PROTOCOL_DUNGEON_OVER, succeed),
    %% stop map server
    map_server:stop(Pid, ?MILLISECONDS),
    %% update state
    {ok, State#map_state{data = DungeonMapData#dungeon_map_data{state = over}}};
refresh_monster(State = #map_state{fighter = FighterList, data = DungeonMapData = #dungeon_map_data{monster_list = [Monster | MonsterList]}}) ->
    Monsters = monster:create([Monster]),
    {ok, Binary} = user_router:write(?PROTOCOL_MAP_FIGHTER, Monsters),
    map:broadcast(State, Binary),
    {ok, State#map_state{fighter = listing:merge(FighterList, Monsters), data = DungeonMapData#dungeon_map_data{monster_list = MonsterList, monster_number = length(Monsters), state = refresh}}}.

%% @doc handle role dead event
-spec role_dead(State :: #map_state{}, #battle_event{}) -> ok.
role_dead(#map_state{pid = Pid, data = #dungeon_map_data{role_id = RoleId}}, #battle_event{}) ->
    %% role dead mean dungeon challenge failed, over notify
    user_sender:send(RoleId, ?PROTOCOL_DUNGEON_OVER, failed),
    %% stop map server
    map_server:stop(Pid, ?MILLISECONDS).

%% @doc role leave dungeon map
-spec role_leave(State :: #map_state{}, #battle_event{}) -> ok.
role_leave(#map_state{pid = Pid}, _) ->
    %% stop map server
    map_server:stop(Pid, ?MILLISECONDS).

%% @doc inspire
-spec inspire(User :: #user{}) -> {ok, ok}.
inspire(#user{role_id = RoleId, role = #role{map = #map{pid = Pid}}}) ->
    BuffId = parameter_data:get(dungeon_inspire_buff_id),
    {ok, map_server:apply_cast(Pid, fun(State = #map_state{data = DungeonMapData = #dungeon_map_data{inspire = 0}}) -> battle_buff:add(State#map_state{data = DungeonMapData#dungeon_map_data{inspire = 1}}, RoleId, BuffId); (_) -> ok end, [])}.

%% @doc over
-spec over(#map_state{}) -> ok.
over(#map_state{data = #dungeon_map_data{state = over}}) ->
    ok;
over(State = #map_state{pid = Pid, data = DungeonMapData = #dungeon_map_data{role_id = RoleId}}) ->
    %% over notify
    user_sender:send(RoleId, ?PROTOCOL_DUNGEON_OVER, failed),
    %% stop map server
    map_server:stop(Pid, ?MILLISECONDS),
    %% update state
    {ok, State#map_state{data = DungeonMapData#dungeon_map_data{state = over}}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
