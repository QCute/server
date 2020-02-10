%%%------------------------------------------------------------------
%%% @doc
%%% module dungeon map
%%% @end
%%%------------------------------------------------------------------
-module(dungeon_map).
%% API
-export([start/3]).
-export([monster_dead/2]).
-export([over/3]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("event.hrl").
-include("map.hrl").
-include("monster.hrl").
-include("dungeon.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc start
-spec start(State :: #map_state{}, RoleId :: non_neg_integer(), DungeonId :: non_neg_integer()) -> {ok, #map_state{}}.
start(State = #map_state{pid = Pid}, RoleId, DungeonId) ->
    %% start notify
    user_sender:send(RoleId, ?PROTOCOL_DUNGEON_START, ok),
    %% over apply
    map_server:apply_delay_cast(Pid, ?MODULE, over, [RoleId, DungeonId], ?MILLISECONDS((dungeon_data:get(DungeonId))#dungeon_data.time)),
    %% add hurt/monster dead event
    {ok, battle_event:add(State, [#trigger{name = event_battle_monster_dead, module = ?MODULE, function = monster_dead}])}.

%% @doc handle battle hurt event
-spec monster_dead(State :: #map_state{}, #battle_event{}) -> ok.
monster_dead(_, #battle_event{object = #fighter{id = Id}, target = #fighter{monster_id = MonsterId}}) ->
    %% monster dead give award
    #monster_data{award = Award} = monster_data:get(MonsterId),
    %% award
    user_server:apply_cast(Id, item, add, [Award, ?MODULE]).

%% @doc over
-spec over(#map_state{}, RoleId :: non_neg_integer(), DungeonId :: non_neg_integer()) -> ok.
over(#map_state{pid = Pid}, RoleId, DungeonId) ->
    %% pass dungeon
    user_server:apply_cast(RoleId, dungeon, passed, [DungeonId]),
    %% over notify
    user_sender:send(RoleId, ?PROTOCOL_DUNGEON_OVER, ok),
    %% stop map server
    map_server:stop(Pid, ?MILLISECONDS).

%%%==================================================================
%%% Internal functions
%%%==================================================================
