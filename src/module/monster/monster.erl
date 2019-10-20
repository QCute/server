%%%-------------------------------------------------------------------
%%% @doc
%%% module monster
%%% @end
%%%-------------------------------------------------------------------
-module(monster).
%% API
-export([create/3]).
%% Includes
-include("map.hrl").
-include("monster.hrl").
-include("attribute.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc create
create([], [], List) ->
    List;
create([MonsterId | MonsterIdList], [UniqueId | UniqueIdList], List) ->
    case monster_data:get(MonsterId) of
        MonsterData = #monster_data{born_points = Points} ->
            {X, Y} = listing:random(Points, {0, 0}),
            Monster = #fighter{
                id = UniqueId,
                x = X,
                y = Y,
                attribute = #attribute{},
                act_type = MonsterData#monster_data.act_type,
                act_script = MonsterData#monster_data.act_script,
                act = MonsterData#monster_data.act,
                camp = MonsterData#monster_data.camp
            },
            create(MonsterIdList, UniqueIdList, [Monster | List]);
        _ ->
            create(MonsterIdList, UniqueIdList, List)
    end.