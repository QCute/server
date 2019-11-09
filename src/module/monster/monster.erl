%%%------------------------------------------------------------------
%%% @doc
%%% module monster
%%% @end
%%%------------------------------------------------------------------
-module(monster).
%% API
-export([create/1]).
%% Includes
-include("map.hrl").
-include("monster.hrl").
-include("attribute.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc create
create(List) ->
    create_loop(List, []).

create_loop([], List) ->
    List;
create_loop([MonsterId | MonsterIdList], List) ->
    case monster_data:get(MonsterId) of
        MonsterData = #monster_data{born_points = Points} ->
            {X, Y} = listing:random(Points, {0, 0}),
            Fighter = #fighter{
                id = increment:next(monster),
                monster_id = MonsterId,
                x = X,
                y = Y,
                type = ?MAP_OBJECT_MONSTER,
                attribute = #attribute{},
                act_type = MonsterData#monster_data.act_type,
                act_script = MonsterData#monster_data.act_script,
                camp = MonsterData#monster_data.camp,
                skills = MonsterData#monster_data.skills
            },
            create_loop(MonsterIdList, [Fighter | List]);
        _ ->
            create_loop(MonsterIdList, List)
    end.