%%%-------------------------------------------------------------------
%%% @doc
%%% module monster
%%% @end
%%%-------------------------------------------------------------------
-module(monster).
%% API
-export([create/3]).
%% Includes
-include("monster.hrl").
-include("map.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc create
create([], [], List) ->
    List;
create([MonsterId | DataIdList], [UniqueId | UniqueIdList], List) ->
    case monster_data:get(MonsterId) of
        DataMonster = #data_monster{born_points = Points} ->
            {X, Y} = listing:random(Points, {0, 0}),
            Monster = #monster{
                id = UniqueId,
                x = X,
                y = Y,
                act_type = DataMonster#data_monster.act_type,
                act_script = DataMonster#data_monster.act_script,
                act = DataMonster#data_monster.act,
                camp = DataMonster#data_monster.camp
            },
            create(DataIdList, UniqueIdList, [Monster | List]);
        _ ->
            create(DataIdList, UniqueIdList, List)
    end.