%%%-------------------------------------------------------------------
%%% @doc
%%% battle rank
%%% @end
%%%-------------------------------------------------------------------
-module(battle_rank).
%% API
-export([name/1]).
-export([new/2]).
-export([drop/1]).
-export([data/1]).
-export([update/5]).
%% Includes
-include("sorter.hrl").
-include("rank.hrl").
-include("map.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc map rank ets table name
-spec name(non_neg_integer() | pid()) -> atom().
name(MapNo) when is_integer(MapNo) ->
    binary_to_atom(<<"battle_rank_", (integer_to_binary(MapNo))/binary>>, utf8);
name(Pid) when is_pid(Pid) ->
    element(2, erlang:process_info(Pid, registered_name)).

%% @doc new sorter
-spec new(#map{}, Mode :: atom()) -> #sorter{}.
new(_, []) ->
    undefined;
new(#map{map_no = MapNo}, Mode) ->
    sorter:new(name(MapNo), Mode, add, infinity, #rank.key, #rank.value, #rank.time, #rank.order, []).

%% @doc drop sorter
-spec drop(#map{}) -> ok.
drop(#map{sorter = undefined}) ->
    ok;
drop(#map{sorter = Sorter}) ->
    sorter:drop(Sorter).

%% @doc update data
-spec update_data(#map{}, Data :: tuple() | [tuple()]) -> ok.
update_data(#map{sorter = undefined}, _) ->
    ok;
update_data(#map{sorter = Sorter}, Data) ->
    sorter:update(Data, Sorter),
    ok.

%% @doc get data
-spec data(#map{}) -> [tuple()].
data(#map{sorter = Sorter}) ->
    sorter:data(Sorter);
data(MapNo) when is_integer(MapNo) ->
    sorter:data(name(MapNo));
data(Name) when is_atom(Name) ->
    sorter:data(Name).

%% @doc update rank
-spec update(#map{}, #fighter{}, non_neg_integer(), non_neg_integer(), term()) -> ok.
update(State = #map{map_id = MapId}, Attacker, Value, Now, Type) ->
    update_rank_value(State, map_data:get(MapId), Attacker, Value, Now, Type).

%% role hurt rank
update_rank_value(State, #map_data{rank_key = role, rank_value = hurt}, #fighter{type = ?MAP_OBJECT_ROLE, id = Key, data = #fighter_role{role_name = Name}}, FinalHurt, Now, hurt) ->
    update_data(State, #rank{key = Key, value = FinalHurt, name = Name, time = Now});
%% guild hurt rank
update_rank_value(State, #map_data{rank_key = guild, rank_value = hurt}, #fighter{type = ?MAP_OBJECT_ROLE, data = #fighter_role{guild_id = Key, guild_name = Name}}, FinalHurt, Now, hurt) ->
    update_data(State, #rank{key = Key, value = FinalHurt, name = Name, time = Now});
%% team hurt rank
update_rank_value(State, #map_data{rank_key = team, rank_value = hurt}, #fighter{data = #fighter_role{team_id = Key, team_name = Name}}, FinalHurt, Now, hurt) ->
    update_data(State, #rank{key = Key, value = FinalHurt, name = Name, time = Now});
%% camp hurt rank
update_rank_value(State, #map_data{rank_key = camp, rank_value = hurt}, #fighter{camp = Key}, FinalHurt, Now, hurt) ->
    update_data(State, #rank{key = Key, value = FinalHurt, time = Now});
%% etc..
update_rank_value(_, _, _, _, _, _) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
