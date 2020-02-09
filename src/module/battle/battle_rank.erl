%%%------------------------------------------------------------------
%%% @doc
%%% module battle buff
%%% @end
%%%------------------------------------------------------------------
-module(battle_rank).
%% API
-export([name/1]).
-export([new/2]).
-export([data/1]).
-export([update/5]).
%% Includes
-include("sorter.hrl").
-include("rank.hrl").
-include("map.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc map rank unique name
-spec name(non_neg_integer() | pid()) -> atom().
name(UniqueId) when is_integer(UniqueId) ->
    type:to_atom(lists:concat(["battle_rank_", UniqueId]));
name(Pid) when is_pid(Pid) ->
    erlang:element(2, erlang:process_info(Pid, registered_name)).

%% @doc new sorter
-spec new(#map_state{}, Mode :: atom()) -> #sorter{}.
new(_, []) ->
    undefined;
new(#map_state{unique_id = UniqueId}, Mode) ->
    sorter:new(name(UniqueId), Mode, add, infinity, #rank.key, #rank.value, #rank.time, #rank.rank, []).

%% @doc update data
-spec update_data(#map_state{}, Data :: tuple() | [tuple()]) -> ok.
update_data(#map_state{sorter = undefined}, _) ->
    ok;
update_data(#map_state{sorter = Sorter}, Data) ->
    sorter:update(Data, Sorter),
    ok.

%% @doc get data
-spec data(#map_state{}) -> [].
data(#map_state{sorter = Sorter}) ->
    sorter:data(Sorter);
data(UniqueId) when is_integer(UniqueId) ->
    sorter:data(name(UniqueId));
data(Name) when is_atom(Name) ->
    sorter:data(Name).

%% @doc update rank
-spec update(#map_state{}, #fighter{}, #fighter{}, non_neg_integer(), non_neg_integer(), term()) -> ok.
update(State = #map_state{map_id = MapId}, Attacker, Value, Now, Type) ->
    update(State, map_data:get(MapId), Attacker, Value, Now, Type).

%% role hurt rank
update(State, #map_data{rank_key = role, rank_value = hurt}, #fighter{type = ?MAP_OBJECT_ROLE, id = Key, name = Name}, FinalHurt, Now, hurt) ->
    update_data(State, #rank{key = Key, value = FinalHurt, name = Name, time = Now});
%% guild hurt rank
update(State, #map_data{rank_key = guild, rank_value = hurt}, #fighter{type = ?MAP_OBJECT_ROLE, guild_id = Key, guild_name = Name}, FinalHurt, Now, hurt) ->
    update_data(State, #rank{key = Key, value = FinalHurt, name = Name, time = Now});
%% team hurt rank
update(State, #map_data{rank_key = team, rank_value = hurt}, #fighter{type = ?MAP_OBJECT_ROLE, team_id = Key, team_name = Name}, FinalHurt, Now, hurt) ->
    update_data(State, #rank{key = Key, value = FinalHurt, name = Name, time = Now});
%% camp hurt rank
update(State, #map_data{rank_key = camp, rank_value = hurt}, #fighter{camp = Key}, FinalHurt, Now, hurt) ->
    update_data(State, #rank{key = Key, value = FinalHurt, time = Now});
%% etc..
update(_, _, _, _, _, _) ->
    ok.


%%%==================================================================
%%% Internal functions
%%%==================================================================
