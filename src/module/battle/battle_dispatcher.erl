%%%-------------------------------------------------------------------
%%% @doc
%%% battle dispatcher
%%% @end
%%%-------------------------------------------------------------------
-module(battle_dispatcher).
%% API
-export([trigger/2]).
%% Includes
-include("event.hrl").
-include("map.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc trigger static event
-spec trigger(Map :: #map{}, Event :: #battle_event{}) -> NewMap :: #map{}.
%% trigger static event @here
%% auto generate, do not edit this code

trigger(Map, _) ->
    Map.

%%%===================================================================
%%% Internal functions
%%%===================================================================
