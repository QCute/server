%%%-------------------------------------------------------------------
%%% @doc
%%% battle attribute
%%% @end
%%%-------------------------------------------------------------------
-module(battle_attribute).
%% API
-export([check/2]).
-export([calculate_hurt/2]).
%% Includes
-include("map.hrl").
-include("attribute.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc check
-spec check(Object :: #fighter{}, What :: term()) -> boolean().
check(#fighter{attribute = #attribute{freeze = 1}}, cannot_attack) ->
    true;
check(#fighter{attribute = #attribute{vertigo = 1}}, cannot_attack) ->
    true;
check(_, _) ->
    false.

%% @doc calculate base attribute hurt
-spec calculate_hurt(Attacker :: #fighter{}, Target :: #fighter{}) -> non_neg_integer().
calculate_hurt(#fighter{attribute = #attribute{fc = Fc}}, _) ->
    Fc.

%%%===================================================================
%%% Internal functions
%%%===================================================================
