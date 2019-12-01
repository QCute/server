%%%------------------------------------------------------------------
%%% @doc
%%% module role condition
%%% @end
%%%------------------------------------------------------------------
-module(battle_attribute).
%% API
-export([check/2]).
-export([calculate_hurt/2]).
%% Includes
-include("map.hrl").
-include("attribute.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc check
-spec check(Object :: #fighter{}, What :: term()) -> boolean().
check(_, _) ->
    %% cannot
    ok.

%% @doc calculate base attribute hurt
-spec calculate_hurt(Attacker :: #fighter{}, Target :: #fighter{}) -> non_neg_integer().
calculate_hurt(_, _) ->
    0.

%%%==================================================================
%%% Internal functions
%%%==================================================================
