%%%-------------------------------------------------------------------
%%% @doc
%%% module notice
%%% @end
%%%-------------------------------------------------------------------
-module(notice).
%% API
-export([make/2, broadcast/2]).
%% includes
-include("common.hrl").
-include("player.hrl").
%% notice float scroll
%% world scene team guild
%%%===================================================================
%%% API
%%%===================================================================
%% @doc broadcast
make(User, Content) ->
    do_make(User, Content).

%% @doc broadcast
broadcast(User, Content) ->
    Data = make(User, Content),
    player_manager:broadcast(Data),
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
do_make(_, _) ->
    <<>>.