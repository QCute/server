%%%-------------------------------------------------------------------
%%% @doc
%%% module notice
%%% @end
%%%-------------------------------------------------------------------
-module(notice).
%% API
-export([make/2, broadcast/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("role.hrl").
-include("notice.hrl").
-include("protocol.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc construct notice msg
-spec make(User :: #user{}, Content :: term()) -> {ok, binary()}.
make(User, Content) ->
    format(User, Content).

%% @doc broadcast
-spec broadcast(User :: #user{}, Content :: term()) -> ok.
broadcast(User, Content) ->
    {ok, Data} = make(User, Content),
    user_manager:broadcast(Data),
    ok.
%%%===================================================================
%%% Internal functions
%%%===================================================================
format(_, _) ->
    {ok, <<>>}.
