%%%-------------------------------------------------------------------
%%% @doc
%%% module vip handle
%%% @end
%%%-------------------------------------------------------------------
-module(vip_handler).
%% API
-export([handle/3]).
%% Includes
-include("user.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% vip
handle(?CMD_VIP, #user{vip = VIP}, []) ->
    {reply, [VIP]};

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.
