%%%-------------------------------------------------------------------
%%% @doc
%%% module role handle
%%% @end
%%%-------------------------------------------------------------------
-module(role_handle).
%% API
-export([handle/3]).
%% Includes
-include("user.hrl").
-include("role.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% role
handle(?CMD_ROLE, #user{role = Role}, []) ->
    {reply, [Role]};

%% assets
handle(?CMD_ROLE_ASSETS, #user{assets = Assets}, []) ->
    {reply, [Assets]};

%% @doc 容错
handle(Protocol, _User, Data) ->
    {error, Protocol, Data}.