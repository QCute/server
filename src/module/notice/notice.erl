%%%-------------------------------------------------------------------
%%% @doc
%%% module notice
%%% @end
%%%-------------------------------------------------------------------
-module(notice).
%% API
-export([broadcast/2, make/2]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("role.hrl").
-include("notice.hrl").
-include("protocol.hrl").
%%%===================================================================
%%% API
%%%===================================================================
%% @doc broadcast
-spec broadcast(User :: #user{}, Content :: [term()]) -> ok.
broadcast(User, Content) ->
    Data = make(User, Content),
    {ok, Binary} = notice_protocol:write(?PROTOCOL_NOTICE, Data),
    user_manager:broadcast(Binary).

%% @doc construct notice msg
-spec make(User :: #user{}, Content :: [term()]) -> [term()].
make(User, Content) ->
    format(User, Content).

%%%===================================================================
%%% Internal functions
%%%===================================================================
format(_, [guild_create, GuildId, GuildName]) ->
    [?NOTICE_SCOPE_WORLD, ?NOTICE_TYPE_CHAT, parser:format(text_data:get(guild_create), [GuildId, GuildName])];
format(_, _) ->
    [].
