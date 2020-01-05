%%%------------------------------------------------------------------
%%% @doc
%%% module notice
%%% @end
%%%------------------------------------------------------------------
-module(notice).
%% API
-export([broadcast/2, format/2]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("role.hrl").
-include("notice.hrl").
%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc broadcast
-spec broadcast(Term :: term(), Content :: [term()]) -> ok.
broadcast(Any, Args) ->
    {ok, Binary} = notice_protocol:write(?PROTOCOL_NOTICE, format(Any, Args)),
    user_manager:broadcast(Binary).

%% @doc construct notice msg
-spec format(Any :: term(), Args :: [term()]) -> [term()].
format(_, [level_upgrade, Level]) ->
    [?NOTICE_SCOPE_WORLD, ?NOTICE_TYPE_CHAT, parser:format(text_data:get(level_upgrade), [Level])];
format(_, [guild_create, GuildId, GuildName]) ->
    [?NOTICE_SCOPE_WORLD, ?NOTICE_TYPE_CHAT, parser:format(text_data:get(guild_create), [GuildId, GuildName])];
format(_, _) ->
    [0, 0, <<>>].

%%%==================================================================
%%% Internal functions
%%%==================================================================
