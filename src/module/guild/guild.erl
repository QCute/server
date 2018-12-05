%%%-------------------------------------------------------------------
%%% @doc
%%% module guild
%%% @end
%%%-------------------------------------------------------------------
-module(guild).
-compile(nowarn_export_all).
-include("common.hrl").
-include("guild.hrl").
-export([
    server_start/0,
    server_stop/0
]).

%%%===================================================================
%%% API
%%%===================================================================
%% @doc guild server start
server_start() ->
    ets:new(guild, [named_table, {keypos, #guild.id}, {read_concurrency, true}]),
    ets:new(guild_player, [named_table, {keypos, #guild_player.player_id}, {read_concurrency, true}]),
    SaveGuild = fun(X) -> ets:insert(guild, X) end,
    data_tool:load(guild_sql:select(), guild, SaveGuild),
    SaveGuildPlayer = fun(X) -> ets:insert(guild_player, X) end,
    data_tool:load(guild_player_sql:select_join(), guild_player, SaveGuildPlayer),
    {ok, []}.

%% @doc guild server stop
server_stop() ->
    guild_sql:update_into(guild),
    guild_player_sql:update_into(guild_player),
    ok.


