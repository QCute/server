-module(guild_handler).
-export([handle/3]).

handle(30101, _, []) ->
    guild_server:query_guild();

handle(30102, _, [GuildId]) ->
    guild_server:query_role(GuildId);

handle(30103, _, [GuildId]) ->
    guild_server:query_apply(GuildId);

handle(30104, User, []) ->
    guild_server:query_self_guild(User);

handle(30105, User, []) ->
    guild_server:query_self_role(User);

handle(30106, User, []) ->
    guild_server:query_self_apply(User);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
