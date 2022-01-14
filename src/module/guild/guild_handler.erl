-module(guild_handler).
-export([handle/3]).

handle(_, 30101, []) ->
    guild_server:query_guild();

handle(User, 30102, []) ->
    guild_server:query_role(User);

handle(User, 30103, []) ->
    guild_server:query_apply(User);

handle(User, 30104, []) ->
    guild_server:query_self_guild(User);

handle(User, 30105, []) ->
    guild_server:query_self_role(User);

handle(User, 30106, []) ->
    guild_server:query_self_apply(User);

handle(User, 30107, [Type, GuildName]) ->
    guild_server:create(User, Type, GuildName);

handle(User, 30108, GuildId) ->
    guild_server:apply(User, GuildId);

handle(User, 30109, GuildId) ->
    guild_server:cancel_apply(User, GuildId);

handle(User, 30110, []) ->
    guild_server:cancel_all_apply(User);

handle(User, 30111, RoleId) ->
    guild_server:approve_apply(User, RoleId);

handle(User, 30112, []) ->
    guild_server:approve_all_apply(User);

handle(User, 30113, RoleId) ->
    guild_server:reject_apply(User, RoleId);

handle(User, 30114, []) ->
    guild_server:reject_all_apply(User);

handle(User, 30115, []) ->
    guild_server:leave(User);

handle(User, 30116, []) ->
    guild_server:dismiss(User);

handle(User, 30117, RoleId) ->
    guild_server:kick(User, RoleId);

handle(User, 30118, [RoleId, Job]) ->
    guild_server:update_job(User, RoleId, Job);

handle(User, 30119, []) ->
    guild_server:upgrade_level(User);

handle(User, 30120, Notice) ->
    guild_server:change_notice(User, Notice);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
