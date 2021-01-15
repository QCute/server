-module(guild_handler).
-export([handle/3]).

handle(30101, _, []) ->
    guild_server:query_guild();

handle(30102, User, []) ->
    guild_server:query_role(User);

handle(30103, User, []) ->
    guild_server:query_apply(User);

handle(30104, User, []) ->
    guild_server:query_self_guild(User);

handle(30105, User, []) ->
    guild_server:query_self_role(User);

handle(30106, User, []) ->
    guild_server:query_self_apply(User);

handle(30107, User, [Type, GuildNameBinary]) ->
    guild_server:create(User, Type, GuildNameBinary);

handle(30108, User, GuildId) ->
    guild_server:apply(User, GuildId);

handle(30109, User, GuildId) ->
    guild_server:cancel_apply(User, GuildId);

handle(30110, User, []) ->
    guild_server:cancel_all_apply(User);

handle(30111, User, RoleId) ->
    guild_server:approve_apply(User, RoleId);

handle(30112, User, []) ->
    guild_server:approve_all_apply(User);

handle(30113, User, RoleId) ->
    guild_server:reject_apply(User, RoleId);

handle(30114, User, []) ->
    guild_server:reject_all_apply(User);

handle(30115, User, []) ->
    guild_server:leave(User);

handle(30116, User, []) ->
    guild_server:dismiss(User);

handle(30117, User, RoleId) ->
    guild_server:kick(User, RoleId);

handle(30118, User, [RoleId, Job]) ->
    guild_server:update_job(User, RoleId, Job);

handle(30119, User, []) ->
    guild_server:upgrade_level(User);

handle(30120, User, NoticeBinary) ->
    guild_server:change_notice(User, NoticeBinary);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
