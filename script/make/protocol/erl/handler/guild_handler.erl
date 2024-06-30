-module(guild_handler).
-export([handle/3]).
-export([send_guild_server_query_guild/2]).
-export([send_guild_server_query_role/2]).
-export([send_guild_server_query_apply/2]).
-export([send_guild_server_query_self_guild/2]).
-export([send_guild_server_query_self_role/2]).
-export([send_guild_server_query_self_apply/2]).
-export([send_guild_server_create/2]).
-export([send_guild_server_apply/2]).
-export([send_guild_server_cancel_apply/2]).
-export([send_guild_server_cancel_all_apply/2]).
-export([send_guild_server_approve_apply/2]).
-export([send_guild_server_approve_all_apply/2]).
-export([send_guild_server_reject_apply/2]).
-export([send_guild_server_reject_all_apply/2]).
-export([send_guild_server_leave/2]).
-export([send_guild_server_dismiss/2]).
-export([send_guild_server_kick/2]).
-export([send_guild_server_update_job/2]).
-export([send_guild_server_upgrade_level/2]).
-export([send_guild_server_change_notice/2]).
-include("user.hrl").

handle(User, 30101, []) ->
    guild_server:query_guild(User);

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

send_guild_server_query_guild(User, List) ->
    {ok, Binary} = guild_protocol:encode(30101, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_query_role(User, List) ->
    {ok, Binary} = guild_protocol:encode(30102, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_query_apply(User, List) ->
    {ok, Binary} = guild_protocol:encode(30103, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_query_self_guild(User, Guild) ->
    {ok, Binary} = guild_protocol:encode(30104, Guild),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_query_self_role(User, GuildRole) ->
    {ok, Binary} = guild_protocol:encode(30105, GuildRole),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_query_self_apply(User, List) ->
    {ok, Binary} = guild_protocol:encode(30106, List),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_create(User, Result) ->
    {ok, Binary} = guild_protocol:encode(30107, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_apply(User, Result) ->
    {ok, Binary} = guild_protocol:encode(30108, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_cancel_apply(User, Result) ->
    {ok, Binary} = guild_protocol:encode(30109, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_cancel_all_apply(User, Result) ->
    {ok, Binary} = guild_protocol:encode(30110, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_approve_apply(User, Result) ->
    {ok, Binary} = guild_protocol:encode(30111, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_approve_all_apply(User, Result) ->
    {ok, Binary} = guild_protocol:encode(30112, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_reject_apply(User, Result) ->
    {ok, Binary} = guild_protocol:encode(30113, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_reject_all_apply(User, Result) ->
    {ok, Binary} = guild_protocol:encode(30114, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_leave(User, Result) ->
    {ok, Binary} = guild_protocol:encode(30115, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_dismiss(User, Result) ->
    {ok, Binary} = guild_protocol:encode(30116, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_kick(User, Result) ->
    {ok, Binary} = guild_protocol:encode(30117, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_update_job(User, Result) ->
    {ok, Binary} = guild_protocol:encode(30118, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_upgrade_level(User, Result) ->
    {ok, Binary} = guild_protocol:encode(30119, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild_server_change_notice(User, Result) ->
    {ok, Binary} = guild_protocol:encode(30120, Result),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

