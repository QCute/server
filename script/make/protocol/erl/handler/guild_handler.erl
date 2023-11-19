-module(guild_handler).
-export([handle/3]).
-export([send_query_guild/2]).
-export([send_query_role/2]).
-export([send_query_apply/2]).
-export([send_query_self_guild/2]).
-export([send_query_self_role/2]).
-export([send_query_self_apply/2]).
-export([send_create/2]).
-export([send_apply/2]).
-export([send_cancel_apply/2]).
-export([send_cancel_all_apply/2]).
-export([send_approve_apply/2]).
-export([send_approve_all_apply/2]).
-export([send_reject_apply/2]).
-export([send_reject_all_apply/2]).
-export([send_leave/2]).
-export([send_dismiss/2]).
-export([send_kick/2]).
-export([send_update_job/2]).
-export([send_upgrade_level/2]).
-export([send_change_notice/2]).
-include("user.hrl").

handle(User, 30101, {}) ->
    guild_server:query_guild(User);

handle(User, 30102, {}) ->
    guild_server:query_role(User);

handle(User, 30103, {}) ->
    guild_server:query_apply(User);

handle(User, 30104, {}) ->
    guild_server:query_self_guild(User);

handle(User, 30105, {}) ->
    guild_server:query_self_role(User);

handle(User, 30106, {}) ->
    guild_server:query_self_apply(User);

handle(User, 30107, {Type, GuildName}) ->
    guild_server:create(User, Type, GuildName);

handle(User, 30108, Data) ->
    guild_server:apply(User, Data);

handle(User, 30109, Data) ->
    guild_server:cancel_apply(User, Data);

handle(User, 30110, {}) ->
    guild_server:cancel_all_apply(User);

handle(User, 30111, Data) ->
    guild_server:approve_apply(User, Data);

handle(User, 30112, {}) ->
    guild_server:approve_all_apply(User);

handle(User, 30113, Data) ->
    guild_server:reject_apply(User, Data);

handle(User, 30114, {}) ->
    guild_server:reject_all_apply(User);

handle(User, 30115, {}) ->
    guild_server:leave(User);

handle(User, 30116, {}) ->
    guild_server:dismiss(User);

handle(User, 30117, Data) ->
    guild_server:kick(User, Data);

handle(User, 30118, {RoleId, Job}) ->
    guild_server:update_job(User, RoleId, Job);

handle(User, 30119, {}) ->
    guild_server:upgrade_level(User);

handle(User, 30120, Data) ->
    guild_server:change_notice(User, Data);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_query_guild(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30101, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_query_role(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30102, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_query_apply(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30103, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_query_self_guild(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30104, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_query_self_role(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30105, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_query_self_apply(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30106, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_create(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30107, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_apply(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30108, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_cancel_apply(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30109, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_cancel_all_apply(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30110, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_approve_apply(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30111, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_approve_all_apply(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30112, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_reject_apply(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30113, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_reject_all_apply(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30114, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_leave(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30115, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_dismiss(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30116, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_kick(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30117, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_update_job(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30118, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_upgrade_level(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30119, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_change_notice(User, Data) ->
    {ok, Binary} = guild_protocol:encode(30120, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

