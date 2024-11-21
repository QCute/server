-module(chat_handler).
-export([handle/3]).
-export([send_get_system_list/2]).
-export([send_world/3]).
-export([send_get_world_list/2]).
-export([send_guild/3]).
-export([send_get_guild_list/2]).
-export([send_private/3]).
-export([send_get_private_list/2]).
-include("user.hrl").

handle(User, 11602, Data) ->
    chat_server:get_system_list(User, Data);

handle(User, 11603, {Type, Message}) ->
    chat:world(User, Type, Message);

handle(User, 11604, Data) ->
    chat_server:get_world_list(User, Data);

handle(User, 11605, {Type, Message}) ->
    chat:guild(User, Type, Message);

handle(User, 11606, Data) ->
    chat_server:get_guild_list(User, Data);

handle(User, 11607, {RoleId, Type, Message}) ->
    chat:private(User, RoleId, Type, Message);

handle(User, 11608, {RoleId, Page}) ->
    chat_server:get_private_list(User, RoleId, Page);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.

send_get_system_list(User, Data) ->
    {ok, Binary} = chat_protocol:encode(11602, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_world(User, Result, WorldChat) ->
    {ok, Binary} = chat_protocol:encode(11603, {Result, WorldChat}),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_get_world_list(User, Data) ->
    {ok, Binary} = chat_protocol:encode(11604, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_guild(User, Result, GuildChat) ->
    {ok, Binary} = chat_protocol:encode(11605, {Result, GuildChat}),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_get_guild_list(User, Data) ->
    {ok, Binary} = chat_protocol:encode(11606, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_private(User, Result, PrivateChat) ->
    {ok, Binary} = chat_protocol:encode(11607, {Result, PrivateChat}),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

send_get_private_list(User, Data) ->
    {ok, Binary} = chat_protocol:encode(11608, Data),
    User#user{buffer = <<(User#user.buffer)/binary, Binary/binary>>}.

