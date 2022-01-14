-module(chat_handler).
-export([handle/3]).

handle(_, 11602, Page) ->
    chat_server:get_system_list(Page);

handle(User, 11603, [Type, Message]) ->
    chat:world(User, Type, Message);

handle(_, 11604, Page) ->
    chat_server:get_world_list(Page);

handle(User, 11605, [Type, Message]) ->
    chat:guild(User, Type, Message);

handle(User, 11606, Page) ->
    chat_server:get_guild_list(User, Page);

handle(User, 11607, [RoleId, Type, Message]) ->
    chat:private(User, RoleId, Type, Message);

handle(User, 11608, [RoleId, Page]) ->
    chat_server:get_private_list(User, RoleId, Page);

handle(_, Protocol, Data) ->
    {error, Protocol, Data}.
