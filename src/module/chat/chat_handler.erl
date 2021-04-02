-module(chat_handler).
-export([handle/3]).

handle(11602, _, Page) ->
    chat_server:get_system_list(Page);

handle(11603, User, [Type, Message]) ->
    chat:world(User, Type, Message);

handle(11604, _, Page) ->
    chat_server:get_world_list(Page);

handle(11605, User, [Type, Message]) ->
    chat:guild(User, Type, Message);

handle(11606, User, Page) ->
    chat_server:get_guild_list(User, Page);

handle(11607, User, [RoleId, Type, Message]) ->
    chat:private(User, RoleId, Type, Message);

handle(11608, User, [RoleId, Page]) ->
    chat_server:get_private_list(User, RoleId, Page);

handle(Protocol, _, Data) ->
    {error, Protocol, Data}.
