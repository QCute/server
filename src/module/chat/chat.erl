%%%-------------------------------------------------------------------
%%% @doc
%%% chat
%%% @end
%%%-------------------------------------------------------------------
-module(chat).
%% API
-export([world/3, guild/3, private/4]).
-export([world_notify/2, guild_notify/2, private_notify/3]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
-include("role.hrl").
-include("chat.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc world
-spec world(User :: #user{}, Type :: non_neg_integer(), Message :: binary()) -> ok() | error().
world(User = #user{world_chat_time = WorldChatTime}, Type, Message) ->
    Now = time:now(),
    case user_checker:check(User, [{level, parameter_data:get(chat_level), level_not_satisfy}, {Now - WorldChatTime, ge, parameter_data:get(chat_cd), time_in_cd}]) of
        ok ->
            world_notify(User, [Type, Message]),
            {ok, User#user{world_chat_time = Now}};
        {error, Error} ->
            {error, [Error, #world_chat{}]}
    end.

%% @doc world notify
-spec world_notify(User :: #user{}, Args :: [term()]) -> ok.
world_notify(User, Args) ->
    WorldChat = user_convert:to_world_chat(User, Args),
    chat_server:chat_world(WorldChat),
    {ok, ChatBinary} = user_router:write(?PROTOCOL_CHAT_WORLD, [ok, WorldChat]),
    user_manager:broadcast(ChatBinary).

%% @doc guild
-spec guild(User :: #user{}, Type :: non_neg_integer(), Message :: binary()) -> ok() | error().
guild(User = #user{role_id = RoleId, guild_chat_time = GuildChatTime}, Type, Message) ->
    Now = time:now(),
    GuildId = guild:role_guild_id(RoleId),
    case user_checker:check(User, [{level, parameter_data:get(chat_level), level_not_satisfy}, {GuildId, ne, 0, no_guild}, {Now - GuildChatTime, ge, parameter_data:get(chat_cd), time_in_cd}]) of
        ok ->
            guild_notify(User, [Type, Message]),
            {ok, User#user{guild_chat_time = Now}};
        {error, Error} ->
            {error, [Error, #guild_chat{}]}
    end.

%% @doc guild notify
-spec guild_notify(User :: #user{}, Args :: [term()]) -> ok.
guild_notify(User = #user{guild_id = GuildId}, Args) ->
    GuildChat = user_convert:to_guild_chat(User, Args),
    chat_server:chat_guild(GuildChat),
    {ok, ChatBinary} = user_router:write(?PROTOCOL_CHAT_GUILD, [ok, GuildChat]),
    guild:broadcast(GuildId, ChatBinary).

%% @doc private
-spec private(User :: #user{}, ReceiverId :: non_neg_integer(), Type :: non_neg_integer(), Message :: binary()) -> ok() | error().
private(User = #user{role_id = RoleId}, ReceiverId, Type, Message) ->
    case user_checker:check(User, [{level, parameter_data:get(chat_level), level_not_satisfy}, {RoleId, ne, ReceiverId, cannot_send_to_self}]) of
        ok ->
            private_notify(User, ReceiverId, [ReceiverId, Type, Message]);
        {error, Error} ->
            {error, [Error, #private_chat{}]}
    end.

%% @doc private notify
-spec private_notify(User :: #user{}, ReceiverId :: non_neg_integer(), Args :: [term()]) -> ok.
private_notify(User, ReceiverId, Args) ->
    PrivateChat = user_convert:to_private_chat(User, Args),
    chat_server:chat_private(PrivateChat),
    user_sender:send(ReceiverId, ?PROTOCOL_CHAT_PRIVATE, [ok, PrivateChat]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
