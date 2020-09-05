%%%-------------------------------------------------------------------
%%% @doc
%%% chat
%%% @end
%%%-------------------------------------------------------------------
-module(chat).
%% API
-export([world/2, guild/2, private/3]).
%% Includes
-include("common.hrl").
-include("protocol.hrl").
-include("user.hrl").
%%%===================================================================
%%% API functions
%%%===================================================================
%% @doc world
-spec world(User :: #user{}, Msg :: binary()) -> ok() | error().
world(User = #user{role_id = RoleId, role_name = RoleName, world_chat_time = WorldChatTime}, Msg) ->
    Now = time:now(),
    case user_checker:check(User, [{level, parameter_data:get(chat_level), level_not_enough}, {Now - WorldChatTime, ge, parameter_data:get(chat_cd), time_in_cd}]) of
        ok ->
            {ok, ChatBinary} = user_router:write(?PROTOCOL_CHAT_WORLD, [ok, RoleId, RoleName, Msg]),
            user_manager:broadcast(ChatBinary),
            {ok, User#user{world_chat_time = Now}};
        {error, Error} ->
            {error, [Error, 0, <<>>, <<>>]}
    end.

%% @doc guild
-spec guild(User :: #user{}, Msg :: binary()) -> ok() | error().
guild(User = #user{role_id = RoleId, role_name = RoleName, guild_chat_time = GuildChatTime}, Msg) ->
    Now = time:now(),
    GuildId = guild:role_guild_id(RoleId),
    case user_checker:check(User, [{level, parameter_data:get(chat_level), level_not_enough}, {GuildId, ne, 0, no_guild}, {Now - GuildChatTime, ge, parameter_data:get(chat_cd), time_in_cd}]) of
        ok ->
            {ok, ChatBinary} = user_router:write(?PROTOCOL_CHAT_GUILD, [ok, RoleId, RoleName, Msg]),
            guild:broadcast(GuildId, ChatBinary),
            {ok, User#user{guild_chat_time = Now}};
        {error, Error} ->
            {error, [Error, 0, <<>>, <<>>]}
    end.

%% @doc private
-spec private(User :: #user{}, ReceiverId :: non_neg_integer(), Msg :: binary()) -> ok() | error().
private(User = #user{role_id = RoleId, role_name = RoleName}, ReceiverId, Msg) ->
    case user_checker:check(User, [{level, parameter_data:get(chat_level), level_not_enough}]) of
        ok when RoleId =/= ReceiverId ->
            case user_sender:pid(ReceiverId) of
                Pid when is_pid(Pid) ->
                    user_sender:send(Pid, ?PROTOCOL_CHAT_PRIVATE, [ok, RoleId, RoleName, Msg]),
                    ok;
                _ ->
                    {error, [user_offline, 0, <<>>, <<>>]}
            end;
        {error, Error} ->
            {error, [Error, 0, <<>>, <<>>]}
    end.
%%%===================================================================
%%% Internal functions
%%%===================================================================