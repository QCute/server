%%%------------------------------------------------------------------
%%% @doc
%%% module chat
%%% @end
%%%------------------------------------------------------------------
-module(chat).
%% API
-export([world/2, guild/2, private/3]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("protocol.hrl").

%%%==================================================================
%%% API functions
%%%==================================================================
%% @doc 世界
-spec world(User :: #user{}, Msg :: binary()) -> ok() | error().
world(User = #user{role_id = RoleId, role_name = RoleName}, Msg) ->
    case user_checker:check(User, [{level, parameter_data:get(chat_level), 2}, {parameter_data:get(chat_cd), ge, 30, 3}]) of
        {ok, _} ->
            {ok, ChatBinary} = user_router:write(?PROTOCOL_CHAT_WORLD, [RoleId, RoleName, Msg]),
            user_manager:broadcast(ChatBinary),
            ok;
        Error ->
            Error
    end.

%% @doc 公会
-spec guild(User :: #user{}, Msg :: binary()) -> ok() | error().
guild(User = #user{role_id = RoleId, role_name = RoleName}, Msg) ->
    GuildId = guild:role_guild_id(RoleId),
    case user_checker:check(User, [{level, parameter_data:get(chat_level), 2}, {GuildId, ne, 0, 3}, {parameter_data:get(chat_cd), ge, 30, 4}]) of
        {ok, _} ->
            {ok, ChatBinary} = user_router:write(?PROTOCOL_CHAT_GUILD, [RoleId, RoleName, Msg]),
            guild:broadcast(GuildId, ChatBinary),
            ok;
        Error ->
            Error
    end.

%% @doc 私聊
-spec private(User :: #user{}, ReceiverId :: non_neg_integer(), Msg :: binary()) -> ok() | error().
private(User = #user{role_id = RoleId, role_name = RoleName}, ReceiverId, Msg) ->
    case user_checker:check(User, [{level, parameter_data:get(chat_level), 2}]) of
        {ok, _} when RoleId =/= ReceiverId ->
            case user_sender:pid(ReceiverId) of
                Pid when is_pid(Pid) ->
                    user_sender:send(Pid, ?PROTOCOL_CHAT_PRIVATE, [RoleId, RoleName, Msg]),
                    ok;
                _ ->
                    {error, 3}
            end;
        Error ->
            Error
    end.
%%%==================================================================
%%% Internal functions
%%%==================================================================