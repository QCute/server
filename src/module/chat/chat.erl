%%%-------------------------------------------------------------------
%%% @doc
%%% module chat
%%% @end
%%%-------------------------------------------------------------------
-module(chat).
%% API
-export([world/2, guild/3, private/3]).
%% Includes
-include("common.hrl").
-include("user.hrl").
-include("role.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 世界
-spec world(User :: #user{}, Msg :: binary()) -> ok | error().
world(User = #user{role_id = RoleId, role_name = RoleName}, Msg) ->
    case user_checker:check(User, [{level, parameter_data:get(chat_level), 2}, {chat_cd, ge, 30, 3}]) of
        ok ->
            {ok, Data} = user_router:write(?PROTOCOL_CHAT_WORLD, [RoleId, RoleName, Msg]),
            user_manager:broadcast(Data),
            ok;
        Error ->
            Error
    end.

%% @doc 公会
-spec guild(User :: #user{}, GuildId :: non_neg_integer(), Msg :: binary()) -> ok | error().
guild(User = #user{role_id = RoleId, role_name = RoleName}, GuildId, Msg) ->
    case user_checker:check(User, [{level, parameter_data:get(chat_level), 2}, {GuildId, ne, 0, 3}, {chat_cd, ge, 30, 4}]) of
        ok ->
            {ok, Data} = user_router:write(?PROTOCOL_CHAT_GUILD, [RoleId, RoleName, Msg]),
            guild:broadcast(GuildId, Data),
            ok;
        Error ->
            Error
    end.

%% @doc 私聊
-spec private(User :: #user{}, ReceiverId :: non_neg_integer(), Msg :: binary()) -> ok | error().
private(User = #user{role_id = RoleId, role_name = RoleName}, ReceiverId, Msg) ->
    case user_checker:check(User, [{level, parameter_data:get(chat_level), 2}]) of
        ok when RoleId =/= ReceiverId ->
            case process:sender_pid(ReceiverId) of
                Pid when is_pid(Pid) ->
                    user_sender:send(Pid, ?PROTOCOL_CHAT_PRIVATE, [RoleId, RoleName, Msg]),
                    ok;
                _ ->
                    {error, 3}
            end;
        Error ->
            Error
    end.
%%%===================================================================
%%% Internal functions
%%%===================================================================