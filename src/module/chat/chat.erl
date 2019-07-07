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
world(User = #user{id = UserId, name = Name}, Msg) ->
    case role_checker:check(User, [{level, parameter_data:get(chat_level), 2}, {chat_cd, ge, 30, 3}]) of
        ok ->
            {ok, Data} = role_router:write(?CMD_CHAT_WORLD, [UserId, Name, Msg]),
            role_manager:broadcast(Data),
            ok;
        Error ->
            Error
    end.

%% @doc 公会
-spec guild(User :: #user{}, GuildId :: non_neg_integer(), Msg :: binary()) -> ok | error().
guild(User = #user{id = UserId, name = Name}, GuildId, Msg) ->
    case role_checker:check(User, [{level, parameter_data:get(chat_level), 2}, {GuildId, ne, 0, 3}, {chat_cd, ge, 30, 4}]) of
        ok ->
            {ok, Data} = role_router:write(?CMD_CHAT_GUILD, [UserId, Name, Msg]),
            guild:broadcast(GuildId, Data),
            ok;
        Error ->
            Error
    end.

%% @doc 私聊
-spec private(User :: #user{}, ReceiverId :: non_neg_integer(), Msg :: binary()) -> ok | error().
private(User = #user{id = UserId, name = Name}, ReceiverId, Msg) ->
    case role_checker:check(User, [{level, parameter_data:get(chat_level), 2}]) of
        ok when UserId =/= ReceiverId ->
            case process:sender_pid(ReceiverId) of
                Pid when is_pid(Pid) ->
                    role_sender:send(Pid, ?CMD_CHAT_PRIVATE, [UserId, Name, Msg]),
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