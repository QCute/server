%%%-------------------------------------------------------------------
%%% @doc
%%% module chat
%%% @end
%%%-------------------------------------------------------------------
-module(chat).
-export([world/2, guild/3, private/5]).
-include("common.hrl").
-include("player.hrl").
-include("protocol.hrl").

%%%===================================================================
%%% API
%%%===================================================================
%% @doc 世界
-spec world(User :: #user{}, Msg :: binary()) -> ok | error().
world(User, Msg) ->
	case player_condition:check(User, [{level, data_parameter:get(chat_level), 2}, {chat_cd, 30, 3}]) of
		true ->
			{ok, Data} = player_route:write(?CMD_CHAT_WORLD, [Msg]),
			player_manager:broadcast(Data),
			ok;
		Error ->
			Error
	end.

%% @doc 公会
-spec guild(User :: #user{}, GuildId :: non_neg_integer(), Msg :: binary()) -> ok | error().
guild(User, GuildId, Msg) ->
	case player_condition:check(User, [{level, data_parameter:get(chat_level), 2}, {chat_cd, 30, 3}]) of
		true ->
			{ok, Data} = player_route:write(?CMD_CHAT_GUILD, [Msg]),
			guild_server:broadcast(GuildId, Data),
			ok;
		Error ->
			Error
	end.

%% @doc 私聊
-spec private(User :: #user{}, Channel :: non_neg_integer(), ReceiverId :: non_neg_integer(), ReceiverName :: binary(), Msg :: binary()) -> ok | error().
private(User = #user{id = Id}, Channel, ReceiverId, _ReceiverName, Msg) ->
	case player_condition:check(User, [{level, data_parameter:get(chat_level), 2}]) of
		true when Id =/= ReceiverId ->
			case process:sender_pid(ReceiverId) of
				Pid when is_pid(Pid) ->
					player_sender:send(Pid, ?CMD_CHAT_PRIVATE, [User, Channel, Msg]),
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