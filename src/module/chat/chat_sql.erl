-module(chat_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("chat.hrl").

%% @doc insert into chat
-spec insert(Chat :: #chat{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(#chat{role_id = RoleId, world_chat_time = WorldChatTime, guild_chat_time = GuildChatTime, scene_chat_time = SceneChatTime, private_chat_time = PrivateChatTime}) ->
    db:insert(<<"INSERT INTO `chat` (`role_id`, `world_chat_time`, `guild_chat_time`, `scene_chat_time`, `private_chat_time`) VALUES (?, ?, ?, ?, ?)">>, [RoleId, WorldChatTime, GuildChatTime, SceneChatTime, PrivateChatTime]).

%% @doc select from chat
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#chat{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `world_chat_time`, `guild_chat_time`, `scene_chat_time`, `private_chat_time` FROM `chat` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, chat).

%% @doc update into chat
-spec update(#chat{}) -> AffectedRows :: non_neg_integer().
update(#chat{role_id = RoleId, world_chat_time = WorldChatTime, guild_chat_time = GuildChatTime, scene_chat_time = SceneChatTime, private_chat_time = PrivateChatTime}) ->
    db:update(<<"UPDATE `chat` SET `role_id` = ?, `world_chat_time` = ?, `guild_chat_time` = ?, `scene_chat_time` = ?, `private_chat_time` = ? WHERE `role_id` = ?">>, [RoleId, WorldChatTime, GuildChatTime, SceneChatTime, PrivateChatTime, RoleId]).
