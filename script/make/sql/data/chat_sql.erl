-module(chat_sql).
-export([insert/1]).
-export([select/1]).
-export([update/1]).
-include("chat.hrl").

%% @doc insert into chat
-spec insert(Chat :: #chat{}) -> InsertIdOrAffectedRows :: non_neg_integer().
insert(Chat) ->
    db:insert(<<"INSERT INTO `chat` (`role_id`, `world_chat_time`, `guild_chat_time`, `scene_chat_time`, `private_chat_time`) VALUES (:2:, :3:, :4:, :5:, :6:)">>, Chat).

%% @doc select from chat
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#chat{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `world_chat_time`, `guild_chat_time`, `scene_chat_time`, `private_chat_time` FROM `chat` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, chat).

%% @doc update into chat
-spec update(Chat :: #chat{}) -> AffectedRows :: non_neg_integer().
update(Chat) ->
    db:update(<<"UPDATE `chat` SET `role_id` = :2:, `world_chat_time` = :3:, `guild_chat_time` = :4:, `scene_chat_time` = :5:, `private_chat_time` = :6: WHERE `role_id` = :1:">>, Chat).
