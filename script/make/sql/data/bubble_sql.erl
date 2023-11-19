-module(bubble_sql).
-export([save/1]).
-export([select/1]).
-export([delete/2]).
-include("bubble.hrl").

%% @doc insert into bubble
-spec save(BubbleList :: [#bubble{}] | ets:tab()) -> NewBubbleList :: [#bubble{}].
save(BubbleList) ->
    db:save_into(<<"INSERT INTO `bubble` (`role_id`, `bubble_id`, `type`, `expire_time`) VALUES">>, <<"(:2:, :3:, :4:, :5:)">>, <<"ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `bubble_id` = VALUES(`bubble_id`), `type` = VALUES(`type`), `expire_time` = VALUES(`expire_time`)">>, BubbleList, #bubble.flag).

%% @doc select from bubble
-spec select(RoleId :: non_neg_integer()) -> Rows :: [#bubble{}].
select(RoleId) ->
    Data = db:select(<<"SELECT `role_id`, `bubble_id`, `type`, `expire_time`, `flag` FROM `bubble` WHERE `role_id` = ?">>, [RoleId]),
    parser:convert(Data, bubble).

%% @doc delete row from bubble
-spec delete(RoleId :: non_neg_integer(), BubbleId :: non_neg_integer()) -> AffectedRows :: non_neg_integer().
delete(RoleId, BubbleId) ->
    db:delete(<<"DELETE FROM `bubble` WHERE `role_id` = ? AND `bubble_id` = ?">>, [RoleId, BubbleId]).
