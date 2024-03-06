-module(notice_role_sql).
-export([save/1]).
-export([select/1]).
-export([update_read/1]).
-include("notice.hrl").

%% @doc insert into notice_role
-spec save(NoticeRoleList :: [#notice_role{}] | ets:tab()) -> NewNoticeRoleList :: [#notice_role{}].
save(NoticeRoleList) ->
    db:save_into(<<"INSERT INTO `notice_role` (`role_id`, `notice_id`, `read_time`) VALUES">>, <<"(:2:, :3:, :6:)">>, <<"ON DUPLICATE KEY UPDATE `role_id` = VALUES(`role_id`), `notice_id` = VALUES(`notice_id`), `read_time` = VALUES(`read_time`)">>, NoticeRoleList, #notice_role.flag).

%% @doc select from notice_role
-spec select(NoticeRoleRoleId :: non_neg_integer()) -> Rows :: [#notice_role{}].
select(NoticeRoleRoleId) ->
    Data = db:select(<<"SELECT `notice_role`.`role_id`, `notice_role`.`notice_id`, `notice`.`receive_time`, `notice`.`expire_time`, `notice_role`.`read_time`, `notice_role`.`title`, `notice_role`.`content`, `notice_role`.`flag` FROM `notice_role` INNER JOIN `notice` ON `notice`.`notice_id` = `notice_role`.`notice_id` WHERE `notice_role`.`role_id` = ?">>, [NoticeRoleRoleId]),
    parser:convert(Data, notice_role).

%% @doc update into notice_role
-spec update_read(NoticeRole :: #notice_role{}) -> AffectedRows :: non_neg_integer().
update_read(NoticeRole) ->
    db:update(<<"UPDATE `notice_role` SET `read_time` = :6: WHERE `role_id` = :1: AND `notice_id` = :2:">>, NoticeRole).
