-module(role_notice_sql).
-export([save/1]).
-export([select/1]).
-export([update_read/1]).
-include("notice.hrl").

%% @doc insert into role_notice
-spec save(RoleNoticeList :: [#role_notice{}] | ets:tab()) -> NewRoleNoticeList :: [#role_notice{}].
save(RoleNoticeList) ->
    db:save(<<"INSERT INTO `role_notice` (`role_id`, `notice_id`, `read_time`) VALUES">>, <<"(?, ?, ?)">>, <<"">>, RoleNoticeList, fun(#role_notice{role_id = RoleId, notice_id = NoticeId, read_time = ReadTime}) -> [RoleId, NoticeId, ReadTime] end, #role_notice.flag).

%% @doc select from role_notice
-spec select(RoleNoticeRoleId :: non_neg_integer()) -> Rows :: [#role_notice{}].
select(RoleNoticeRoleId) ->
    Data = db:select(<<"SELECT `role_notice`.`role_id`, `role_notice`.`notice_id`, `notice`.`receive_time`, `notice`.`expire_time`, `role_notice`.`read_time` FROM `role_notice` INNER JOIN `notice` ON `notice`.`notice_id` = `role_notice`.`notice_id` WHERE `role_notice`.`role_id` = ?">>, [RoleNoticeRoleId]),
    parser:convert(Data, role_notice).

%% @doc update into role_notice
-spec update_read(#role_notice{}) -> AffectedRows :: non_neg_integer().
update_read(#role_notice{read_time = ReadTime, role_id = RoleId, notice_id = NoticeId}) ->
    db:update(<<"UPDATE `role_notice` SET `read_time` = ? WHERE `role_id` = ? AND `notice_id` = ?">>, [ReadTime, RoleId, NoticeId]).
